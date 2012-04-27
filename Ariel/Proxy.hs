{-# LANGUAGE OverloadedStrings #-}

module Cortex.Ariel.Proxy
    ( runProxy
    ) where

-----

import Control.Monad.State (evalStateT, get, put)
import Control.Monad.Error (catchError, throwError)
import Control.Monad (when, liftM)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Char8 as BS
import Data.Map (Map)
import qualified Data.Map as Map
import System.Process (runCommand)
import Control.Monad.Trans (liftIO)

import qualified Cortex.Common.Sha1 as Sha1
import Cortex.Common.ErrorIO
    ( iOpenTempFile
    , iConnectTo
    , iReadFile
    , iWriteFile
    , iPrintLog
    , iRawSystem
    )
import Cortex.Common.LazyIO
import Cortex.Common.Error
import Cortex.Common.Event
import Cortex.Ariel.GrandMonadStack
import qualified Cortex.Ariel.Config as Config

-----

runProxy :: String -> Int -> LesserMonadStack ()
runProxy host port = do
    -- TODO: This file is never cleaned up when Ariel is killed by a signal.
    (path, hdl) <- iOpenTempFile "/tmp" "nginx.conf"
    lClose hdl
    evalStateT runProxy' (host, port, path, "")

-----

runProxy' :: GrandMonadStack ()
runProxy' = do
    (_, _, path, _) <- get
    emptyConf <- makeConf Map.empty
    iWriteFile path emptyConf
    liftIO $ runCommand ("nginx -c " ++ path)
    periodicTimer Config.proxyUpdateTime updateProxy
    eventLoop

-----

updateProxy :: GrandMonadStack ()
updateProxy = do
    { (host, port, path, oldHash) <- get
    ; hdl <- iConnectTo host port
    ; lPutStrLn hdl "lookup all"
    ; lPutStrLn hdl "app::instance"
    ; lFlush hdl
    ; instances <- lGetContents hdl
    ; lClose hdl
    ; hdl' <- iConnectTo host port
    ; lPutStrLn hdl' "lookup all with value"
    ; lPutStrLn hdl' "app::port"
    ; lFlush hdl'
    ; ports <- lGetContents hdl'
    ; lClose hdl'
    ; let hash1 = Sha1.lhash instances
    ; let hash2 = Sha1.lhash ports
    ; let hash = Sha1.hash (BS.concat [hash1, hash2])
    ; when (hash /= oldHash) $ reload (LBS.lines instances)
    ; put (host, port, path, hash)
    } `catchError` reportError

-----

reload :: [LBS.ByteString] -> GrandMonadStack ()
reload instances = do
    { iPrintLog "Instance change detected, reloading"
    ; let apps = makeApps instances Map.empty
    ; conf <- makeConf apps
    ; (_, _, path, _) <- get
    ; iWriteFile path conf
    ; pid <- (liftM $ LBS.takeWhile (/= '\n')) $ iReadFile (path ++ ".pid")
    ; iRawSystem "kill" ["-HUP", LBS.unpack pid]
    }

-----

makeApps :: [LBS.ByteString] -> Map LBS.ByteString [LBS.ByteString] ->
    Map LBS.ByteString [LBS.ByteString]
makeApps [] m = m
makeApps (h:t) m = do
    let app = LBS.takeWhile (':' /=) h
    let location = LBS.drop 2 $ LBS.dropWhile (':' /=) h
    let l = Map.findWithDefault [] app m
    makeApps t (Map.insert app (location:l) m)

-----

makeConf :: Map LBS.ByteString [LBS.ByteString] -> GrandMonadStack LBS.ByteString
makeConf apps = do
    (_, _, path, _) <- get
    configs <- mapM (makeAppConf apps) (Map.keys apps)
    return $ LBS.unlines
        [ "error_log /dev/null;"
        , LBS.concat ["pid ", LBS.pack path, ".pid;"]
        , "daemon off;"
        , "worker_processes 1;"
        , "events {"
        , "    worker_connections 1024;"
        , "}"
        , "http {"
        , "    include /etc/nginx/mime.types;"
        , LBS.unlines configs
        , "}"
        ]
    where
        makeAppConf :: Map LBS.ByteString [LBS.ByteString] ->
            LBS.ByteString -> GrandMonadStack LBS.ByteString
        makeAppConf m app = do
            { (host, port, _, _) <- get
            ; hdl <- iConnectTo host port
            ; lPutStrLn hdl "lookup"
            ; lPutStrLn hdl $ LBS.concat ["app::port::", app]
            ; lFlush hdl
            ; p <- lGetLine hdl
            ; lClose hdl
            ; when (p == "Nothing") $ throwError "No port is set"
            ; let instances = m Map.! app
            ; let servers = map (\s -> LBS.concat ["server ", s, ";"]) instances
            ; return $ LBS.unlines
                [ LBS.concat ["upstream ", app, " {"]
                , LBS.unlines servers
                , "}"
                , "server {"
                , "    access_log /dev/null;"
                -- Drop the "Just ".
                , LBS.concat ["    listen ", LBS.drop 5 p, ";"]
                , "    location / {"
                , LBS.concat ["        proxy_pass http://", app, ";"]
                , "    }"
                , "}"
                ]
            } `catchError` (\_ -> return "")

-----
