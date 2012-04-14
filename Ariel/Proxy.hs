module Cortex.Ariel.Proxy
    ( runProxy
    ) where

-----

import Control.Monad.State (evalStateT, get, put)
import Control.Monad.Error (catchError, throwError)
import Control.Monad (when, liftM)
import qualified Data.ByteString.Lazy.Char8 as BS
import System.IO (stderr)
import Data.Map (Map)
import qualified Data.Map as Map
import System.Process (runCommand)
import Control.Monad.Trans (liftIO)

import qualified Cortex.Common.Sha1 as Sha1
import Cortex.Common.ErrorIO
import Cortex.Common.Error
import Cortex.Common.Event
import Cortex.Ariel.GrandMonadStack
import qualified Cortex.Ariel.Config as Config

-----

runProxy :: String -> Int -> LesserMonadStack ()
runProxy host port = do
    -- TODO: This file is never cleaned up when Ariel is killed by a signal.
    (path, hdl) <- iOpenTempFile "/tmp" "nginx.conf"
    iClose hdl
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
    ; iPutStrLn hdl "lookup all"
    ; iPutStrLn hdl "app::instance"
    ; iFlush hdl
    ; instances <- ibGetContents hdl
    ; hdl' <- iConnectTo host port
    ; iPutStrLn hdl' "lookup all with value"
    ; iPutStrLn hdl' "app::port"
    ; iFlush hdl'
    ; ports <- ibGetContents hdl'
    ; let hash1 = Sha1.bhash instances
    ; let hash2 = Sha1.bhash ports
    ; let hash = Sha1.hash (hash1 ++ hash2)
    ; when (hash /= oldHash) $ reload (map BS.unpack $ BS.lines instances)
    ; put (host, port, path, hash)
    } `catchError` reportError

-----

reload :: [String] -> GrandMonadStack ()
reload instances = do
    { iPutStrLn stderr "Instance change detected, reloading"
    ; let apps = makeApps instances Map.empty
    ; conf <- makeConf apps
    ; (_, _, path, _) <- get
    ; iWriteFile path conf
    ; pid <- (liftM $ takeWhile (/= '\n')) $ iReadFile (path ++ ".pid")
    ; iRawSystem "kill" ["-HUP", pid]
    }

-----

makeApps :: [String] -> Map String [String] -> Map String [String]
makeApps [] m = m
makeApps (h:t) m = do
    let app = takeWhile (':' /=) h
    let location = drop 2 $ dropWhile (':' /=) h
    let l = Map.findWithDefault [] app m
    makeApps t (Map.insert app (location:l) m)

-----

makeConf :: Map String [String] -> GrandMonadStack String
makeConf apps = do
    (_, _, path, _) <- get
    configs <- mapM (makeAppConf apps) (Map.keys apps)
    return $ unlines
        [ "error_log /dev/null;"
        , "pid " ++ path ++ ".pid;"
        , "daemon off;"
        , "worker_processes 1;"
        , "events {"
        , "    worker_connections 1024;"
        , "}"
        , "http {"
        , "    include /etc/nginx/mime.types;"
        , unlines configs
        , "}"
        ]
    where
        makeAppConf :: Map String [String] -> String -> GrandMonadStack String
        makeAppConf m app = do
            { (host, port, _, _) <- get
            ; hdl <- iConnectTo host port
            ; iPutStrLn hdl "lookup"
            ; iPutStrLn hdl $ "app::port::" ++ app
            ; iFlush hdl
            ; p <- iGetLine hdl
            ; iClose hdl
            ; when (p == "Nothing") $ throwError "No port is set"
            ; let instances = m Map.! app
            ; let servers = map (\s -> concat ["server ", s, ";"]) instances
            ; return $ unlines
                [ "upstream " ++ app ++ " {"
                , unlines servers
                , "}"
                , "server {"
                , "    access_log /dev/null;"
                -- Drop the "Just ".
                , "    listen " ++ (drop 5 p) ++ ";"
                , "    location / {"
                , "        proxy_pass http://" ++ app ++ ";"
                , "    }"
                , "}"
                ]
            } `catchError` (\_ -> return "")

-----
