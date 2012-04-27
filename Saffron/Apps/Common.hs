{-# LANGUAGE OverloadedStrings #-}

module Cortex.Saffron.Apps.Common
    ( run
    ) where

-----

import Control.Concurrent.Lifted
import Control.Monad.State
import Control.Monad.Error (catchError)
import System.Process
    ( waitForProcess
    , terminateProcess
    , getProcessExitCode
    )
import Data.Maybe (isNothing)
import qualified Data.ByteString.Lazy.Char8 as LBS

import Cortex.Saffron.GrandMonadStack
import qualified Cortex.Saffron.Config as Config
import Cortex.Common.ErrorIO (iRunProcess, iPrintLog, iConnectTo)
import Cortex.Common.LazyIO

-----

run :: Int -> String -> [String] -> Maybe String -> Maybe [(String, String)] ->
    AppManagerMonadStack (MVar (), MVar Int)
run port cmd args location env = do
    (_, _, app, _) <- get
    stop <- newEmptyMVar
    finished <- newEmptyMVar
    fork $ do
        { proc <- iRunProcess cmd args location env

        ; fork $ do
            { takeMVar stop
            ; liftIO $ terminateProcess proc
            }

        -- Stop if Miranda thinks this instance doesn't exist.
        ; fork $ do
            { while (amIAlive port)
            ; e <- liftIO $ getProcessExitCode proc
            -- No need to kill this instance if it's already dead.
            ; when (isNothing e) $ do
                { iPrintLog $ concat
                    [  LBS.unpack app
                    , " instance on port "
                    , show port
                    , " accortding to Miranda doesn't exist, stopping"
                    ]
                ; tryPutMVar stop ()
                ; return ()
                }
            }

        ; liftIO $ waitForProcess proc
        ; putMVar finished port
        ; iPrintLog $ (LBS.unpack app) ++ " instance stopped on port " ++ (show port)
        }
    return (stop, finished)

-----

amIAlive :: Int -> AppManagerMonadStack Bool
amIAlive port = do
    { (h, p, app, _) <- get
    ; let key = LBS.concat
            [ "app::instance::"
            , app
            , "::"
            , LBS.pack Config.host
            , ":"
            , LBS.pack $ show port
            ]
    ; hdl <- iConnectTo h p
    ; lPutStrLn hdl "lookup"
    ; lPutStrLn hdl key
    ; lFlush hdl
    ; value <- lGetLine hdl
    ; lClose hdl
    ; return (value /= "Nothing")
    -- Ignore connection errors, assume everything is OK.
    } `catchError` (\_ -> return True)

-----

while :: AppManagerMonadStack Bool -> AppManagerMonadStack ()
while f = do
    let i = round $ (10 ** 6) * Config.instanceDeleteTime
    threadDelay i
    b <- f
    when b (while f)
