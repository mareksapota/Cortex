module Cortex.Saffron.Apps.Static
    ( run
    ) where

-----

import Control.Concurrent.Lifted
import Control.Monad.State
import Control.Monad.Error (catchError)
import System.IO (stderr)
import System.Process
    ( runCommand
    , waitForProcess
    , terminateProcess
    , getProcessExitCode
    )
import Data.Maybe (isNothing)

import Cortex.Saffron.GrandMonadStack
import qualified Cortex.Saffron.Config as Config
import Cortex.Common.ErrorIO
import Cortex.Common.Error

-----

run :: Int -> String -> AppManagerMonadStack (MVar (), MVar Int)
run port location = do
    (_, _, app, _) <- get
    stop <- newEmptyMVar
    finished <- newEmptyMVar
    -- TODO: Put this in prepare, no need to do this more than once.
    ignoreError $ iRawSystem "cp" ["Saffron/Apps/static.ru", location]
    fork $ do
        { let cmd = concat
                [ "thin -R '../static.ru' -c '"
                , location
                , "/repo' -p "
                , show port
                , " start &> /dev/null"
                ]
        ; proc <- liftIO $ runCommand cmd

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
                { iPutStrLn stderr $ concat
                    [  app
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
        ; iPutStrLn stderr $ app ++ " instance stopped on port " ++ (show port)
        }
    return (stop, finished)

-----

amIAlive :: Int -> AppManagerMonadStack Bool
amIAlive port = do
    { (h, p, app, _) <- get
    ; let key = concat
            [ "app::instance::"
            , app
            , "::"
            , Config.host
            , ":"
            , show port
            ]
    ; hdl <- iConnectTo h p
    ; iPutStrLn hdl "lookup"
    ; iPutStrLn hdl key
    ; iFlush hdl
    ; value <- iGetLine hdl
    ; iClose hdl
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
