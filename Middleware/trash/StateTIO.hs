module Adria.Middleware.StateTIO
    ( iSetNewlineMode
    , iSetBuffering
    , iGetLine
    , iClose
    , iFlush
    , iPutStrLn
    ) where

import Control.Monad.Trans (liftIO)
import Control.Monad.Error (throwError)
import IO (try)
import System.IO

import Adria.Middleware.Types

-- System.IO helpers for StateT IO monad.

ioReport :: IO a -> ESI b a
ioReport f = do
    t <- liftIO $ try f
    case t of
        Left e -> throwError $ show e
        Right a -> return a

iSetNewlineMode :: Handle -> NewlineMode -> ESI a ()
iSetNewlineMode a b = ioReport $ hSetNewlineMode a b

iSetBuffering :: Handle -> BufferMode -> ESI a ()
iSetBuffering a b = ioReport $ hSetBuffering a b

iGetLine :: Handle -> ESI a String
iGetLine a = ioReport $ hGetLine a

iClose :: Handle -> ESI a ()
iClose a = ioReport $ hClose a

iFlush :: Handle -> ESI a ()
iFlush a = ioReport $ hFlush a

iPutStrLn :: Handle -> String -> ESI a ()
iPutStrLn a b = ioReport $ hPutStrLn a b
