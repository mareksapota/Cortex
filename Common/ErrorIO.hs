{-# LANGUAGE FlexibleContexts #-}

module Cortex.Common.ErrorIO
    ( iSetNewlineMode
    , iSetBuffering
    , iGetLine
    , ibGetContents
    , iOpen
    , iClose
    , iFlush
    , iPutStrLn
    , ibPutStrLn
    , iPutStr
    , ibPutStr
    , iListenOn
    , iAccept
    , iConnectTo
    ) where

import Control.Monad.Trans (liftIO, MonadIO)
import Control.Monad.Error (MonadError, throwError)
import Control.Exception (try, IOException)
import System.IO
import Network
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS

-- IO helpers that report errors through Error monad.

-----

ioReport :: (MonadError String m, MonadIO m) => IO a -> m a
ioReport f = do
    t <- liftIO $ try f
    case t of
        Left e -> throwError $ show (e :: IOException)
        Right a -> return a

-----

iSetNewlineMode :: (MonadError String m, MonadIO m) =>
    Handle -> NewlineMode -> m ()
iSetNewlineMode a b = ioReport $ hSetNewlineMode a b

-----

iSetBuffering :: (MonadError String m, MonadIO m) =>
    Handle -> BufferMode -> m ()
iSetBuffering a b = ioReport $ hSetBuffering a b

-----

iGetLine :: (MonadError String m, MonadIO m) => Handle -> m String
iGetLine a = ioReport $ hGetLine a

-----

ibGetContents :: (MonadError String m, MonadIO m) => Handle -> m ByteString
ibGetContents a = ioReport $ BS.hGetContents a

-----

iOpen :: (MonadError String m, MonadIO m) => FilePath -> IOMode -> m Handle
iOpen a b = ioReport $ openFile a b

-----

iClose :: (MonadError String m, MonadIO m) => Handle -> m ()
iClose a = ioReport $ hClose a

-----

iFlush :: (MonadError String m, MonadIO m) => Handle -> m ()
iFlush a = ioReport $ hFlush a

-----

iPutStrLn :: (MonadError String m, MonadIO m) => Handle -> String -> m ()
iPutStrLn a b = ioReport $ hPutStrLn a b

-----

ibPutStrLn :: (MonadError String m, MonadIO m) => Handle -> ByteString -> m ()
ibPutStrLn a b = do
    ibPutStr a b
    ioReport $ BS.hPut a $ BS.pack "\n"

-----

iPutStr :: (MonadError String m, MonadIO m) => Handle -> String -> m ()
iPutStr a b = ioReport $ hPutStr a b

-----

ibPutStr :: (MonadError String m, MonadIO m) => Handle -> ByteString -> m ()
ibPutStr a b = ioReport $ BS.hPut a b

-----

iListenOn :: (MonadError String m, MonadIO m) => PortID -> m Socket
iListenOn a = ioReport $ listenOn a

-----

iAccept :: (MonadError String m, MonadIO m) =>
    Socket -> m (Handle, HostName, PortNumber)
iAccept a = ioReport $ accept a

-----

iConnectTo :: (MonadError String m, MonadIO m) => String -> Int -> m Handle
iConnectTo host port =
    ioReport $ connectTo host $ PortNumber $ fromIntegral port
