{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}

module Cortex.Common.ErrorIO
    ( iSetBuffering
    , iGetLine
    , iGetChar
    , ibGetContents
    , iOpen
    , iPersistentOpen
    , iClose
    , iFlush
    , iPutStrLn
    , ibPutStrLn
    , iPutStr
    , ibPutStr
    , iListenOn
    , iAccept
    , iConnectTo
    , iEncode
    , iDecode
    , iRawSystem
    , iReadProcess
    , iOpenTempFile
    , iWriteFile
    , iReadFile
    ) where

import Control.Monad.Trans (liftIO, MonadIO)
import Control.Monad.Error (MonadError, throwError)
import Control.Exception (try, IOException, SomeException)
import System.IO
import System.IO.Error (isAlreadyInUseError)
import Network
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Binary (encode, decode, Binary)
import System.Process (rawSystem, readProcess)
import System.Exit (ExitCode (ExitSuccess, ExitFailure))

-- IO helpers that report errors through Error monad.

-----

ioReport :: (MonadError String m, MonadIO m) => IO a -> m a
ioReport f = do
    t <- liftIO $ try f
    case t of
        Left e -> throwError $ show (e :: IOException)
        Right a -> return a

-----

iSetBuffering :: (MonadError String m, MonadIO m) =>
    Handle -> BufferMode -> m ()
iSetBuffering a b = ioReport $ hSetBuffering a b

-----

iGetLine :: (MonadError String m, MonadIO m) => Handle -> m String
iGetLine a = iGetLine' a []

iGetLine' :: (MonadError String m, MonadIO m) => Handle -> String -> m String
iGetLine' hdl s = do
    c <- iGetChar hdl
    if c == '\n'
        then return $ reverse s
        else iGetLine' hdl (c:s)

-----

iGetChar :: (MonadError String m, MonadIO m) => Handle -> m Char
iGetChar a = ioReport $ hGetChar a

-----

ibGetContents :: (MonadError String m, MonadIO m) => Handle -> m ByteString
ibGetContents a = ioReport $ BS.hGetContents a

-----

iOpen :: (MonadError String m, MonadIO m) => FilePath -> IOMode -> m Handle
iOpen a b = ioReport $ openFile a b

-----

iPersistentOpen :: (MonadError String m, MonadIO m) => FilePath -> IOMode -> m Handle
iPersistentOpen a b = do
    t <- liftIO $ try $ openFile a b
    handle t
    where
        handle (Right v) = return v
        handle (Left e)
            | isAlreadyInUseError e = iPersistentOpen a b
            | otherwise = throwError $ show e

-----

iClose :: (MonadError String m, MonadIO m) => Handle -> m ()
iClose a = ioReport $ hClose a

-----

iFlush :: (MonadError String m, MonadIO m) => Handle -> m ()
iFlush a = ioReport $ hFlush a

-----

iPutStrLn :: (MonadError String m, MonadIO m) => Handle -> String -> m ()
iPutStrLn a b = do
    ioReport $ hPutStr a b
    ioReport $ hPutChar a '\n'

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

-----

iEncode :: (Monad m) => Binary a => a -> m ByteString
iEncode a = return $ encode a

-----

iDecode :: (Binary a, MonadError String m, MonadIO m) => ByteString -> m a
iDecode a = do
    -- Without strict evaluation this won't catch errors.
    t <- liftIO $ try (return $! decode a)
    case t of
        Left e -> throwError $ show (e :: SomeException)
        Right r -> return r

-----

iRawSystem :: (MonadError String m, MonadIO m) => String -> [String] -> m ()
iRawSystem a b = do
    e <-liftIO $ rawSystem a b
    iRawSystem' e

iRawSystem' :: (MonadError String m, MonadIO m) => ExitCode -> m ()
iRawSystem' ExitSuccess = return ()
iRawSystem' (ExitFailure a) = throwError $ "ExitFailure " ++ (show a)

-----

iReadProcess :: (MonadError String m, MonadIO m) =>
    String -> [String] -> m String
iReadProcess a b = ioReport $ readProcess a b ""

-----

iOpenTempFile :: (MonadError String m, MonadIO m) => String -> String ->
    m (String, Handle)
iOpenTempFile a b = ioReport $ openTempFile a b

-----

iWriteFile :: (MonadError String m, MonadIO m) => String -> String -> m ()
iWriteFile path str = do
    hdl <- iOpen path WriteMode
    iPutStr hdl str
    iClose hdl

-----

iReadFile :: (MonadError String m, MonadIO m) => String -> m String
iReadFile path = do
    hdl <- iOpen path ReadMode
    str <- ibGetContents hdl
    return $ BS.unpack str
