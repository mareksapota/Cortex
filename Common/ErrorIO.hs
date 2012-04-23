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
    , iRunProcess
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
import System.Process (rawSystem, readProcess, runProcess, ProcessHandle)
import System.Exit (ExitCode (ExitSuccess, ExitFailure))
import Control.Concurrent (yield)

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
iSetBuffering hdl mode = ioReport $ hSetBuffering hdl mode

-----

iGetLine :: (MonadError String m, MonadIO m) => Handle -> m String
iGetLine hdl = ioReport $ iGetLine' hdl []

iGetLine' :: Handle -> String -> IO String
iGetLine' hdl s = do
    c <- hGetChar hdl
    if c == '\n'
        then return $ reverse s
        else iGetLine' hdl (c:s)

-----

iGetChar :: (MonadError String m, MonadIO m) => Handle -> m Char
iGetChar hdl = ioReport $ hGetChar hdl

-----

ibGetContents :: (MonadError String m, MonadIO m) => Handle -> m ByteString
ibGetContents hdl = ioReport $ BS.hGetContents hdl

-----

iOpen :: (MonadError String m, MonadIO m) => FilePath -> IOMode -> m Handle
iOpen path mode = ioReport $ openFile path mode

-----

iPersistentOpen :: (MonadError String m, MonadIO m) => FilePath -> IOMode -> m Handle
iPersistentOpen path mode = do
    t <- liftIO $ try $ openFile path mode
    handle t
    where
        handle (Right v) = return v
        handle (Left e)
            | isAlreadyInUseError e = do
                -- Use active waiting only if no other threads are available to
                -- run.
                liftIO yield
                iPersistentOpen path mode
            | otherwise = throwError $ show e

-----

iClose :: (MonadError String m, MonadIO m) => Handle -> m ()
iClose hdl = ioReport $ hClose hdl

-----

iFlush :: (MonadError String m, MonadIO m) => Handle -> m ()
iFlush hdl = ioReport $ hFlush hdl

-----

iPutStrLn :: (MonadError String m, MonadIO m) => Handle -> String -> m ()
iPutStrLn hdl s = ioReport $ do
    hPutStr hdl s
    hPutChar hdl '\n'

-----

ibPutStrLn :: (MonadError String m, MonadIO m) => Handle -> ByteString -> m ()
ibPutStrLn hdl s = ioReport $ do
    BS.hPut hdl s
    BS.hPut hdl $ BS.pack "\n"

-----

iPutStr :: (MonadError String m, MonadIO m) => Handle -> String -> m ()
iPutStr hdl s = ioReport $ hPutStr hdl s

-----

ibPutStr :: (MonadError String m, MonadIO m) => Handle -> ByteString -> m ()
ibPutStr hdl s = ioReport $ BS.hPut hdl s

-----

iListenOn :: (MonadError String m, MonadIO m) => Int -> m Socket
iListenOn port = ioReport $ listenOn $ PortNumber $ fromIntegral port

-----

iAccept :: (MonadError String m, MonadIO m) =>
    Socket -> m (Handle, HostName, PortNumber)
iAccept sock = ioReport $ accept sock

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
iRawSystem cmd args = do
    e <- liftIO $ rawSystem cmd args
    iRawSystem' e

iRawSystem' :: (MonadError String m, MonadIO m) => ExitCode -> m ()
iRawSystem' ExitSuccess = return ()
iRawSystem' (ExitFailure a) = throwError $ "ExitFailure " ++ (show a)

-----

iReadProcess :: (MonadError String m, MonadIO m) =>
    String -> [String] -> m String
iReadProcess cmd args = ioReport $ readProcess cmd args ""

-----

iRunProcess :: (MonadError String m, MonadIO m) => String -> [String] ->
    Maybe String -> Maybe [(String, String)] -> m ProcessHandle
iRunProcess cmd args location env = ioReport $ do
    input <- openFile "/dev/null" ReadMode
    output <- openFile "/dev/null" WriteMode
    runProcess cmd args location env (Just input) (Just output) Nothing

-----

iOpenTempFile :: (MonadError String m, MonadIO m) => String -> String ->
    m (String, Handle)
iOpenTempFile dir name = ioReport $ openTempFile dir name

-----

iWriteFile :: (MonadError String m, MonadIO m) => String -> String -> m ()
iWriteFile path str = ioReport $ do
    hdl <- openFile path WriteMode
    hPutStr hdl str
    hClose hdl

-----

iReadFile :: (MonadError String m, MonadIO m) => String -> m String
iReadFile path = ioReport $ do
    hdl <- openFile path ReadMode
    str <- BS.hGetContents hdl
    return $ BS.unpack str

-----
