{-# LANGUAGE FlexibleContexts, Rank2Types #-}

module Cortex.Common.ErrorIO
    ( ioReport
    , iListenOn
    , iAccept
    , iConnectTo
    , iSetBuffering
    , iEncode
    , iDecode
    , iRawSystem
    , iReadProcess
    , iRunProcess
    , iPrintLog
    , iOpenTempFile
    , iWriteFile
    , iReadFile
    ) where

import Control.Monad.Trans (liftIO, MonadIO)
import Control.Monad.Error (MonadError, throwError)
import Control.Exception (try, SomeException)
import System.IO
import Network
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Binary (encode, decode, Binary)
import Data.Maybe (isNothing, fromJust)
import System.Process (rawSystem, readProcess, runProcess, ProcessHandle)
import System.Exit (ExitCode (ExitSuccess, ExitFailure))

import Cortex.Common.LazyIO
import Cortex.Common.IOReport

-----

type EIO m a = (MonadError String m, MonadIO m) => m a

-----

iPrintLog :: String -> EIO m ()
iPrintLog s = ioReport $ hPutStrLn stderr s

-----

iSetBuffering :: (MonadError String m, MonadIO m) =>
    Handle -> BufferMode -> m ()
iSetBuffering hdl mode = ioReport $ hSetBuffering hdl mode

-----

iListenOn :: (MonadError String m, MonadIO m) => Int -> m Socket
iListenOn port = ioReport $ listenOn $ PortNumber $ fromIntegral port

-----

iAccept :: (MonadError String m, MonadIO m) =>
    Socket -> m (LazyHandle, HostName, Int)
iAccept sock = do
    (hdl', host, port) <- ioReport $ accept sock
    hdl <- lConvert hdl'
    return (hdl, host, read $ show port)

-----

iConnectTo :: (MonadError String m, MonadIO m) => String -> Int -> m LazyHandle
iConnectTo host port = do
    hdl' <- ioReport $ connectTo host $ PortNumber $ fromIntegral port
    lConvert hdl'

-----

iEncode :: (Monad m) => Binary a => a -> m LBS.ByteString
iEncode a = return $ encode a

-----

iDecode :: (Binary a, MonadError String m, MonadIO m) => LBS.ByteString -> m a
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
    let loc = if isNothing location
        then "/"
        else fromJust location
    -- Use the Python wrapper, so `terminateProcess` sends a SIGKILL to the
    -- process instead of the default SIGTERM.
    runProcess "./Common/runprocess.py" (loc:cmd:args) Nothing env
        (Just input) (Just output) Nothing

-----

iOpenTempFile :: (MonadError String m, MonadIO m) => String -> String ->
    m (String, LazyHandle)
iOpenTempFile dir name = do
    (path, hdl) <- ioReport $ openTempFile dir name
    ioReport $ hClose hdl
    lHdl <- lOpenFile path WriteMode
    return (path, lHdl)

-----

iWriteFile :: (MonadError String m, MonadIO m) => String -> LBS.ByteString -> m ()
iWriteFile path str = do
    hdl <- lOpenFile path WriteMode
    lPutStr hdl str
    lClose hdl

-----

iReadFile :: (MonadError String m, MonadIO m) => String -> m LBS.ByteString
iReadFile path = do
    hdl <- lOpenFile path ReadMode
    str <- lGetContents hdl
    lClose hdl
    return str

-----
