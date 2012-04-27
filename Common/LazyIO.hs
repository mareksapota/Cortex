{-# LANGUAGE FlexibleContexts, OverloadedStrings, Rank2Types #-}

module Cortex.Common.LazyIO
    ( LazyHandle
    , lConvert
    , lOpenFile
    , lClose
    , lFlush
    , lPutStr
    , lPutStrLn
    , lGetLine
    , lGetLines
    , lGetContents
    ) where

-----
-- IO operations for lazy byte string.  Methods in this library are not thread
-- safe and lazy handles should not be used concurrently by multiple threads.
-----

import Control.Monad (when)
import Control.Monad.Error (MonadError, throwError)
import Control.Monad.Trans (liftIO, MonadIO)
import qualified Data.ByteString.Lazy.Char8 as LBS
import System.IO
import Data.IORef

import Cortex.Common.IOReport

-----

data LazyHandle =
      ReadHandle (IORef LBS.ByteString)
    | WriteHandle Handle
    | RWHandle (IORef LBS.ByteString) Handle

-----

type EIO m a = (MonadError String m, MonadIO m) => m a

-----
-- Returns a lazy handle capable of reading and writing.  Can be used for
-- sockets.

lConvert :: Handle -> EIO m LazyHandle
lConvert hdl = do
    content <- ioReport $ LBS.hGetContents hdl
    ref <- liftIO $ newIORef content
    return $ RWHandle ref hdl

-----

lOpenFile :: FilePath -> IOMode -> EIO m LazyHandle
lOpenFile path WriteMode = lOpenFile' path WriteMode
lOpenFile path ReadMode = lOpenFile' path ReadMode
lOpenFile _ _ = throwError "Unsupported IO mode"

lOpenFile' :: FilePath -> IOMode -> EIO m LazyHandle
lOpenFile' path mode = do
    hdl <- ioReport $ openFile path mode
    ioReport $ hSetBuffering hdl (BlockBuffering $ Just 32)
    chooseMode mode hdl
    where
        chooseMode ReadMode hdl = do
            content <- liftIO $ LBS.hGetContents hdl
            ref <- liftIO $ newIORef content
            return $ ReadHandle ref
        chooseMode WriteMode hdl = return $ WriteHandle hdl
        chooseMode _ hdl = do
            ioReport $ hClose hdl
            throwError "Unsupported IO mode"

-----

lClose :: LazyHandle -> EIO m ()
lClose (ReadHandle ref) = liftIO $ writeIORef ref ""
lClose (WriteHandle hdl) = ioReport $ hClose hdl
lClose (RWHandle ref hdl) = do
    lClose $ ReadHandle ref
    lClose $ WriteHandle hdl

-----

lFlush :: LazyHandle -> EIO m ()
lFlush (WriteHandle hdl) = ioReport $ hFlush hdl
lFlush (RWHandle _ hdl) = lFlush $ WriteHandle hdl
lFlush _ = throwError "Handle not open for writing"

-----

lPutStr :: LazyHandle -> LBS.ByteString -> EIO m ()
lPutStr (WriteHandle hdl) str = ioReport $ LBS.hPut hdl str
lPutStr (RWHandle _ hdl) str = lPutStr (WriteHandle hdl) str
lPutStr _ _ = throwError "Handle not open for writing"

-----

lPutStrLn :: LazyHandle -> LBS.ByteString -> EIO m ()
lPutStrLn (WriteHandle hdl) str = ioReport $ do
    LBS.hPut hdl str
    LBS.hPut hdl "\n"
lPutStrLn (RWHandle _ hdl) str = lPutStrLn (WriteHandle hdl) str
lPutStrLn _ _ = throwError "Handle not open for writing"

-----

lGetLine :: LazyHandle -> EIO m LBS.ByteString
lGetLine (ReadHandle ref) = do
    s <- liftIO $ readIORef ref
    let (l, s') = LBS.span (/= '\n') s
    -- Check for EOF.
    when (LBS.null $ LBS.take 1 s') $ throwError "EOF encountered"
    -- Drop the '\n'.
    liftIO $ writeIORef ref (LBS.tail s')
    return l
lGetLine (RWHandle ref _) = lGetLine $ ReadHandle ref
lGetLine _ = throwError "Handle not open for reading"

-----

lGetLines :: LazyHandle -> EIO m [LBS.ByteString]
lGetLines hdl = do
    c <- lGetContents hdl
    return $ LBS.lines c

-----

lGetContents :: LazyHandle -> EIO m LBS.ByteString
lGetContents (ReadHandle ref) = do
    s <- liftIO $ readIORef ref
    liftIO $ writeIORef ref ""
    return s
lGetContents (RWHandle ref _) = lGetContents $ ReadHandle ref
lGetContents _ = throwError "Handle not open for reading"

-----
