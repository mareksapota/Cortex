{-# LANGUAGE FlexibleContexts #-}

module Adria.Common.Middleware
    ( send
    , subscribe
    ) where

-----

import System.IO (stderr)
import Control.Monad
import Control.Monad.Trans (MonadIO)
import Control.Monad.Error (runErrorT, MonadError)
import qualified Data.List as List
import qualified Data.Set as Set

import Adria.Common.ErrorIO

-----
-- `send` runs in it's own Error monad, all connection errors will be contained.

send :: (MonadIO m, MonadError String m) =>
    String -> Int -> [String] -> String -> m ()
send host port topics msg = do
    e <- runErrorT (send' host port topics msg)
    reportError e
    where
        reportError (Left s) = iPutStrLn stderr $
            "Middleware communication error: " ++ s
        reportError (Right _) = return ()

send' :: (MonadIO m, MonadError String m) =>
    String -> Int -> [String] -> String -> m ()
send' host port topics msg = do
    hdl <- iConnectTo host port
    iPutStrLn hdl "publish"
    forM_ topics (\t -> iPutStrLn hdl t)
    iPutStrLn hdl ""
    iPutStrLn hdl msg
    iClose hdl

-----

subscribe :: (MonadIO m, MonadError String m) =>
    String -> Int -> [String] -> ([String] -> String -> m ()) -> m ()
subscribe host port topics handler = do
    hdl <- iConnectTo host port
    iPutStrLn hdl "subscribe"
    forM_ topics (\t -> iPutStrLn hdl ('+':t))
    iFlush hdl
    forever $ iGetLine hdl >>= process
    where
        topicsSet = Set.fromList topics

        process msg = do
            let t = split $ takeWhile (/= ' ') msg
            let m = tail $ dropWhile (/= ' ') msg
            handler (Set.toList $ Set.intersection topicsSet t) m

        split s = Set.fromList $ filter (/= "|") (List.groupBy cmp s)

        cmp '|' '|' = True
        cmp '|' _ = False
        cmp _ '|' = False
        cmp _ _ = True
