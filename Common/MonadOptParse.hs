{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Adria.Common.MonadOptParse
    ( OptParseT
    , OptParse
    , MonadOptParse
    , addOption
    , execOptParseT
    , execOptParse
    ) where

import Control.Monad.Identity
import Control.Monad.State

import qualified Adria.Common.OptParse as P

-- Monad for easy argument adding.

newtype OptParseT m a = OptParseT (StateT P.CmdArgs m a)
    deriving (Monad, MonadTrans)
newtype OptParse a = OptParse (OptParseT Identity a)
    deriving (Monad, MonadOptParse)

class (Monad m) => MonadOptParse m where
    addOption :: [P.CmdSwitch] -> P.CmdName -> P.CmdHelp -> m ()

instance (Monad m) => MonadOptParse (OptParseT m) where
    addOption switches name help = OptParseT $ do
        options <- get
        put $ P.addOption switches name help options

execOptParseT :: (Monad m) => OptParseT m a -> m P.CmdArgs
execOptParseT (OptParseT s) = execStateT s P.empty

execOptParse :: OptParse a -> P.CmdArgs
execOptParse (OptParse s) = runIdentity $ execOptParseT s
