{-# LANGUAGE DefaultSignatures, TypeSynonymInstances, FlexibleInstances #-}

module Cortex.Common.MaybeRead
    ( MaybeRead
    , maybeRead
    ) where

import Data.Maybe (listToMaybe)

-- `maybeRead` acts as a version of `read` that reports errors through the Maybe
-- monad.  Additionaly it treats strings differently, not requiring quotation
-- marks around them.
class MaybeRead a where
    maybeRead :: String -> Maybe a
    default maybeRead :: Read a => String -> Maybe a
    maybeRead = fmap fst . listToMaybe . reads

instance MaybeRead String where
    maybeRead s = Just s

instance MaybeRead Int
instance MaybeRead Double
