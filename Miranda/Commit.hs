module Cortex.Miranda.Commit
    ( Commit
    , Hash
    , set
    , delete
    , getValue
    , getValueHash
    , rebase
    , getKey
    , getHash
    , isDelete
    , isSet
    , toString
    , fromString
    ) where

import Cortex.Miranda.CommitRaw

-- This file contains public interface to `Cortex.Miranda.CommitRaw` and should
-- be used instead of it in production code.
