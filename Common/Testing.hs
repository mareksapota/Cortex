module Cortex.Common.Testing 
    ( runInIOStateError
    , runFailInIOStateError
    ) where

import Test.HUnit
import System.IO.Temp
import Control.Monad.State
import Control.Monad.Error
import System.IO.Unsafe

-----
-- Run tests in `Error String`, `State String`, and `IO` monads.

runInIOStateError :: ErrorT String (StateT String IO) Assertion -> Test
runInIOStateError s = TestCase $ unsafePerformIO inIO
    where
        inIO :: IO Assertion
        inIO = withSystemTempDirectory "hunit" (evalStateT inState)

        inState :: StateT String IO Assertion
        inState = do
            a <- runErrorT s
            case a of
                Left e -> return $ assertBool (show e) False
                Right b -> return b

-----

-----
-- Run tests in `Error String`, `State String`, and `IO` monads expecting a
-- failure.

runFailInIOStateError :: ErrorT String (StateT String IO) a -> Test
runFailInIOStateError s = TestCase $ unsafePerformIO inIO
    where
        inIO :: IO Assertion
        inIO = withSystemTempDirectory "hunit" (evalStateT inState)

        inState :: StateT String IO Assertion
        inState = do
            a <- runErrorT s
            case a of
                Left _ -> return $ assertBool "" True
                Right _ -> return $ assertBool "" False

-----
