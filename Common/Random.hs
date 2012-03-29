module Cortex.Common.Random
    ( generate
    ) where

import Control.Monad.Trans (liftIO, MonadIO)
import System.Random
import Data.Set (Set)
import qualified Data.Set as Set

-----

generate :: (MonadIO m) => (Int, Int) -> Int -> m [Int]
generate (a, b) n
    | b - a + 1 < n = generate (a, b) (b - a + 1)
    | otherwise = generate' (a, b) n Set.empty

generate' :: (MonadIO m) => (Int, Int) -> Int -> Set Int -> m [Int]
generate' _ 0 s = return $ Set.toList s
generate' (a, b) n s = do
    x <- liftIO $ randomRIO (a, b)
    if Set.member x s
        then generate' (a, b) n s
        else generate' (a, b) (n - 1) (Set.insert x s)
