import Test.HUnit

import Adria.Common.Util

-----
-- Partitioning tests.

-- Pair of ints compared only by the first value.
data Pair = Pair Int Int
    deriving (Show)

instance Eq Pair where
    (Pair a _) == (Pair b _) = a == b

instance Ord Pair where
    compare (Pair a _) (Pair b _) = compare a b

equal :: [Pair] -> [Pair] -> Bool
equal a b = foldl equal' True $ zip a b

equal' :: Bool -> (Pair, Pair) -> Bool
equal' v (Pair a b, Pair c d) = v && (a == c) && (b == d)

test1 :: Test
test1 = TestCase $ do
    let a = [Pair 2 1, Pair 1 1, Pair 2 2, Pair 2 3, Pair 1 2]
    let b = [Pair 1 1, Pair 1 2]
    let c = [Pair 2 1, Pair 2 2, Pair 2 3]

    let (b', c') = stablePartition (< Pair 2 5) a
    let b'' = stableFilter (< Pair 2 5) a
    let c'' = stableFilter (>= Pair 2 5) a
    assertBool "" $ equal b b'
    assertBool "" $ equal b b''
    assertBool "" $ equal c c'
    assertBool "" $ equal c c''

-----

tests :: Test
tests = TestList
    [ TestLabel "test1" test1
    ]

main :: IO Counts
main = runTestTT tests
