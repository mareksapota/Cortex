import Prelude hiding (lookup)
import Test.HUnit

import Cortex.Miranda.Commit

-----
-- Equality tests.

test1 :: Test
test1 = TestCase $ assertEqual ""
    (Commit "a" Delete "hash" "2012.03.18 19:32:32:910425188000")
    (Commit "a" Delete "hash" "2012.03.18 19:32:32:910425188000")

test2 :: Test
test2 = TestCase $ assertBool "" $ (/=)
    (Commit "a" Delete "hash" "2012.03.18 19:32:32:910425188000")
    (Commit "b" Delete "hash" "2012.03.18 19:32:32:910425188000")

test3 :: Test
test3 = TestCase $ assertEqual ""
    (Commit "a" Delete "hash" "2012.03.18 19:32:32:910425188000")
    (Commit "a" Delete "other hash" "2012.03.18 19:32:32:910425188000")

test4 :: Test
test4 = TestCase $ assertBool "" $ (/=)
    (Commit "a" Delete "hash" "2012.03.18 19:32:32:910425188000")
    (Commit "a" Delete "hash" "2011.03.18 19:32:32:910425188000")

-----

-----
-- Ordering tests.

test5 :: Test
test5 = TestCase $ assertBool "" $ (<)
    (Commit "a" Delete "hash" "2011.03.18 19:32:32:910425188000")
    (Commit "a" Delete "hash" "2012.03.18 19:32:32:910425188000")

test6 :: Test
test6 = TestCase $ assertBool "" $ (<)
    (Commit "a" Delete "hash" "2012.03.18 19:32:32:910425188000")
    (Commit "a" (Set "moo") "hash" "2012.03.18 19:32:32:910425188000")

test7 :: Test
test7 = TestCase $ assertBool "" $ (<)
    (Commit "a" (Set "a") "hash" "2012.03.18 19:32:32:910425188000")
    (Commit "a" (Set "b") "hash" "2012.03.18 19:32:32:910425188000")

test8 :: Test
test8 = TestCase $ assertBool "" $ (<)
    (Commit "a" Delete "hash" "2012.03.18 19:32:32:910425188000")
    (Commit "b" Delete "hash" "2012.03.18 19:32:32:910425188000")

test9 :: Test
test9 = TestCase $ assertBool "" $ (<)
    (Commit "b" Delete "hash" "2011.03.18 19:32:32:910425188000")
    (Commit "a" Delete "hash" "2012.03.18 19:32:32:910425188000")

test10 :: Test
test10 = TestCase $ assertBool "" $ (<)
    (Commit "a" (Set "moo") "hash" "2011.03.18 19:32:32:910425188000")
    (Commit "a" Delete "hash" "2012.03.18 19:32:32:910425188000")

test11 :: Test
test11 = TestCase $ assertBool "" $ (<)
    (Commit "a" (Set "moo") "hash" "2012.03.18 19:32:32:910425188000")
    (Commit "b" Delete "hash" "2012.03.18 19:32:32:910425188000")

-----

-----
-- Rebase tests.

test12 :: Test
test12 = TestCase $ do
    let a = Commit "a" Delete "abc" "2012.03.18 19:32:32:910425188000"
    let b = Commit "b" (Set "moo") "" "2012.03.18 20:32:32:910425188000"
    assertEqual ""
        (getHash $ rebase (Just a) b)
        "6002ed55882fc9ca46f8c6dc1883a5b9094bb62e"
    assertEqual ""
        (getHash $ rebase Nothing b)
        "634ef390d319a744ba836b54a92c1fb97215b57b"

-----

-----
-- Read and show tests.

test13 :: Test
test13 = TestCase $ do
    a <- new "ala" (Set "makota")
    let b = read $ show a
    assertEqual "" a b

-----

-----
-- Hash extraction tests.

test14 :: Test
test14 = TestCase $ assertEqual ""
    (getHash $ Commit "a" Delete "abc" "time")
    "abc"

-----

-----

tests :: Test
tests = TestList
    [ TestLabel "test1" test1
    , TestLabel "test2" test2
    , TestLabel "test3" test3
    , TestLabel "test4" test4
    , TestLabel "test5" test5
    , TestLabel "test6" test6
    , TestLabel "test7" test7
    , TestLabel "test8" test8
    , TestLabel "test9" test9
    , TestLabel "test10" test10
    , TestLabel "test11" test11
    , TestLabel "test12" test12
    , TestLabel "test13" test13
    , TestLabel "test14" test14
    ]

main :: IO Counts
main = runTestTT tests
