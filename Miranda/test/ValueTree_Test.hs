import Prelude hiding (lookup)
import Test.HUnit

import Cortex.Miranda.ValueTree

-----
-- Insert/delete tests.

test1 :: Test
test1 = TestCase $ assertEqual ""
    empty
    (delete "a" $ insert "a" "b" empty)

test2 :: Test
test2 = TestCase $ assertEqual ""
    empty
    (delete "a" $ delete "a::b" $ insert "a::b" "c" $ insert "a" "b" empty)

test3 :: Test
test3 = TestCase $ assertEqual ""
    empty
    (delete "a::b" $ delete "a" $ insert "a::b" "c" $ insert "a" "b" empty)

test4 :: Test
test4 = TestCase $ assertEqual ""
    (insert "a" "b" empty)
    (delete "a::b" $ insert "a::b" "c" $ insert "a" "b" empty)

test5 :: Test
test5 = TestCase $ assertEqual ""
    (insert "a::b" "c" empty)
    (delete "a" $ insert "a::b" "c" $ insert "a" "b" empty)

-----

-----
-- Lookup tests.

test6 :: Test
test6 = TestCase $ assertEqual ""
    Nothing
    (lookup "a" $ insert "a::b" "c" empty)

test7 :: Test
test7 = TestCase $ assertEqual ""
    Nothing
    (lookup "a::b::c" $ insert "a::b" "c" empty)

test8 :: Test
test8 = TestCase $ assertEqual ""
    (Just "c")
    (lookup "a::b" $ insert "a::b" "c" empty)

test9 :: Test
test9 = TestCase $ assertEqual ""
    (Just "d")
    (lookup "a:b" $ insert "a::b" "c" $ insert "a:b" "d" empty)

test10 :: Test
test10 = TestCase $ assertEqual ""
    [("a:b", "c")]
    (lookupAll "x" $ insert "x::a:b" "c" empty)

test11 :: Test
test11 = TestCase $ assertEqual ""
    (Just "a")
    (lookup "" $ insert "" "a" empty)

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
    ]

main :: IO Counts
main = runTestTT tests
