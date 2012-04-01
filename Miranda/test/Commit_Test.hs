import Prelude hiding (lookup)
import Test.HUnit
import Data.ByteString.Lazy.Char8

import Cortex.Common.Testing
import Cortex.Miranda.CommitRaw
import Cortex.Common.ErrorIO

-----
-- Equality tests.

test1 :: Test
test1 = TestCase $ assertBool "" $ (==)
    (Commit "a" Delete "hash" "2012.03.18 19:32:32:910425188000")
    (Commit "a" Delete "hash" "2012.03.18 19:32:32:910425188000")

test2 :: Test
test2 = TestCase $ assertBool "" $ (/=)
    (Commit "a" Delete "hash" "2012.03.18 19:32:32:910425188000")
    (Commit "b" Delete "hash" "2012.03.18 19:32:32:910425188000")

test3 :: Test
test3 = TestCase $ assertBool "" $ (==)
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
    assertBool "" $ (==)
        (getHash $ rebase (Just a) b)
        "6002ed55882fc9ca46f8c6dc1883a5b9094bb62e"
    assertBool "" $ (==)
        (getHash $ rebase Nothing b)
        "634ef390d319a744ba836b54a92c1fb97215b57b"

-----

-----
-- Serialization tests.

test13 :: Test
test13 = runInIOStateError $ do
    a <- set "ala" (pack "makota")
    b <- toString a
    c <- fromString b
    return $ assertBool "" $ (==) a c

test15 :: Test
test15 = runInIOStateError $ do
    a <- set "ala" (pack "makota")
    b <- iEncode a
    c <- iDecode b
    return $ assertBool "" $ (==) a c

-----

-----
-- Property extraction tests.

test14 :: Test
test14 = TestCase $ assertBool "" $ (==)
    (getHash $ Commit "a" Delete "abc" "time")
    "abc"

test16 :: Test
test16 = TestCase $ assertBool "" $ (==)
    (getKey $ Commit "a" Delete "abc" "time")
    "a"

test17 :: Test
test17 = runInIOStateError $ do
    a <- getValueHash $ Commit "a" (Set "123") "abc" "time"
    return $ assertBool "" $ (==) a "123"

test18 :: Test
test18 = runInIOStateError $ do
    a <- set "ala" (pack "makota")
    b <- getValue a
    return $ assertBool "" $ (==) b (pack "makota")

test19 :: Test
test19 = runFailInIOStateError $ do
    a <- delete "moo"
    getValue a
    return ()

test20 :: Test
test20 = runFailInIOStateError $ do
    a <- delete "moo"
    getValueHash a
    return ()

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
    , TestLabel "test15" test15
    , TestLabel "test16" test16
    , TestLabel "test17" test17
    , TestLabel "test18" test18
    , TestLabel "test19" test19
    , TestLabel "test20" test20
    ]

main :: IO Counts
main = runTestTT tests
