import Prelude hiding (lookup)
import Test.HUnit
import Data.ByteString.Lazy.Char8 (pack)

import Cortex.Common.Testing
import Cortex.Miranda.ValueTree
import qualified Cortex.Miranda.Commit as C

-----
-- Insert/delete tests.

test1 :: Test
test1 = runInIOStateError $ do
    a <- C.delete "moo"
    return $ assertBool "" $ (==)
        empty
        (delete "a" $ insert "a" a empty)

test2 :: Test
test2 = runInIOStateError $ do
    a <- C.delete "moo"
    b <- C.delete "moo"
    return $ assertBool "" $ (==)
        empty
        (delete "a" $ delete "a::b" $ insert "a::b" a $ insert "a" b empty)

test3 :: Test
test3 = runInIOStateError $ do
    a <- C.delete "moo"
    b <- C.delete "moo"
    return $ assertBool "" $ (==)
        empty
        (delete "a::b" $ delete "a" $ insert "a::b" a $ insert "a" b empty)

test4 :: Test
test4 = runInIOStateError $ do
    a <- C.delete "moo"
    b <- C.delete "moo"
    return $ assertBool "" $ (==)
        (insert "a" b empty)
        (delete "a::b" $ insert "a::b" a $ insert "a" b empty)

test5 :: Test
test5 = runInIOStateError $ do
    a <- C.delete "moo"
    b <- C.delete "moo"
    return $ assertBool "" $ (==)
        (insert "a::b" a empty)
        (delete "a" $ insert "a::b" a $ insert "a" b empty)

-----

-----
-- Lookup tests.

test6 :: Test
test6 = runInIOStateError $ do
    a <- C.set "a" (pack "b")
    v <- lookup "a" $ insert "a::b" a empty
    return $ assertBool "" $ (==) Nothing v

test7 :: Test
test7 = runInIOStateError $ do
    a <- C.set "a" (pack "b")
    v <- lookup "a::b::c" $ insert "a::b" a empty
    return $ assertBool "" $ (==) Nothing v

test8 :: Test
test8 = runInIOStateError $ do
    a <- C.set "a" (pack "b")
    v <- lookup "a::b" $ insert "a::b" a empty
    return $ assertBool "" $ (==) (Just (pack "b")) v

test9 :: Test
test9 = runInIOStateError $ do
    a <- C.set "a" (pack "b")
    b <- C.set "b" (pack "c")
    v <- lookup "a:b" $ insert "a::b" a $ insert "a:b" b empty
    return $ assertBool "" $ (==) (Just (pack "c")) v

test10 :: Test
test10 = runInIOStateError $ do
    a <- C.set "a" (pack "b")
    b <- C.set "b" (pack "c")
    v <- lookupAll "x" $ insert "x::a:b" a $ insert "a::b" b empty
    return $ assertBool "" $ (==) [("a:b", (pack "b"))] v

test11 :: Test
test11 = runInIOStateError $ do
    a <- C.set "a" (pack "b")
    v <- lookup "" $ insert "" a empty
    return $ assertBool "" $ (==) (Just (pack "b")) v

test12 :: Test
test12 = runInIOStateError $ do
    a <- C.set "a" (pack "b")
    b <- C.set "b" (pack "c")
    v <- lookupAllWhere "x" (\_ v -> v == (pack "b")) $
        insert "x::a:b" a $ insert "x::b" b empty
    return $ assertBool "" $ (==) [("a:b", (pack "b"))] v

test13 :: Test
test13 = runInIOStateError $ do
    a <- C.set "a" (pack "b")
    b <- C.set "b" (pack "c")
    v <- lookupAllWhere "x" (\k _ -> k == "a:b") $
        insert "x::a:b" a $ insert "x::b" b empty
    return $ assertBool "" $ (==) [("a:b", (pack "b"))] v

test14 :: Test
test14 = runInIOStateError $ do
    a <- C.set "a" (pack "b")
    v <- lookupHash "a" $ insert "a" a empty
    return $ assertBool "" $ (==)
        (Just "e9d71f5ee7c92d6dc9e92ffdad17b8bd49418f98")
        v

-----

-----
-- Commit application tests.

test15 :: Test
test15 = runInIOStateError $ do
    a <- C.set "a" (pack "a")
    b <- C.set "b" (pack "b")
    c <- C.delete "b"
    return $ assertBool "" $ (==)
        (insert "a" a empty)
        (apply c $ apply b $ apply a empty)

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
    ]

main :: IO Counts
main = runTestTT tests
