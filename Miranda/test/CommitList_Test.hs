import Test.HUnit

import qualified Cortex.Miranda.CommitRaw as C
import Cortex.Miranda.CommitList

-----
-- Test if insertion order matters.

test0 :: Test
test0 = TestCase $ do
    let a = C.Commit ("a", C.Set "1", "", "2012.03.18 20:32:32:910425188000")
    let b = C.Commit ("b", C.Set "2", "", "2012.03.18 20:32:33:910425188000")
    let c = C.Commit ("c", C.Set "3", "", "2012.03.18 20:32:34:910425188000")
    let add x y = fst $ insert x y
    let cl1 = add a $ add b $ add c empty
    let cl2 = add a $ add c $ add b empty
    let cl3 = add c $ add b $ add a empty
    let cl4 = add b $ add c $ add a empty

    let t1 = member "2fba5c6f2ca481b06b69336926c51bfbd57e27a9" cl1
    let t2 = member "2fba5c6f2ca481b06b69336926c51bfbd57e27a9" cl2
    let t3 = member "2fba5c6f2ca481b06b69336926c51bfbd57e27a9" cl3
    let t4 = member "2fba5c6f2ca481b06b69336926c51bfbd57e27a9" cl4

    let t5 = member "3a67d20754c9ca37b0da711cfdaf0b54c75f59ff" cl1
    let t6 = member "3a67d20754c9ca37b0da711cfdaf0b54c75f59ff" cl2
    let t7 = member "3a67d20754c9ca37b0da711cfdaf0b54c75f59ff" cl3
    let t8 = member "3a67d20754c9ca37b0da711cfdaf0b54c75f59ff" cl4

    let t9 = member "6f5c2c772412e41d75f5fa64ceb81bdbf11462b0" cl1
    let t10 = member "6f5c2c772412e41d75f5fa64ceb81bdbf11462b0" cl2
    let t11 = member "6f5c2c772412e41d75f5fa64ceb81bdbf11462b0" cl3
    let t12 = member "6f5c2c772412e41d75f5fa64ceb81bdbf11462b0" cl4

    assertBool "cl1 lost commits" $ (==) 3 (length $ toList cl1)
    assertBool "cl2 lost commits" $ (==) 3 (length $ toList cl2)
    assertBool "cl3 lost commits" $ (==) 3 (length $ toList cl3)
    assertBool "cl4 lost commits" $ (==) 3 (length $ toList cl4)
    assertBool "hash mismatch" $
        and [t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12]

-----

-----
-- Test if converting to list works.

test1 :: Test
test1 = TestCase $ do
    let a = C.Commit ("a", C.Set "1", "", "2012.03.18 20:32:32:910425188000")
    let b = C.Commit ("b", C.Set "2", "", "2012.03.18 20:32:33:910425188000")
    let c = C.Commit ("c", C.Set "3", "", "2012.03.18 20:32:34:910425188000")
    let add x y = fst $ insert x y
    let cl = add a $ add b $ add c empty

    assertBool "" $ (==) (toList cl) [c, b, a]

-----

-----
-- Test insertion of equal commits.

test2 :: Test
test2 = TestCase $ do
    let a = C.Commit ("a", C.Set "1", "", "2012.03.18 20:32:32:910425188000")
    let add x y = fst $ insert x y
    let cl = add a $ add a $ add a empty
    let r = snd $ insert a cl

    assertBool "" $ null r
    assertBool "" $ (==) 1 (length $ toList cl)

-----

-----

tests :: Test
tests = TestList
    [ test0
    , test1
    , test2
    ]

main :: IO Counts
main = runTestTT tests
