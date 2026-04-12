module Fuzzy.Sets.FuzzyCardinalityTest (
    fuzzyCardinalityTests
) where

import Test.Tasty
import Test.Tasty.HUnit
import Fuzzy.Sets.LSet
import Fuzzy.Sets.FuzzyCardinality
import Lattices.UnitIntervalStructures.Lukasiewicz
import Lattices.ResiduatedLattice
import Utils.Utils
import Lattices.UnitIntervalStructures.Product
import Lattices.UnitIntervalStructures.Godel 

fuzzyCardinalityTests :: TestTree
fuzzyCardinalityTests = testGroup "Fuzzy Cardinality Tests" [
    testCase "bracket identifies max alpha for given k" testBracket,
    testCase "fgCount maps counts to maximal alpha" testFgCount,
    testCase "flCount maps counts to minimal alpha" testFlCount,
    testCase "feCount maps counts to intersection" testFeCount,
    testCase "ralescuF returns fuzzy count" testRalescuF,
    testCase "generalisedFGCount on Lukasiewicz" testGeneralizedFGLuk,
    testCase "generalisedFGCount on Godel" testGeneralizedFGGodel,
    testCase "generalisedFGCount on Product" testGeneralizedFGProd
    --testCase "generalisedFLCount" testGeneralizedFL,
    --testCase "generalisedFECount" testGeneralizedFE
    ]

whereSet :: (ResiduatedLattice l) => LSet String l
whereSet = fromList [("a", 0.1), ("b", 0.2), ("c", 0.1), ("d", 0.5), ("e", 0.7), ("f", 0.2), ("g", 0.4)]


testBracket :: Assertion
testBracket =
    assertApproxEqual "bracket 1 should be 0.7"
        (fromLukasiewiczUnitInterval $ bracket 1 whereSet)
        0.7


testFgCount :: Assertion
testFgCount = do
    let c = fgCount whereSet :: LSet Int UILukasiewicz
        expected = [(0,1),(1,0.7),(2,0.5),(3,0.4),(4,0.2),(5,0.2),(6,0.1),(7,0.1)]
        actual = [(k, fromLukasiewiczUnitInterval v) | (k,v) <- toList c]
        pairs = zip expected actual
    mapM_ (\(e,a) -> do
            assertEqual "fgCount key" (fst e) (fst a)
            assertApproxEqual "fgCount value" (snd a) (snd e)
        ) pairs


testFlCount :: Assertion
testFlCount = do
    let c = flCount whereSet :: LSet Int UILukasiewicz
        expected = [(0,0.3),(1,0.5),(2,0.6),(3,0.8),(4,0.8),(5,0.9),(6,0.9),(7,1.0)]
        actual = [(k, fromLukasiewiczUnitInterval v) | (k,v) <- toList c]
        pairs = zip expected actual
    mapM_ (\(e,a) -> do
            assertEqual "flCount key" (fst e) (fst a)
            assertApproxEqual "flCount value" (snd a) (snd e)
        ) pairs


testFeCount :: Assertion
testFeCount = do
    let c = feCount whereSet :: LSet Int UILukasiewicz
        expected = [(0,0.3),(1,0.5),(2,0.5),(3,0.4),(4,0.2),(5,0.2),(6,0.1),(7,0.1)]
        actual = [(k, fromLukasiewiczUnitInterval v) | (k,v) <- toList c]
        pairs = zip expected actual
    mapM_ (\(e,a) -> do
            assertEqual "feCount key" (fst e) (fst a)
            assertApproxEqual "feCount value" (snd a) (snd e)
        ) pairs


testRalescuF :: Assertion
testRalescuF = do
    let c = ralescuF whereSet :: LSet Int UILukasiewicz
        expected = [(0,0.3),(1,0.5),(2,0.5),(3,0.4),(4,0.2),(5,0.2),(6,0.1),(7,0.1)]
        actual = [(k, fromLukasiewiczUnitInterval v) | (k,v) <- toList c]
        pairs = zip expected actual
    mapM_ (\(e,a) -> do
            assertEqual "ralescuF key" (fst e) (fst a)
            assertApproxEqual "ralescuF value" (snd a) (snd e)
        ) pairs



testGeneralizedFGProd :: Assertion 
testGeneralizedFGProd = do
    let 
        c = generalizedFGCount whereSet :: LSet Int UIProduct
        expected = [(0,1),(1,0.7),(2,0.35),(3, 0.14),(4,0.028),(5,0.0056),(6,0.00056),(7,0.000056)]
        actual = [(k, fromProductUnitInterval v) | (k,v) <- toList c]
        pairs = zip expected actual

    mapM_ (\(e,a) -> do
        assertEqual "generalizedFG product key" (fst e) (fst a)
        assertApproxEqual "generalizedFG product value" (snd a) (snd e)
        ) pairs



testGeneralizedFGGodel :: Assertion
testGeneralizedFGGodel = do 
    let 
        c = generalizedFGCount whereSet :: LSet Int UIGodel
        expected = [(0,1),(1,0.7),(2,0.5),(3,0.4),(4,0.2),(5,0.2),(6,0.1),(7,0.1)]
        actual = [(k, fromGodelUnitInterval v) | (k,v) <- toList c]
        pairs = zip expected actual

    mapM_ (\(e,a) -> do
        assertEqual "generalizedFG godel key" (fst e) (fst a)
        assertApproxEqual "generalizedFG godel value" (snd a) (snd e)
        ) pairs


testGeneralizedFGGodelEqualsFG :: Assertion
testGeneralizedFGGodelEqualsFG = do 
    let 
        c = generalizedFGCount whereSet :: LSet Int UIGodel
        c2 = fgCount whereSet :: LSet Int UIGodel
        expected = [(k, fromGodelUnitInterval v) | (k,v) <- toList c2]
        actual = [(k, fromGodelUnitInterval v) | (k,v) <- toList c]
        pairs = zip expected actual

    mapM_ (\(e,a) -> do
        assertEqual "generalizedFG godel equals key" (fst e) (fst a)
        assertApproxEqual "generalizedFG godel equal FGCount value" (snd a) (snd e)
        ) pairs


testGeneralizedFGLuk :: Assertion
testGeneralizedFGLuk = do
    let c = generalizedFGCount whereSet :: LSet Int UILukasiewicz
        expected = [(0,1),(1,0.7),(2,0.2),(3,0),(4,0),(5,0),(6,0),(7,0)]
        actual = [(k, fromLukasiewiczUnitInterval v) | (k,v) <- toList c]
        pairs = zip expected actual

    mapM_ (\(e,a) -> do
        assertEqual "generalizedFG lukasiewicz key" (fst e) (fst a)
        assertApproxEqual "generalizedFG lukasiewicz value" (snd a) (snd e)
        ) pairs

{-
testGeneralizedFL :: Assertion
testGeneralizedFL = do
    let cLuk = generalizedFLCount whereSet :: LSet Int UILukasiewicz
        expectedLuk = [(0,0),(1,0),(2,0),(3,0.4),(4,0.6),(5,0.8),(6,0.9),(7,1)]
        actualLuk = [(k, fromLukasiewiczUnitInterval v) | (k,v) <- toList cLuk]
        pairsLuk = zip expectedLuk actualLuk

        cGodel = generalizedFLCount whereSet :: LSet Int UIGodel
        expectedGodel = [(0,0),(1,0),(2,0),(3,0),(4,0),(5,0),(6,0),(7,1)]
        actualGodel = [(k, fromGodelUnitInterval v) | (k,v) <- toList cGodel]
        pairsGodel = zip expectedGodel actualGodel

        cProduct = generalizedFLCount whereSet :: LSet Int UIProduct
        expectedProduct = [(0,0),(1,0),(2,0),(3,0),(4,0),(5,0),(6,0),(7,1)]
        actualProduct = [(k, fromProductUnitInterval v) | (k,v) <- toList cProduct]
        pairsProduct = zip expectedProduct actualProduct

    mapM_ (\(e,a) -> do
        assertEqual "generalizedFL lukasiewicz key" (fst e) (fst a)
        assertApproxEqual "generalizedFL lukasiewicz value" (snd a) (snd e)
        ) pairsLuk

    mapM_ (\(e,a) -> do
        assertEqual "generalizedFL godel key" (fst e) (fst a)
        assertApproxEqual "generalizedFL godel value" (snd a) (snd e)
        ) pairsGodel

    mapM_ (\(e,a) -> do
        assertEqual "generalizedFL product key" (fst e) (fst a)
        assertApproxEqual "generalizedFL product value" (snd a) (snd e)
        ) pairsProduct


testGeneralizedFE :: Assertion
testGeneralizedFE = do
    let cLuk = generalizedFECount whereSet :: LSet Int UILukasiewicz
        expectedLuk = [(0,0),(1,0),(2,0),(3,0),(4,0),(5,0),(6,0),(7,0)]
        actualLuk = [(k, fromLukasiewiczUnitInterval v) | (k,v) <- toList cLuk]
        pairsLuk = zip expectedLuk actualLuk

        cGodel = generalizedFECount whereSet :: LSet Int UIGodel
        expectedGodel = [(0,0),(1,0),(2,0),(3,0),(4,0),(5,0),(6,0),(7,0.1)]
        actualGodel = [(k, fromGodelUnitInterval v) | (k,v) <- toList cGodel]
        pairsGodel = zip expectedGodel actualGodel

        cProduct = generalizedFECount whereSet :: LSet Int UIProduct
        expectedProduct = [(0,0),(1,0),(2,0),(3,0),(4,0),(5,0),(6,0),(7,0.000056)]
        actualProduct = [(k, fromProductUnitInterval v) | (k,v) <- toList cProduct]
        pairsProduct = zip expectedProduct actualProduct

    mapM_ (\(e,a) -> do
        assertEqual "generalizedFE lukasiewicz key" (fst e) (fst a)
        assertApproxEqual "generalizedFE lukasiewicz value" (snd a) (snd e)
        ) pairsLuk

    mapM_ (\(e,a) -> do
        assertEqual "generalizedFE godel key" (fst e) (fst a)
        assertApproxEqual "generalizedFE godel value" (snd a) (snd e)
        ) pairsGodel

    mapM_ (\(e,a) -> do
        assertEqual "generalizedFE product key" (fst e) (fst a)
        assertApproxEqual "generalizedFE product value" (snd a) (snd e)
        ) pairsProduct
-}