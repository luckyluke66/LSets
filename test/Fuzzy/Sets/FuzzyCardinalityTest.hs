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

fuzzyCardinalityTests :: TestTree
fuzzyCardinalityTests = testGroup "Fuzzy Cardinality Tests" [
    testCase "bracket identifies max alpha for given k" testBracket,
    testCase "fgCount maps counts to maximal alpha" testFgCount,
    testCase "flCount maps counts to minimal alpha" testFlCount,
    testCase "feCount maps counts to intersection" testFeCount
    ]

whereSet :: LSet String UILukasiewicz
whereSet = fromList [("a", 0.1), ("b", 0.2), ("c", 0.1), ("d", 0.5), ("e", 0.7), ("f", 0.2), ("g", 0.4)]


testBracket :: Assertion
testBracket =
    assertApproxEqual "bracket 1 should be 0.7"
        (fromLukasiewiczUnitInterval $ bracket 1 whereSet)
        0.7


testFgCount :: Assertion
testFgCount = do
    let c = fgCount whereSet :: LSet Int UILukasiewicz
        expected = [(0,0.7),(1,0.7),(2,0.5),(3,0.4),(4,0.2),(5,0.2),(6,0.1),(7,0.1)]
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
