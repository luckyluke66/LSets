module Fuzzy.Sets.CardinalityTest (
    cardinalityTests
) where

import Test.Tasty
import Test.Tasty.HUnit
import Fuzzy.Sets.LSet
import Fuzzy.Sets.Cardinality
import Lattices.UnitIntervalStructures.Lukasiewicz
import Lattices.ResiduatedLattice
import Utils.Utils


cardinalityTests :: TestTree
cardinalityTests = testGroup "Cardinality Tests" [
    testCase "ralescuS numeric count" testRalescuS,
    testCase "sigmaCount for set and empty" testSigmaCount,
    testCase "thresholdSigmaCount behavior" testThresholdSigmaCount,
    testCase "normalizedSigmaCount behavior" testNormalizedSigmaCount,
    testCase "sigmaCountMod behavior" testSigmaCountMod,
    testCase "modifierFunction and derived modifiers" testModifierFunctions,
    testCase "alphaCutModifier boundary behavior" testAlphaCutModifier
    ]


whereSet :: LSet String UILukasiewicz
whereSet = fromList [("a", 0.1), ("b", 0.2), ("c", 0.1), ("d", 0.5), ("e", 0.7), ("f", 0.2), ("g", 0.4)]


testRalescuS :: Assertion
testRalescuS =
    assertEqual "ralescuS should count degrees > 0.5 plus boundary if equals 0.5"
        3
        (ralescuS whereSet)


testSigmaCount :: Assertion
testSigmaCount = do
    let set = fromList [("x", 0.2), ("y", 0.7), ("z", 0.5)] :: LSet String UILukasiewicz
        emptySet = mkEmptySet :: LSet Int UILukasiewicz
    assertApproxEqual "sigmaCount from standard set" (sigmaCount set) 1.4
    assertApproxEqual "sigmaCount empty set" (sigmaCount emptySet) 0.0


testThresholdSigmaCount :: Assertion
testThresholdSigmaCount = do
    let set = fromList [("x", 0.2), ("y", 0.7), ("z", 0.5)] :: LSet String UILukasiewicz
    assertApproxEqual "thresholdSigmaCount 0.5 should include 0.5 and 0.7" (thresholdSigmaCount 0.5 set) 1.2
    assertApproxEqual "thresholdSigmaCount 0.8 should yield 0" (thresholdSigmaCount 0.8 set) 0.0


testNormalizedSigmaCount :: Assertion
testNormalizedSigmaCount = do
    let set = fromList [("x", 0.2), ("y", 0.7), ("z", 0.5)] :: LSet String UILukasiewicz
        result = fromLukasiewiczUnitInterval $ normalizedSigmaCount set
    assertApproxEqual "normalizedSigmaCount should be sigmaCount / universe size" result (1.4 / 3)


testSigmaCountMod :: Assertion
testSigmaCountMod = do
    let set = fromList [("x", 0.2), ("y", 0.7), ("z", 0.5)] :: LSet String UILukasiewicz
    assertApproxEqual "sigmaCountMod identity equals sigmaCount" (sigmaCountMod identityModifier set) 1.4
    assertApproxEqual "sigmaCountMod sigmoid 2 (threshold 1)" (sigmaCountMod (sigmoidModifier 2 1.0) set) 0.78


testModifierFunctions :: Assertion
testModifierFunctions = do
    let mf = modifierFunction 2 2 0.5 :: UILukasiewicz -> UILukasiewicz
    assertApproxEqual "modifierFunction below threshold" (fromLukasiewiczUnitInterval $ mf 0.3) 0.18
    assertApproxEqual "modifierFunction above threshold" (fromLukasiewiczUnitInterval $ mf 0.7) 0.82

    let sm = sigmoidModifier 2 0.5 :: UILukasiewicz -> UILukasiewicz
    assertApproxEqual "sigmoidModifier below threshold" (fromLukasiewiczUnitInterval $ sm 0.3) 0.18
    assertApproxEqual "sigmoidModifier above threshold" (fromLukasiewiczUnitInterval $ sm 0.7) 0.82

    let im = identityModifier :: UILukasiewicz -> UILukasiewicz
    assertApproxEqual "identityModifier preserve small" (fromLukasiewiczUnitInterval $ im 0.3) 0.3
    assertApproxEqual "identityModifier preserve large" (fromLukasiewiczUnitInterval $ im 0.7) 0.7

    let sd = subDiagonalModifier 2 :: UILukasiewicz -> UILukasiewicz
    assertApproxEqual "subDiagonalModifier 2 for 0.3" (fromLukasiewiczUnitInterval $ sd 0.3) 0.09
    assertApproxEqual "subDiagonalModifier 2 for 0.7" (fromLukasiewiczUnitInterval $ sd 0.7) 0.49


testAlphaCutModifier :: Assertion
testAlphaCutModifier = do
    let ac = alphaCutModifier 0.5 :: UILukasiewicz -> UILukasiewicz
    assertApproxEqual "alphaCutModifier below threshold" (fromLukasiewiczUnitInterval $ ac 0.3) 0.0
    assertApproxEqual "alphaCutModifier at threshold" (fromLukasiewiczUnitInterval $ ac 0.5) 0.0
    assertApproxEqual "alphaCutModifier above threshold" (fromLukasiewiczUnitInterval $ ac 0.7) 1.0