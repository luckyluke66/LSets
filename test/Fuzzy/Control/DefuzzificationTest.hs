module Fuzzy.Control.DefuzzificationTest (
    defuzzificationTests
) where

import Test.Tasty
import Test.Tasty.HUnit
import Fuzzy.Control.Defuzzification
import Fuzzy.Sets.LSet
import FuzzySet
import Fuzzy.Sets.Cardinality
import Lattices.UnitIntervalStructures.Godel


sampleSet :: LSet Double UIGodel
sampleSet = fromList [(1, 0.2), (2, 0.8)]


emptySet :: LSet Double UIGodel
emptySet = mkEmptySet


defuzzificationTests :: TestTree
defuzzificationTests = testGroup "Defuzzification Tests" [
    testCase "centerOfGravity on non-empty set" $
        assertEqual "centroid correct" 1.8 (centerOfGravity sampleSet),
    testCase "centerOfGravity on empty set" $
        assertEqual "empty set yields 0" 0.0 (centerOfGravity emptySet),
    testCase "centerOfMaxima uses universe bounds" $
        assertEqual "midpoint between min and max" 1.5 (centerOfMaxima sampleSet),
    testCase "meanOfMaximaMod with identity modifier" $
        assertEqual "normalized sigma count" 0.5 (meanOfMaximaMod id sampleSet),
    testCase "meanOfMaximaMod with alpha-cut modifier" $
        assertEqual "alpha-cut count over universe size" 0.5 (meanOfMaximaMod (alphaCutModifier 0.5) sampleSet),
    testCase "maxMembership on non-empty set" $
        assertEqual "highest membership element" 2.0 (maxMembership sampleSet),
    testCase "maxMembership on empty set" $
        assertEqual "empty set returns 0" 0.0 (maxMembership emptySet)
    ]
