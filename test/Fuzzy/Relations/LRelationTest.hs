{-# LANGUAGE FlexibleContexts #-}

module Fuzzy.Relations.LRelationTest (
    lrelationTests
) where

import Test.Tasty
import Test.Tasty.HUnit
import Fuzzy.Relations.LRelation
import FuzzySet
import Lattices.UnitIntervalStructures.Lukasiewicz

lrelationTests :: TestTree
lrelationTests = testGroup "LRelation Tests" [
    testCase "fromList creates correct LRelation" testFromList,
    testCase "fromFunction creates correct LRelation" testFromFunction,
    testCase "mkEmptySet creates empty LRelation" testMkEmptySet,
    testCase "mkSingletonSet creates singleton LRelation" testMkSingletonSet,
    testCase "mkUniversalSet creates universal LRelation" testMkUniversalSet,
    testCase "member retrieves correct membership value" testMember,
    testCase "universe retrieves correct universe" testUniverse
    ]

-- Test fromList
testFromList :: Assertion
testFromList = do
    let lst = [((1, 1), mkLattice 0.8), ((1, 2), mkLattice 0.4), ((2, 1), mkLattice 0.6)]
    let rel = fromList lst :: LRelation Int UILukasiewicz
    assertEqual "Membership (1,1)" (mkLattice 0.8) (member rel (1, 1))
    assertEqual "Membership (1,2)" (mkLattice 0.4) (member rel (1, 2))
    assertEqual "Membership (2,1)" (mkLattice 0.6) (member rel (2, 1))
    assertEqual "Membership (2,2)" bot (member rel (2, 2))

-- Test fromFunction
testFromFunction :: Assertion
testFromFunction = do
    let f (x, y) = if x == y then top else bot
    let u = [1, 2]
    let rel = fromFunction f u :: LRelation Int UILukasiewicz
    assertEqual "Membership (1,1)" (mkLattice 1.0) (member rel (1, 1))
    assertEqual "Membership (1,2)" (mkLattice 0.0) (member rel (1, 2))

-- Test mkEmptySet
testMkEmptySet :: Assertion
testMkEmptySet = do
    let rel = mkEmptySet :: LRelation Int UILukasiewicz
    assertEqual "Membership (1,1)" bot (member rel (1, 1))
    assertEqual "Universe is empty" [] (universe rel)

-- Test mkSingletonSet
testMkSingletonSet :: Assertion
testMkSingletonSet = do
    let u = [(1, 1), (1, 2), (2, 1), (2, 2)]
    let rel = mkSingletonSet u ((1, 1), mkLattice 0.8) :: LRelation Int UILukasiewicz
    assertEqual "Membership (1,1)" (mkLattice 0.8) (member rel (1, 1))
    assertEqual "Membership (1,2)" bot (member rel (1, 2))

-- Test mkUniversalSet
testMkUniversalSet :: Assertion
testMkUniversalSet = do
    let u = [(1, 1), (1, 2), (2, 1), (2, 2)]
    let rel = mkUniversalSet u :: LRelation Int UILukasiewicz
    assertEqual "Membership (1,1)" top (member rel (1, 1))
    assertEqual "Membership (1,2)" top (member rel (1, 2))

-- Test member
testMember :: Assertion
testMember = do
    let lst = [((1, 1), mkLattice 0.8), ((1, 2), mkLattice 0.4)]
    let rel = fromList lst :: LRelation Int UILukasiewicz
    assertEqual "Membership (1,1)" (mkLattice 0.8) (member rel (1, 1))
    assertEqual "Membership (2,2)" bot (member rel (2, 2))

-- Test universe
testUniverse :: Assertion
testUniverse = do
    let lst = [((1, 1), mkLattice 0.8), ((1, 2), mkLattice 0.4)]
    let rel = fromList lst :: LRelation Int UILukasiewicz
    assertEqual "Universe" [(1, 1), (1, 2), (2, 1), (2, 2)] (universe rel)