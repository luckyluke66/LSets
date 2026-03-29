module Main (main) where
import Test.Tasty
import Lattices.UnitIntervalStructures.Godel
import Lattices.ResiduatedLattice
import Lattices.UnitInterval
import UnitIntervalStructuresTest
import Fuzzy.Sets.LSetTest 
import Fuzzy.Sets.FuzzyCardinalityTest
import Fuzzy.Sets.PropertiesTest 
import Fuzzy.Sets.MembershipFunctionsTest
import Fuzzy.Relations.LRelationTest
import Fuzzy.Relations.PropertiesTest
import Fuzzy.Control.DefuzzificationTest

-- Run all tests
main :: IO ()
main = defaultMain $ testGroup "All Tests" [
    godelTests,
    lukasiewiczTests,
    productTests,
    lsetTests,
    fuzzyCardinalityTests,
    membershipFunctionsTests,
    propertiesTests,
    lrelationTests,
    relPropertiesTests,
    defuzzificationTests
    ]