-- | Simple membership functions for building fuzzy relations.
module Fuzzy.Relations.MembershipFunctions (
    isCloseTo
) where

import Lattices.ResiduatedLattice

-- | Symmetric closeness relation on real numbers.
--
-- Values closer than distance @1@ receive linearly decreasing membership,
-- while larger distances map to 'bot'.
isCloseTo :: ResiduatedLattice l => (Double, Double) -> l
isCloseTo (x, y) = mkLattice $ max 0 (1 - abs (x - y))
