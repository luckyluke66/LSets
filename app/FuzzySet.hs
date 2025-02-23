{-# LANGUAGE FunctionalDependencies #-}

module FuzzySet(FuzzySet(..)) where

import Lattices.ResiduatedLattice


class (ResiduatedLattice l) => FuzzySet set a l | set -> a l where
    member :: set -> a -> l
    universe :: set -> [a]
    universeCardinality :: set -> Int
    universeCardinality s = length $ universe s
    elements :: set -> [l]
    elements s = map (member s) (universe s)
