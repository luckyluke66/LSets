module FuzzyRelations.FuzzyRelation (
    FuzzyRelation(FuzzyRelation),
    membership,
    alphaCut
) where

import Lattices.ResiduatedLattice


data (ResiduatedLattice l) => FuzzyRelation a l =  FuzzyRelation
    { membershipFunction :: (a, a) -> l
    , universeSet :: ![a]
    , eq :: a -> a -> l
    }
    -- normally fuzzy relation is function R: X x Y -> L 
    -- but we can create any type U =  X | Y
    -- this way we can represent the relation with one universal set
    -- so we have R: U x U -> L

membership :: (ResiduatedLattice l) => FuzzyRelation a l -> (a, a) -> l
membership (FuzzyRelation f _ _) = f

equality :: (ResiduatedLattice l) => (a -> l) -> a -> a -> l
equality f x y = f x <--> f y

alphaCut :: (ResiduatedLattice l) => l -> FuzzyRelation a l -> [(a, a)]
alphaCut alpha (FuzzyRelation f u _) = [(x, y) | x <- u, y <- u, f (x, y) >= alpha]