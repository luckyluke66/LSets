{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Concrete fuzzy sets represented by an explicit universe and membership function.
module Fuzzy.Sets.LSet(
    LSet(LSet),
    FuzzySet(member, universe),
    fromList,
    fromFunction,
    toList
) where

import Lattices.UnitInterval
import Lattices.ResiduatedLattice
import FuzzySet
import Data.Maybe(fromMaybe)

-- | A fuzzy set over elements of type @a@ with truth values in lattice @l@.
--
-- The constructor stores both the membership function and the explicit
-- universe on which the set is evaluated.
data (ResiduatedLattice l) => LSet a l = LSet
    {
    -- | Membership function assigning a truth degree to each element.
     membership :: a -> l
    -- | Explicit universe of elements considered by the set.
    , universe :: ![a]
    }


instance (Eq a, Show a, Show l, ResiduatedLattice l) => Show (LSet a l) where
    show :: LSet a l -> String
    show set = "FuzzySet { " ++ show (toList set) ++ " }"


instance (ResiduatedLattice l, Eq a) => FuzzySet (LSet a l) a l where
    member :: LSet a l -> a -> l
    member (LSet f _) = f
    universe :: LSet a l -> [a]
    universe (LSet _ u) = u
    mkFuzzySet :: (a -> l) -> [a] -> LSet a l
    mkFuzzySet = LSet


-- | Build a fuzzy set from explicit @(element, membership)@ pairs.
--
-- Elements not present in the list default to 'bot'. The universe is taken
-- from the first components of the supplied pairs.
fromList :: (ResiduatedLattice l, Eq a) => [(a, l)] -> LSet a l
fromList xs = LSet f u
    where
        f x = fromMaybe bot (lookup x xs)
        u = map fst xs


{- | Construct a fuzzy set from a membership function and a universe set

==== __Examples__

>>> let f x = if x == 1 then 0.8 else 0.3
>>> let set = fromFunction f [1, 2, 3] :: LSet Int UILukasiewicz
>>> toList set
[(1,0.8),(2,0.3),(3,0.3)]
-}
fromFunction :: (ResiduatedLattice l) => (a -> l) -> [a] -> LSet a l
fromFunction = LSet


-- | Convert a fuzzy set into its explicit list representation over the stored universe.
toList :: (ResiduatedLattice l, Eq a) => LSet a l -> [(a, l)]
toList (LSet f setUniverse) = [(u, f u) | u <- setUniverse]
