{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}

-- | Concrete fuzzy relations represented as fuzzy sets over pairs.
module Fuzzy.Relations.LRelation (
    LRelation(LRelation),
    FuzzySet(..),
    fromList,
    fromFuzzySet,
    fromFunction,
    toList
) where

import Lattices.ResiduatedLattice
import Data.Maybe
import FuzzySet
import Data.List(foldl')
import Utils.Utils (universeToList, listToUniverse)

-- | A binary fuzzy relation over a universe of ordered pairs.
--
-- The constructor stores both the membership function and the explicit pair
-- universe on which the relation is evaluated.
data (ResiduatedLattice l, Eq a) => LRelation a l = LRelation
    { -- | Membership function of the relation.
      membership :: (a, a) -> l
    -- | Explicit universe of ordered pairs.
    , universe :: ![(a, a)]
    }

instance (Eq a, Show a, Show l, ResiduatedLattice l) => Show (LRelation a l) where
    show :: LRelation a l -> String
    show (LRelation r u) =
        let memberships = [(p, r p) | p <- u]
        in  "LRelation {\n"
        ++ "  Memberships: " ++ show memberships ++ "\n"
        ++ "\n}"

instance (Eq a, ResiduatedLattice l) => FuzzySet (LRelation a l) (a, a) l where
    member :: LRelation a l -> (a, a) -> l
    member (LRelation r _) = r
    universe :: LRelation a l -> [(a, a)]
    universe (LRelation _ u) = u
    mkFuzzySet :: ((a, a) -> l) -> [(a, a)] -> LRelation a l
    mkFuzzySet = LRelation


{- | Construct a fuzzy relation from a fuzzy set.

==== __Examples__

>>> let fuzzySet = fromList [((1, 2), 0.5), ((2, 3), 0.8)] :: LSet (Int, Int) UILukasiewicz
>>> let rel = fromFuzzySet fuzzySet
>>> toList rel
[((1,2),0.5),((2,3),0.8)]
-}
fromFuzzySet :: (FuzzySet f (a, a) l, ResiduatedLattice l, Eq a) => f -> LRelation a l
fromFuzzySet fuzzySet = LRelation (member fuzzySet) (FuzzySet.universe fuzzySet)


-- | Construct a relation from explicit @((x, y), degree)@ pairs.
--
-- Missing pairs default to 'bot'. The carrier set is inferred from all values
-- appearing in the supplied pairs.
fromList :: (ResiduatedLattice l, Eq a) => [((a, a), l)] -> LRelation a l
fromList lst = LRelation member (listToUniverse u)
    where
        member (x, y) = fromMaybe bot (lookup (x, y) lst)
        u = universeToList (map fst lst)


{- | Construct a fuzzy relation from a membership function and a carrier set.

==== __Examples__

>>> let f (x, y) = if x < y then 0.7 else 0.3
>>> let rel = fromFunction f [1, 2, 3] :: LRelation Int UILukasiewicz
>>> toList rel
[((1,1),0.3),((1,2),0.7),((1,3),0.7),((2,1),0.3),((2,2),0.3),((2,3),0.7),((3,1),0.3),((3,2),0.3),((3,3),0.3)]
-}
fromFunction :: (ResiduatedLattice l, Eq a) => ((a, a) -> l) -> [a] -> LRelation a l
fromFunction f u = LRelation f (listToUniverse u)


-- | Convert a relation into an explicit list of pair memberships.
toList :: (ResiduatedLattice l, Eq a) => LRelation a l -> [((a, a), l)]
toList (LRelation f u) = [(x, f x) | x <- u]



-- | Convert the relation as a matrix indexed by its universe. Only possible if universe is numeric.
toMatrix :: (ResiduatedLattice l, Show l) => LRelation Double l -> [[l]]
toMatrix rel =
    foldl' insert empty xs
    where
        xs = toList rel
        n = length $ universeToList (map fst xs)
        empty = replicate n (replicate n bot)

        insert m ((r,c), v) =
            let ri = round r - 1
                ci = round c - 1
            in take ri m
                ++ [replace ci v (m !! ri)]
                ++ drop (ri+1) m

        replace i x row =
            take i row ++ [x] ++ drop (i+1) row

-- | Create a relation from adjacency matrix with specified list of values.
fromMatrix :: (ResiduatedLattice l, Show l) => [[l]] -> [Double] -> LRelation Double l
fromMatrix matrix vals =
    fromList [((i, j), val) | (i, row) <- zip vals matrix, (j, val) <- zip vals row]