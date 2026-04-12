{-# LANGUAGE FunctionalDependencies #-}

-- | Core type class and set operations shared by fuzzy sets and fuzzy relations.
module FuzzySet where

import Lattices.ResiduatedLattice
import Lattices.UnitIntervalStructures.Lukasiewicz
import qualified Data.List as SetOp(union, intersect)

-- | Common interface for fuzzy-set-like values.
--
-- The functional dependency states that a concrete set type determines both
-- its element type and the lattice used for truth degrees.
class (ResiduatedLattice l, Eq a) => FuzzySet set a l | set -> a l where
    -- | Construct a fuzzy set from a membership function and an explicit universe.
    mkFuzzySet :: (a -> l) -> [a] -> set
    -- | Query the membership degree of an element.
    member :: set -> a -> l
    -- | Return the explicit universe stored by the fuzzy set.
    universe :: set -> [a]
    -- | Collect all membership degrees in universe order.
    truthDegrees :: set -> [l]
    truthDegrees set = [member set x | x <- universe set]
    -- | Number of elements in the explicit universe.
    universeCardinality :: set -> Int
    universeCardinality s = length $ universe s


-- | Construct an empty fuzzy set whose membership is always 'bot'.
mkEmptySet :: (FuzzySet set a l) => set
mkEmptySet = mkFuzzySet (const bot) []

-- | Construct a fuzzy set with a single non-bottom element.
mkSingletonSet :: (FuzzySet set a l, Eq a) => [a] -> (a, l) -> set
mkSingletonSet u (x, l) = mkFuzzySet f u
    where
        f y
            | y == x = l
            | otherwise = bot

-- | Construct a fuzzy set that assigns 'top' to every universe element.
mkUniversalSet :: (FuzzySet set a l, Eq a) => [a] -> set
mkUniversalSet = mkFuzzySet (const top)


{- | list of all values from 'universe' that have ('member' u) >= alpha

==== __Examples__

>>> let set = fromList [(1, 0.1), (2, 0.2), (3, 0.4)]
>>> alphaCut 0.15 set
[2, 3]

>>> alphaCut 0.3 set
[3]

>>> alphaCut 0.5 set
[]
>>> alphaCut 1 (mkUniversalSet [1..10])
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
-}
alphaCut :: (FuzzySet set a l) => l -> set -> [a]
alphaCut alpha set = [x | x <- u, f x >= alpha]
    where f = member set
          u = universe set


{- | Fuzzy set union A union B. Universe of the new set is union of universes from A and B.

==== __Examples__

>>> let set1 = fromList [(1, 0.2), (2, 0.7), (3, 0.1)] :: LSet Int UILukasiewicz
>>> let set2 = fromList [(1, 0.3), (2, 0.4)] :: LSet Int UILukasiewicz
>>> let set3 = fromList [(1, 0.5), (2, 0.1), (4, 0.8)] :: LSet Int UILukasiewicz
>>> toList $ union set1 set2
[(1,0.3),(2,0.7),(3,0.1)]

>>> toList $ union set1 set3
[(1,0.5),(2,0.7),(3,0.1),(4,0.8)]

>>> toList $ union set1 mkEmptySet
[(1,0.2),(2,0.7),(3,0.1)]
-}
union :: (FuzzySet set a l) => set -> set -> set
union set1 set2 = mkFuzzySet (\x -> f x \/ g x) u
    where f = member set1
          g = member set2
          u = SetOp.union (universe set1) (universe set2)


-- | 'union' over a list of sets.
unions :: (FuzzySet set a l, Eq a) => [set] -> set
unions sets@(set:_) = foldr union (mkUniversalSet (universe set)) sets


{- | Fuzzy set intersection A intersection B. Universe of the new set is intersection of universes from A and B.

==== __Examples__

>>> let set1 = fromList [(1, 0.2), (2, 0.7), (3, 0.1)]
>>> let set2 = fromList [(1, 0.3), (2, 0.4)]
>>> let set3 = fromList [(1, 0.5), (2, 0.1), (4, 0.8)] :: LSet Int UILukasiewicz
>>> toList $ intersection set1 set2
[(1,0.2),(2,0.4)]

>>> toList $ intersection set1 set3
[(1,0.2),(2,0.1)]

>>> toList $ intersection set1 mkEmptySet
[]
-}
intersection :: (FuzzySet set a l) => set -> set -> set
intersection set1 set2 = mkFuzzySet (\x ->  f x /\ g x) u
    where f = member set1
          g = member set2
          u = SetOp.intersect (universe set1) (universe set2)

-- | 'intersection' over a list of sets.
intersections :: (FuzzySet set a l, Eq a) => [set] -> set
intersections = foldr intersection mkEmptySet

{- | Complement of a fuzzy set A'

==== __Examples__

>>> let set1 = fromList [(1, 0.2), (2, 0.7)] :: LSet Int UILukasiewicz
>>> toList $ complement set1
[(1,0.8),(2,0.3)]

>>> let set2 = fromList [(1, 1), (2, 1)]
>>> toList $ complement set2
[(1,0),(2,0)]
-}
complement :: (FuzzySet set a l) => set -> set
complement set = mkFuzzySet (negation . f)  (universe set)
    where f = member set


{- | Apply a t-norm operation over two fuzzy sets. Both sets should be defined on the same 'universe'.

==== __Examples__

>>> let set1 = fromList [(1, 0.2), (2, 0.7)] :: LSet Int UILukasiewicz
>>> let set2 = fromList [(1, 0.3), (2, 0.4)] :: LSet Int UILukasiewicz
>>> toList $ setTnorm set1 set2
[(1,0.2),(2,0.4)]
-}
setTnorm :: (FuzzySet set a l) => set -> set -> set
setTnorm set1 set2 = mkFuzzySet (\x -> f x `tnorm` g x) u
    where f = member set1
          g = member set2
          u = universe set1


{- | Apply a residuum operation over two fuzzy sets. Both sets should be defined on the same 'universe'.

==== __Examples__

>>> let set1 = fromList [(1, 0.2), (2, 0.7)] :: LSet Int UILukasiewicz
>>> let set2 = fromList [(1, 0.3), (2, 0.4)] :: LSet Int UILukasiewicz
>>> toList $ setResiduum set1 set2
[(1,1.0),(2,0.7)]
-}
setResiduum :: (FuzzySet set a l) => set -> set -> set
setResiduum set1 set2 = mkFuzzySet (\x -> f x --> g x) u
    where f = member set1
          g = member set2
          u = universe set1


{- | Modify the membership function of a fuzzy set by applying another function to its elements.

==== __Examples__

>>> let set = fromList [(1, 0.2), (2, 0.7)] :: LSet Int UILukasiewicz
>>> let modifiedSet = mapMembership set (\x -> x + 1)
>>> toList modifiedSet
[(1,0.0),(2,0.0)]
-}
mapMembership :: (FuzzySet set a l) => set -> (a -> a) -> set
mapMembership set g = mkFuzzySet (f . g) u
    where
        u = universe set
        f = member set


{- | Filter values of a fuzzy set based on a predicate.

==== __Examples__

>>> let set = fromList [(1, 0.2), (2, 0.7), (3, 0.4)] :: LSet Int UILukasiewicz
>>> let filteredSet = filterMembership set (\x -> x > 1)
>>> toList filteredSet
[(1,0.0),(2,0.7),(3,0.4)]
-}
filterMembership :: (FuzzySet set a l) => set -> (a -> Bool) -> set
filterMembership set pred =  mkFuzzySet h u
    where
        h x = if pred x then f x else bot
        f = member set
        u = universe set


{- | Modify the universe of a fuzzy set by applying a function to its elements.

==== __Examples__

>>> let set = fromList [(1, 0.2), (2, 0.7)] :: LSet Int UILukasiewicz
>>> let modifiedSet = mapU set (\x -> x * 2)
>>> toList modifiedSet
[(2,0.2),(4,0.7)]
-}
mapU :: (FuzzySet set a l) => set -> (a -> a) -> set
mapU set g = mkFuzzySet f u
    where
        f = member set
        u = map g (universe set)


{- | Filter the universe of a fuzzy set based on a predicate.

==== __Examples__

>>> let set = fromList [(1, 0.2), (2, 0.7), (3, 0.4)] :: LSet Int UILukasiewicz
>>> let filteredSet = filterU set (\x -> x > 1)
>>> toList filteredSet
[(2,0.7),(3,0.4)]
-}
filterU :: (FuzzySet set a l) => set -> (a -> Bool) -> set
filterU set pred = mkFuzzySet f u
    where
        f = member set
        u = filter pred (universe set)
