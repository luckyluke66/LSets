-- | Predicates and graded property measures for fuzzy sets.
--
-- This module provides both crisp Boolean checks such as emptiness and
-- singletonhood, and graded comparisons such as subsethood and equality in a
-- residuated lattice.
module Fuzzy.Sets.Properties (
    -- * Standard predicates 
    isEmpty,
    isSingleton,
    isCrisp,
    isUniversal,
    strictSubsethood,
    strictEquality,
    -- * Graded predicates
    gradedSubsethood,
    gradedEquality,
) where

import FuzzySet
import Fuzzy.Sets.LSet
import Lattices.ResiduatedLattice 
import FuzzySet

{- | True if 'fuzzySet' is empty, meaning for each `u` in 'universe', 
'member' u == 'bot'.

==== __Examples__

>>> let emptySet = mkEmptySet :: LSet Int UILukasiewicz
>>> isEmpty emptySet
True

>>> let nonEmptySet = fromList [(1, 0.2), (2, 0.0)] :: LSet Int UILukasiewicz
>>> isEmpty nonEmptySet
False
-}
isEmpty :: (FuzzySet set a l) => set -> Bool
isEmpty set = all (== bot) (truthDegrees set)


{- | 'fuzzySet' is a singleton if and only if there is exactly 
one `u` in 'universe' for which 'member' u is greater than 'bot'.

==== __Examples__

>>> let singletonSet = mkSingletonSet [1, 2, 3] (2, 0.8) :: LSet Int UILukasiewicz
>>> isSingleton singletonSet
True

>>> let nonSingletonSet = fromList [(1, 0.2), (2, 0.7)] :: LSet Int UILukasiewicz
>>> isSingleton nonSingletonSet
False
-}
isSingleton :: (FuzzySet set a l) => set -> Bool
isSingleton set = length (filter (> bot) (truthDegrees set)) == 1


{- | Check if fuzzy set is a crisp set (Truth value structure contains only two values: 0 and 1).

==== __Examples__

>>> let crispSet = fromList [(1, 1.0), (2, 0.0)] :: LSet Int UILukasiewicz
>>> isCrisp crispSet
True

>>> let nonCrispSet = fromList [(1, 0.5), (2, 0.7)] :: LSet Int UILukasiewicz
>>> isCrisp nonCrispSet
False
-}
isCrisp :: (FuzzySet set a l) => set -> Bool
isCrisp set = all (\x -> x == top || x == bot) [f x | x <- u]
    where
        f = member set
        u = universe set

{- | Fuzzy set is universal if it returns 'top' for every value in its universe. 
This means that the fuzzy set equals its universe.

==== __Examples__

>>> let universalSet = mkUniversalSet [1, 2, 3] :: LSet Int UILukasiewicz
>>> isUniversal universalSet
True

>>> let nonUniversalSet = fromList [(1, 1.0), (2, 0.8)] :: LSet Int UILukasiewicz
>>> isUniversal nonUniversalSet
False
-} 
isUniversal :: (FuzzySet set a l) => set -> Bool
isUniversal set = all (== top) [f x | x <- u]
    where 
        u = universe set
        f = member set


-- | Highest possible membership of a functions. 
-- It is always bound by the 'top' of the defining lattice.
height :: (FuzzySet set a l) => set -> l
height _ = top


{- | Support is a list of all elements with non 'bot' 'member' 

>>> let set = fromList [(1, 0), (2, 0.8), (3, 0), (4, 0.5), (6, 0)]
[2, 4] 
-}
support :: (FuzzySet set a l) => set -> [a]
support set = filter ((/=bot) . f) u
    where f = member set
          u = universe set 


{- | Core of a fuzzy set is 'alphaCut' where alpha = 'top'. 
In other words it's a list of items with 'member' equal to 'top'

==== __Examples__

>>> let set = fromList [(1, 1:0), (2, 0.8), (3, 1.0), (4, 0.5), (6, 1.0)]
>>> core set
[1, 3, 6]

>>> alphaCut 1.0 set == core set
True
-}
core :: (FuzzySet set a l) => set -> [a]
core = alphaCut top

{- | Is 'FuzzySet' A a strict subset of 'FuzzySet' B?
Membership of values from A is smaller than B for every item of universe

==== __Examples__

>>> let setA = fromList [(1, 0.2), (2, 0.5)] :: LSet Int UILukasiewicz
>>> let setB = fromList [(1, 0.3), (2, 0.7)] :: LSet Int UILukasiewicz
>>> strictSubsethood setA setB
True

>>> strictSubsethood setB setA
False
-}
strictSubsethood :: (FuzzySet set a l) => set -> set -> Bool
strictSubsethood set1 set2 = top == gradedSubsethood set1 set2


{- | Is 'FuzzySet' A strictly equal to 'FuzzySet' B? 
Member of all values from universe in set A is equal to member of all values in set B.

==== __Examples__

>>> let setA = fromList [(1, 0.2), (2, 0.5)] :: LSet Int UILukasiewicz
>>> let setB = fromList [(1, 0.2), (2, 0.5)] :: LSet Int UILukasiewicz
>>> strictEquality setA setB
True

>>> let setC = fromList [(1, 0.3), (2, 0.5)] :: LSet Int UILukasiewicz
>>> strictEquality setA setC
False
-}
strictEquality :: (FuzzySet set a l) => set -> set -> Bool
strictEquality set1 set2 = top == gradedEquality set1 set2 


{- | Degree to which set A is a subset of B.
If the result is 1, we can conclude that A ⊆ B.

==== __Examples__

>>> let setA = fromList [(1, 0.2), (2, 0.5)] :: LSet Int UILukasiewicz
>>> let setB = fromList [(1, 0.3), (2, 0.7)] :: LSet Int UILukasiewicz
>>> gradedSubsethood setA setB
1.0

>>> gradedSubsethood setB setA
0.8
-}
gradedSubsethood :: (FuzzySet set a l) => set -> set -> l
gradedSubsethood set1 set2 = foldr (/\) top $ zipWith (-->) (map f u) (map g u)
        where 
            f = member set1
            g = member set2
            u = universe set1


{- | Degree to which set A is equal to set B.
If the result is 1, the fuzzy sets are equal.

==== __Examples__

>>> let setA = fromList [(1, 0.2), (2, 0.5)] :: LSet Int UILukasiewicz
>>> let setB = fromList [(1, 0.2), (2, 0.5)] :: LSet Int UILukasiewicz
>>> gradedEquality setA setB
1.0

>>> let setC = fromList [(1, 0.3), (2, 0.5)] :: LSet Int UILukasiewicz
>>> gradedEquality setA setC
0.9
-}
gradedEquality :: (FuzzySet set a l) => set -> set -> l
gradedEquality set1 set2 = foldr (/\) top $ zipWith (<-->) (map f u) (map g u)
        where 
            f = member set1
            g = member set2
            u = universe set1


{- $Standard predicates
Predicate functions that take a fuzzy set and return true if fuzzy set has some property (crisp, empty...)
-}

{- $Graded Predicates determining if two Fuzzy sets are equal can be... well, fuzzy. 
in this section we introduce predicates that return graded values from 'ResiduatedLattice'
-}
