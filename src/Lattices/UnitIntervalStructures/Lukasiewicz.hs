{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Lukasiewicz operations on the unit interval.
module Lattices.UnitIntervalStructures.Lukasiewicz(
    UILukasiewicz(UILukasiewicz),
    BoundedLattice(..),
    mkLukasiewiczUnitInterval,
    fromLukasiewiczUnitInterval
) where

import Lattices.ResiduatedLattice
import Lattices.UnitInterval

-- | Unit-interval truth values equipped with Lukasiewicz operations.
newtype UILukasiewicz = UILukasiewicz UnitInterval
    deriving (Eq, Ord, Num, Real, RealFrac, Fractional)

instance BoundedLattice UILukasiewicz where
    (/\) (UILukasiewicz x) (UILukasiewicz y) = UILukasiewicz (x /\ y)
    (\/) (UILukasiewicz x) (UILukasiewicz y) = UILukasiewicz (x \/ y)
    bot = UILukasiewicz bot
    top = UILukasiewicz top
    mkLattice = mkLukasiewiczUnitInterval

-- | Lukasiewicz residuated-lattice structure.
instance ResiduatedLattice UILukasiewicz where
    tnorm a b = (a + b - top) \/ bot
    a --> b = (top - a + b) /\ top

instance Show UILukasiewicz where
    show (UILukasiewicz x) = show x

-- | Smart constructor for Lukasiewicz truth values.
mkLukasiewiczUnitInterval :: Double -> UILukasiewicz
mkLukasiewiczUnitInterval x = UILukasiewicz $ mkUnitInterval x

-- | Extract the underlying 'Double' value.
fromLukasiewiczUnitInterval :: UILukasiewicz -> Double
fromLukasiewiczUnitInterval (UILukasiewicz (UnitInterval x)) = x
