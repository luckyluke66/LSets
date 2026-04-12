{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The closed real unit interval used as the base carrier for truth values.
module Lattices.UnitInterval(
    UnitInterval(..),
    mkUnitInterval
) where

import Lattices.ResiduatedLattice

-- | Real numbers restricted to the closed interval @[0, 1]@.
newtype UnitInterval = UnitInterval Double
    deriving (Eq, Ord, Num, Real, RealFrac, Fractional)

instance BoundedLattice UnitInterval where
    (/\) = min
    (\/) = max
    top = 1
    bot = 0
    mkLattice = mkUnitInterval

instance Show UnitInterval where
    show (UnitInterval x) = show x

-- | Smart constructor that clamps values into the interval @[0, 1]@.
mkUnitInterval :: Double -> UnitInterval
mkUnitInterval x = UnitInterval $ max 0 $ min x 1
