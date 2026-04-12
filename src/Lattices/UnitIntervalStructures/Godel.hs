{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

-- | Godel operations on the unit interval.
module Lattices.UnitIntervalStructures.Godel(
    UIGodel(UIGodel),
    BoundedLattice(..),
    mkGodelUnitInterval,
    fromGodelUnitInterval
) where

import Lattices.ResiduatedLattice
import Lattices.UnitInterval

-- | Unit-interval truth values equipped with Godel operations.
newtype UIGodel = UIGodel UnitInterval
    deriving (Eq, Ord, Num, Real, RealFrac, Fractional)

instance BoundedLattice UIGodel where
    (/\) :: UIGodel -> UIGodel -> UIGodel
    (/\) (UIGodel x) (UIGodel y) = UIGodel (x /\ y)
    (\/) (UIGodel x) (UIGodel y) = UIGodel (x \/ y)
    bot = UIGodel bot
    top = UIGodel top
    mkLattice = mkGodelUnitInterval

-- | Godel residuated-lattice structure.
instance ResiduatedLattice UIGodel where
    tnorm x y = x /\ y
    (-->)  = godelResiduum

instance Show UIGodel where
    show (UIGodel x) = show x

-- | Smart constructor for Godel truth values.
mkGodelUnitInterval :: Double -> UIGodel
mkGodelUnitInterval x = UIGodel $ mkUnitInterval x

-- | Extract the underlying 'Double' value.
fromGodelUnitInterval :: UIGodel -> Double
fromGodelUnitInterval (UIGodel (UnitInterval x)) = x

-- | Residuum induced by the Godel t-norm.
godelResiduum :: UIGodel -> UIGodel -> UIGodel
godelResiduum x y
    | x <= y = top
    | otherwise = y
