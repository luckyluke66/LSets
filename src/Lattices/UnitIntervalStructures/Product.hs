{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Product (Goguen) operations on the unit interval.
module Lattices.UnitIntervalStructures.Product(
    UIProduct(UIProduct),
    BoundedLattice(..),
    mkProductUnitInterval,
    fromProductUnitInterval
) where

import Lattices.ResiduatedLattice
import Lattices.UnitInterval

-- | Unit-interval truth values equipped with product (Goguen) operations.
newtype UIProduct = UIProduct UnitInterval
    deriving (Eq, Ord, Num, Real, RealFrac, Fractional)

instance BoundedLattice UIProduct where
    (/\) (UIProduct x) (UIProduct y) = UIProduct (x /\ y)
    (\/) (UIProduct x) (UIProduct y) = UIProduct (x \/ y)
    bot = UIProduct bot
    top = UIProduct top
    mkLattice = mkProductUnitInterval

-- | Product (Goguen) residuated-lattice structure.
instance ResiduatedLattice UIProduct where
    tnorm x y = x * y
    a --> b
        | a <= b = top
        | otherwise = b / a

instance Show UIProduct where
    show (UIProduct x) = show x

-- | Smart constructor for product truth values.
mkProductUnitInterval :: Double -> UIProduct
mkProductUnitInterval x = UIProduct $ mkUnitInterval x

-- | Extract the underlying 'Double' value.
fromProductUnitInterval :: UIProduct -> Double
fromProductUnitInterval (UIProduct (UnitInterval x)) = x
