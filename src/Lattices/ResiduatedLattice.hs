-- | Core lattice and residuated-lattice abstractions used throughout the library.
module Lattices.ResiduatedLattice(
    ResiduatedLattice(..),
    BoundedLattice(..),
    Nat
) where

-- | Alias used for exponents in repeated t-norm multiplication.
type Nat = Int

-- | Lattice is an algebraic structure with join and meet operations.
-- BoundedLattice has Top and Bottom elements.
-- Lattice satisfies following laws:
--
-- /Associativity/
--
-- @
-- x '\/' (y '\/' z) == (x '\/' y) '\/' z
-- x '/\\' (y '/\\' z) == (x '/\\' y) '/\\' z
-- @
--
-- /Commutativity/
--
-- @
-- x '\/' y == y '\/' x
-- x '/\\' y == y '/\\' x
-- @
--
-- /Idempotency/
--
-- @
-- x '\/' x == x
-- x '/\\' x == x
-- @
--
-- /Absorption/
--
-- @
-- a '\/' (a '/\\' b) == a
-- a '/\\' (a '\/' b) == a
-- @
class RealFrac l => BoundedLattice l where
    -- | Meet.
    (/\) :: l -> l -> l
    -- | Join.
    (\/) :: l -> l -> l
    -- | Greatest element.
    top :: l
    -- | Least element.
    bot :: l
    -- | Smart constructor for lattice values.
    mkLattice :: Double -> l

-- | A bounded lattice with additional laws and operations, namely residuum and t-norm.
--
-- /Commutativity/
--
-- @
-- x 'tnorm' y == y 'tnorm' x
-- @
--
-- /Associativity of 'tnorm'/
--
-- @
-- x 'tnorm' (y 'tnorm' z) == (x 'tnorm' y) 'tnorm' z
-- @
--
-- /Identity element/
--
-- @
-- x 'tnorm' 1 == x
-- @
--
-- /Adjointness/
--
-- @
-- x <= y '-->' z iff x 'tnorm' y <= z
-- @
class BoundedLattice l => ResiduatedLattice l where
    -- | Residuum.
    (-->), (<--) :: l -> l -> l
    a <-- b = b --> a
    -- | Biresiduum, usually interpreted as graded equivalence.
    (<-->) :: l -> l -> l
    a <--> b = (a --> b) /\ (b --> a)
    -- | Monoidal conjunction associated with the residuum.
    tnorm :: l -> l -> l
    -- | Derived negation induced by the residuum.
    negation :: l -> l
    negation a = a --> bot

    -- | Repeated t-norm multiplication.
    power :: l -> Nat -> l
    power a 0 = top
    power a n = a `tnorm` power a (n - 1)
