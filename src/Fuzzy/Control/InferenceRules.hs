module Fuzzy.Control.InferenceRules where

import Lattices.ResiduatedLattice
import Fuzzy.Sets.LSet
import FuzzySet
import Fuzzy.Sets.MembershipFunctions(constant)

-- | A fuzzy inference rule of the form:
--
-- > IF antecedent1 AND antecendent2 AND ... AND antecendentn THEN consequent
--
-- The antecedent and consequent are fuzzy sets over different universes.
--
-- @a@ is the input (crisp) universe
-- @b@ is the output universe
-- @l@ is the residuated lattice used for membership values
data (ResiduatedLattice l, Eq a, Eq b) => Rule a b l = Rule
    { antecedents :: [LSet a l]
      -- ^ Fuzzy set describing the condition (IF-part)

    , consequent :: LSet b l
      -- ^ Fuzzy set describing the result (THEN-part)
    }

-- | A rule base is a collection of fuzzy inference rules.
type RuleBase a b l = [Rule a b l]


-- | Scales a fuzzy set by an activation degree using a t-norm.
--
-- This corresponds to rule firing strength modulation:
--
-- > A(x)' = α ⊗ A(x)
--
-- where @α@ is the rule activation degree, ⊗ is the t-norm a A is the fuzzy set.
--
-- === Parameters
--
-- * @alpha@ - firing strength of the rule
-- * @set@   - consequent fuzzy set
--
-- === Returns
--
-- A new fuzzy set with reduced or unchanged membership values.
scale :: (ResiduatedLattice l, Eq a) => l -> LSet a l -> LSet a l
scale alpha set =  setTnorm set (fromFunction (constant alpha) (universe set))
    

-- | Evaluates a single fuzzy rule for a given input.
--
-- Steps:
--
-- 1. Compute rule activation degree:
--    @alpha = member antecedent x@
--
-- 2. Scale the consequent by this degree
--
-- === Example
--
-- @
-- evalRule 25 rule
-- -- returns a partially activated fuzzy set
-- @
evalRule:: (ResiduatedLattice l, Eq a, Eq b) => a -> Rule a b l -> LSet b l
evalRule x (Rule ant cons) =
    let alpha = member (aggregate ant) x
    in scale alpha cons


-- | Aggregates multiple fuzzy sets into one combined fuzzy set.
--
-- Uses fuzzy union.
--
-- === Behavior
--
-- This corresponds to combining contributions from multiple rules.
aggregate :: (ResiduatedLattice l, Eq a) => [LSet a l]-> LSet a l
aggregate = foldr union mkEmptySet


-- | Performs fuzzy inference over a rule base for a given input.
--
-- === Pipeline
--
-- 1. Evaluate each rule with 'evalRule'
-- 2. Aggregate all resulting fuzzy sets
--
-- === Result
--
-- A single fuzzy set over the output universe representing
-- the inferred fuzzy conclusion.
--
-- === Example
--
-- @
-- infer rules 25
-- -- fuzzy output set over b
-- @
infer :: (ResiduatedLattice l, Eq a, Eq b) => RuleBase a b l-> a -> LSet b l
infer rules x = aggregate (map (evalRule x) rules)