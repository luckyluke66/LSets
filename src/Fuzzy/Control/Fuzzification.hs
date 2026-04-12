module Fuzzy.Control.Fuzzification where

import Lattices.ResiduatedLattice
import Fuzzy.Sets.LSet

-- | A linguistic variable in fuzzy logic.
--
-- A linguistic variable represents a concept (e.g. "temperature")
-- described by a set of fuzzy linguistic terms (e.g. "cold", "warm", "hot").
--
-- Each term is a fuzzy set over the same universe of discourse.
--
-- @a@ is the type of the universe (e.g. Double, Int, etc.)
-- @l@ is the underlying residuated lattice used for membership values.
data (ResiduatedLattice l, Eq a) => LinguisticVariable a l = LV
    { name  :: String
      -- ^ Name of the linguistic variable (e.g. "temperature")

    , terms :: [(String, LSet a l)]
      -- ^ Named fuzzy sets representing linguistic terms
      --   (e.g. ("cold", coldSet), ("warm", warmSet))
    }


-- | Safely construct a linguistic variable.
--
-- Ensures all fuzzy sets share the same universe of discourse.
--
-- === Example
--
-- @
-- mkSafeLinguisticVariable "temperature"
--   [ ("cold", coldSet)
--   , ("warm", warmSet)
--   , ("hot", hotSet)
--   ]
-- @
--
-- === Safety
--
-- Returns a Left error if:
--   * The list of terms is empty
--   * Not all fuzzy sets share the same universe
--
-- === Returns
--
-- * @Right LinguisticVariable@ if valid
-- * @Left String@ describing the inconsistency otherwise
mkSafeLinguisticVariable
  :: (ResiduatedLattice l, Eq a)
  => String
  -> [(String, LSet a l)]
  -> Either String (LinguisticVariable a l)
mkSafeLinguisticVariable name terms =
  case terms of
    [] -> Left "mkSafeLinguisticVariable: empty term list"
    ((_, firstSet):rest) ->
      let baseUniverse = universe firstSet
          mismatches =
            [ termName
            | (termName, set) <- rest
            , universe set /= baseUniverse
            ]
      in if null mismatches
           then Right (LV name terms)
           else Left $
             "mkSafeLinguisticVariable: inconsistent universes in terms: "
             ++ show mismatches


-- | Unsafe constructor for a linguistic variable.
--
-- This function will crash at runtime if the input is invalid.
-- It is intended only for quick prototyping or trusted input.
--
-- Prefer 'mkSafeLinguisticVariable'.
mkUnsafeLinguisticVariable
  :: (ResiduatedLattice l, Eq a)
  => String
  -> [(String, LSet a l)]
  -> LinguisticVariable a l
mkUnsafeLinguisticVariable name terms =
    case mkSafeLinguisticVariable name terms of
        Left err -> error err
        Right lv -> lv


-- | Extracts the universe of discourse from a linguistic variable.
--
-- Assumes all terms share the same universe (guaranteed by safe constructor).
universeLv :: (ResiduatedLattice l, Eq a) => LinguisticVariable a l -> [a]
universeLv (LV _ ((_, firstSet):_)) = universe firstSet


-- | Extracts the names of all linguistic terms.
--
-- === Example
--
-- @
-- termNames temperature
-- -- ["cold","warm","hot"]
-- @
termNames :: (ResiduatedLattice l, Eq a) => LinguisticVariable a l -> [String]
termNames (LV _ terms) = map fst terms


-- | Fuzzifies a crisp input value into a fuzzy distribution over terms.
--
-- For each linguistic term, computes the membership degree of @x@
-- in the corresponding fuzzy set.
--
-- === Example
--
-- @
-- fuzzify 22 temperature
-- -- fromList [("cold",0.1),("warm",0.8),("hot",0.0)]
-- @
--
-- === Output
--
-- A fuzzy set over term names, where each membership value represents
-- how strongly the input belongs to that linguistic term.
fuzzify :: (ResiduatedLattice l, Eq a) => a-> LinguisticVariable a l -> LSet String l
fuzzify x (LV _ terms) =
    fromList [(name, member set x) | (name, set) <- terms]