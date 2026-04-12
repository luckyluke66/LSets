-- | Core functionality for fuzzy sets.
--
-- This namespace groups together concrete set types, common membership
-- functions, cardinality measures, and graded set properties.
module Fuzzy.Sets (
    module Fuzzy.Sets.LSet,
    module Fuzzy.Sets.MembershipFunctions,
    module Fuzzy.Sets.Cardinality,
    module Fuzzy.Sets.FuzzyCardinality,
    module Fuzzy.Sets.Properties
) where

import Fuzzy.Sets.LSet
import Fuzzy.Sets.MembershipFunctions
import Fuzzy.Sets.Cardinality
import Fuzzy.Sets.FuzzyCardinality
import Fuzzy.Sets.Properties
