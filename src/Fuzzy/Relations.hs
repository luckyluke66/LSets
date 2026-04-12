-- | Core functionality for fuzzy relations.
--
-- This namespace groups relation representations, basic constructors,
-- composition operators, elementary membership functions, and graded
-- relation properties.
module Fuzzy.Relations (
    module Fuzzy.Relations.LRelation,
    module Fuzzy.Relations.MembershipFunctions,
    module Fuzzy.Relations.RelationComposition,
    module Fuzzy.Relations.Properties
) where

import Fuzzy.Relations.LRelation
import Fuzzy.Relations.MembershipFunctions
import Fuzzy.Relations.RelationComposition
import Fuzzy.Relations.Properties
