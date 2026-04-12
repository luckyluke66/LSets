-- | Standard residuated-lattice structures built on the real unit interval.
--
-- The library currently provides Godel, Lukasiewicz, and Product structures.
module Lattices.UnitIntervalStructures (
    module Lattices.UnitIntervalStructures.Godel,
    module Lattices.UnitIntervalStructures.Lukasiewicz,
    module Lattices.UnitIntervalStructures.Product
) where

import Lattices.UnitIntervalStructures.Godel
import Lattices.UnitIntervalStructures.Lukasiewicz
import Lattices.UnitIntervalStructures.Product
