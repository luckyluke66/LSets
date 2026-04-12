-- | Lattice-valued truth structures used throughout the library.
--
-- This namespace collects the abstract lattice interfaces, the base unit
-- interval carrier, and the provided standard unit-interval structures.
module Lattices (
    module Lattices.ResiduatedLattice,
    module Lattices.UnitInterval,
    module Lattices.UnitIntervalStructures
) where

import Lattices.ResiduatedLattice
import Lattices.UnitInterval
import Lattices.UnitIntervalStructures
