module Main where

import Fuzzy.Sets.LSet
import Fuzzy.Sets.FuzzyCardinality
import Lattices.UnitIntervalStructures.Lukasiewicz (UILukasiewicz(UILukasiewicz))

main :: IO ()
main = do 
    let set = fromList [("a", 0.1), ("b", 0.2), ("c", 0.1), ("d", 0.5), ("e", 0.7), ("f", 0.2), ("g", 0.4)] :: LSet String UILukasiewicz
        --count = fgCount set :: LSet String UILukasiewicz
        b = bracket 1 set
    print b