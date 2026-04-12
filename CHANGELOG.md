# Revision history for fuzzySets

## 1.0.0 -- 2025-04-17

* First version.

## 1.0.1 -- 2025-05-01
* Changed constrains for dependencies in .cabal file to allow for more flexibility in package versions.

## 1.0.2 -- 2025-03-28
* Added deffuzzification function to the library, allowing users to convert fuzzy sets (Lset Double l) back into numbers. 
* This update is prepared for the next major release, which will include fuzzy control systems, if than rules and more.

## 1.1.0 -- 2025-04-11
* Added new functions for computing cardinality of fuzzy sets.
* Fixed some minor bugs in the implementation of defuzzification functions. 
* Updated documentation.
* Added tutorial modules to get started with the library.
* Added toMatrix and fromMatrix functions to convert between fuzzy sets and matrices.
* Added fuzzification functions to convert crisp values into fuzzy sets.
* Added support for linguistic variables and terms, allowing users to define fuzzy sets based on natural language descriptions.
* Added inference engine for fuzzy control systems, enabling users to create fuzzy rules and evaluate them based on input data.
