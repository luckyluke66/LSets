{- | This module contains parametrized membership function 
    use currying to construct the functions 
    arguments a b c ... are parameters for constructing specific functions 
    x is the variable for which membership is evaluated
-}

module Fuzzy.Sets.MembershipFunctions(
    constant, 
    linear, 
    sigmoid, 
    triangular,
    rectangular,
    trapezoidal,
    gaussian,
    exponential,
) where


import Lattices.ResiduatedLattice


{- | Constant function that returns `a` for any value of `x` \[f(x) = a\]

==== __Examples__

>>> let f = constant 0.5 :: Double -> UILukasiewicz
>>> f 1
0.5

>>> f 100
0.5
-}
constant :: ResiduatedLattice l => Double -> (Double -> l)
constant a _ = mkLattice a


{-| Standart textbook linear function where \[f(x) = ax + b\]

>           / 
>          /   
>         /

==== __Examples__

>>> let f = linear 2 1 :: Double -> UILukasiewicz
>>> f 0
1.0

>>> f 1
3.0

>>> f (-1)
-1.0
-}
linear :: ResiduatedLattice l => Double -> Double -> (Double -> l)
linear a b x =
    let y = a * x + b
    in mkLattice y


{-| Standart logistic function
Takes K which is growth value of the function and x0 a midpoint of the function. \[f(x) = \frac{1}{1 + e^{ -k(x - x_0)}} \]
               
>                       ______
>                    .´      
>                  /           
>           _____.'       

==== __Examples__

>>> let f = sigmoid 1 0 :: Double -> UILukasiewicz
>>> f 0
0.5

>>> f 1
0.7310585786300049

>>> f (-1)
0.2689414213699951
-}
sigmoid :: ResiduatedLattice l => Double -> Double -> (Double -> l)
sigmoid k x0 x =
    let pow = -k * (x - x0)
    in mkLattice $ 1 / (1 + (exp 1 ** pow))

{-| A combination of two linear functions
with this specific shape. first and second arguments are interval determining where the triangle will be on the number line. \[
\operatorname{tri}(x) =
\begin{cases}
    \frac{x - a}{b - a}, & a \leq x < b \\
    \frac{c - x}{c- b}, & b \leq x \leq c \\
    0, & \text{otherwise}
\end{cases}
\] 0 stands for 'bot' 

>          
>             /\
>          __/  \__

==== __Examples__

>>> let f = triangular 0 1 2 :: Double -> UILukasiewicz
>>> f 0
0.0

>>> f 0.5
0.5

>>> f 1
1.0

>>> f 1.5
0.5

>>> f 2
0.0

-}
triangular :: ResiduatedLattice l => Double -> Double -> Double -> (Double -> l)
triangular a b c x 
    | a <= x && x < b = mkLattice $ (x - a) / (b - a)
    | b <= x && x < c = mkLattice $ (c - x) / (c - b)
    | otherwise       = bot
    


{-| Constant function on interval [a, b], first two arguments, else returns 'bot' of Residuated lattice
- this creates a rectangle shaped function. Third argument is height of the set.

>             _____ 
>            |     |
>         ___|     |___


>>> let f = rectangular 1 3 0.8 :: Double -> UILukasiewicz
>>> f 0
0.0

>>> f 2
0.8

>>> f 3
0.8

>>> f 4
0.0
-}
rectangular :: ResiduatedLattice l => Double -> Double -> Double -> (Double -> l)
rectangular a b h x
    | x >= a && x <= b = mkLattice h
    | otherwise        = bot


{-| Trapezoidal function is combination of triangular and rectangular functions \[
\operatorname{tra}(x) =
\begin{cases}
    \frac{x - a}{b1 - a}, & a \leq x < b1 \\
    1, & b1 \leq x < b2  \\
    \frac{c - x}{c -b2}, & b2 \leq x < c \\
    0, & \text{otherwise}
\end{cases}
\]

>           _______   
>          /       \
>       __/         \__

==== __Examples__

>>> let f = trapezoidal 0 1 3 4 :: Double -> UILukasiewicz
>>> f 0
0.0

>>> f 0.5
0.5

>>> f 1
1.0

>>> f 2
1.0

>>> f 3.5
0.5

>>> f 4
0.0
-}
trapezoidal :: ResiduatedLattice l => Double -> Double -> Double -> Double -> (Double -> l)
trapezoidal a b1 b2 c x 
    | a <= x && x < b1   = mkLattice $ (x - a) / (b1 - a)
    | b1 <= x && x <= b2 = top
    | b2 <= x && x < c   = mkLattice $ (c - x) / (c - b2)
    | otherwise = bot


{-| Gausian function, also called Bell Curve

>           
>                     .-' `-.
>                   .'       `.
>                  /           \
>                /               \
>        ______.'                 '.____

==== __Examples__

>>> let f = gaussian 1 0 1 :: Double -> UILukasiewicz
>>> f 0
1.0

>>> f 1
0.6065306597126334

>>> f (-1)
0.6065306597126334

>>> f 2
0.1353352832366127
-}
gaussian :: ResiduatedLattice l => Double -> Double -> Double -> Double -> l 
gaussian a b c x = 
    mkLattice $ a * exp ( - ((x - b) ^ 2) / (2 * c ^ 2) )


{-| Exponential function eˣ

>           
>                    |
>                   '  
>                  /   
>                 /     
>        ______.'   

==== __Examples__

>>> let f = exponential :: Double -> UILukasiewicz
>>> f 0
1.0

>>> f 1
2.718281828459045

>>> f (-1)
0.36787944117144233
-}
exponential :: ResiduatedLattice l => Double -> l
exponential x = 
    let e = exp 1
    in mkLattice $ e**x