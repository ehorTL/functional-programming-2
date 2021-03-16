module Integration where

import Control.Parallel

{-|
 Integration with trapezoidal rule (https://en.wikipedia.org/wiki/Trapezoidal_rule).
 Used uniform grid with n eqully spaced segments.
 One-threaded.
-}
integrate f a b n = let 
    dx = (b-a) / n
    sum acc lastInd curInd curXi
        | (curInd > lastInd) || (curXi >b-dx) = acc
        | otherwise = sum (acc + f curXi) lastInd (curInd+1) (curXi+dx)
    in dx * ((sum 0.0 (n-1) 1 (a+dx) ) + ((f b + f a) / 2))


{-|
    Sequential integration with Trapezoidal rule.
    Error is evaluated with Runge's rule.
    One-threaded.
-}
integrateWithEps f a b n eps = let
    integrateWithEpsInner f a b n eps prevEps = let
        res1 = integrate f a b n
        res2 = integrate f a b (n*2)
        curEps = abs $ res1 - res2
        in if (abs curEps <= eps) then res2 else 
            if (curEps > prevEps) then res1 else 
                integrateWithEpsInner f a b (n*2) eps curEps
    in integrateWithEpsInner f a b n eps 1000


{-|
    See 'integrateWithEps' function docs.
    Logging added.
    Parameter 'prevEps' should be set to a big number 
        [bigger than 'eps' (like 100)].
-}
integrateWithEpsM f a b n eps prevEps = do
    let res1 = integrate f a b n
    let res2 = integrate f a b (n*2)
    let curEps = abs $ res2 - res1
    let evalEps = 0.3 * curEps
    putStrLn $ "I1: " ++ show res1 ++ "\n" ++
        "I2: " ++ show res2 ++ "\n" ++
        "curEps: " ++ show curEps ++ "\n\n"
    if (abs evalEps <= eps) then do
        putStrLn $ show res2 else 
            if (abs evalEps > prevEps) then do putStrLn $ show res1 else do
                integrateWithEpsM f a b (n*2) eps evalEps


{-|
    Parallel version of 'integrateWithEps' function.
-}
integrateWithEpsPar f a b n eps = let
    integrateWithEpsInner f a b n eps prevEps = let
        res1 = integrate f a b n
        res2 = integrate f a b (n*2)
        curEps = par res1 (pseq res2 (abs $ res1 - res2))
        in if (abs curEps <= eps) then res2 else 
            if (curEps > prevEps) then res1 else 
                integrateWithEpsInner f a b (n*2) eps curEps
    in integrateWithEpsInner f a b n eps 1000
