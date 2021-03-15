module Integration where
-- module Integration (integrateWithEps) where

import Control.Parallel

-- https://en.wikipedia.org/wiki/Trapezoidal_rule
integrate f a b n = let 
    dx = (b-a) / n
    sum acc lastInd curInd curXi
        | (curInd > lastInd) || (curXi >b-dx) = acc
        | otherwise = sum (acc + f curXi) lastInd (curInd+1) (curXi+dx)
    in dx * ((sum 0.0 (n-1) 1 (a+dx) ) + ((f b + f a) / 2))

integrateWithEps f a b n eps = let
    integrateWithEpsInner f a b n eps prevEps = let
        res1 = integrate f a b n
        res2 = integrate f a b (n*2)
        curEps = abs $ res1 - res2 
        in if (abs curEps <= eps) then res2 else 
            if (curEps > prevEps) then res1 else 
                integrateWithEpsInner f a b (n*2) eps curEps
    in integrateWithEpsInner f a b n eps 1000


integrateTest1 = integrate (\x -> x) 0 5 5

integrateTest2 = integrateWithEps (\x-> x) 0 5 5 0.000001

integrateTest3 = integrateWithEps sin 0 pi 100000 0.000001