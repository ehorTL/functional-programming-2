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

integrateWithEpsM f a b n eps prevEps = do
    let res1 = integrate f a b n
    let res2 = integrate f a b (n*2)
    let curEps = abs $ res2 - res1
    let evalEps = 0.3 * curEps
    putStrLn $ "res1: " ++ show res1 ++ "\n" ++
        "res2: " ++ show res2 ++ "\n" ++
        "curEps: " ++ show curEps ++ "\n\n"
    if (abs evalEps <= eps) then do
        putStrLn $ show res2 else 
            if (abs evalEps > prevEps) then do putStrLn $ show res1 else do
                integrateWithEpsM f a b (n*2) eps evalEps

integrateTest1 = integrate (\x -> x) 0 5 5

integrateTest2 = integrateWithEps (\x-> x) 0 5 5 0.000001

integrateTest3 = integrateWithEps sin 0 pi 100000 0.000001

integrateTest3M = integrateWithEpsM sin 0 pi 20 0.000001 100