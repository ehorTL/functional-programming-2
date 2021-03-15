module CommandLineInterface (cli) where

import Text.Read

import Integration
-- import System.Eval.Haskell

cli :: IO ()
cli = do 
    putStrLn "Choose the function to integrate:\n1.sin(x)\n2.f(x)=x^2"

