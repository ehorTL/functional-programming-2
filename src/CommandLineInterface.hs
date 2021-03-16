module CommandLineInterface (cli) where

import Text.Read

import Demo


cli :: IO ()
cli = do 
    putStrLn "Choose the function to integrate:\n1.sin(x)\n2.f(x)=x\n3.f(x)=x^2"
    n <- readMaybe <$> getLine :: IO (Maybe Int)
    case n of
        Nothing -> putStrLn "Exit."
        Just num -> do 
            if num == 1 then do
                putStrLn $ "Integral of sin(x) on [0, pi]:\n" ++ show integrateTest3 ++ "\n\n"
            else if num == 2 then do
                putStrLn $ "Integral of f(x)=x on [0, 5]:\n" ++ show integrateTest2 ++ "\n\n"
            else if num == 3 then do
                putStrLn $ "Integral of f(x)=x^2 on [1, 5]:\n" ++ show integrateTest4 ++ "\n\n"
            else do putStrLn "Exit"

