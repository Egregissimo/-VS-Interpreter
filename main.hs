module Main where
-- To compile the project: ghc -O2 -no-keep-hi-files -no-keep-o-files main.hs
import Parser
import Interpreter

getResult :: String -> IO Int
getResult inp = do
                putStrLn inp
                putStr "Result: "
                return (interpreter (eval inp))

-- Trivial examples to execute a REC program 
example1, example2, example3, example4, example5 :: IO Int
example1 = getResult "f1(x1, x2) = x1 + 2, f2() = f2() + 1; 3 + f1(x1 + 2, f2()); x1 = 2;" 
example2 = getResult "f2(x1) = x1 + 2; f2(y); y = undef;" 
example3 = getResult "functional (n) = if n then 1 else functional(n-1) * n; functional (x); x = 3;" 
example4 = getResult "functional (n) = if n then 1 else functional(n-1) * n; functional (-2);;" 
example5 = getResult "; undef;;" 

-- You can choose an example or write your own REC program as a string
main :: IO ()
main = do
    inp <- readLn
    case inp of
        "example1" -> do
            s <- example1
            print s
        "example2" -> do
            s <- example2
            print s
        "example3" -> do
            s <- example3
            print s
        "example4" -> do
            s <- example4
            print s
        "example5" -> do
            s <- example5
            print s
        _ -> do
            result <- getResult inp 
            print result
