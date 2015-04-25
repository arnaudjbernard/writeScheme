module Main where

import LispDefinition
import LispParsing
import LispEval
import LispRepl

import System.Environment
import Control.Monad (liftM)

---------------------------------------------------------------------------------------------------
-- Input

getArgsOrInput :: IO [String]
getArgsOrInput = askArgsOrInput "Args?"

askArgsOrInput :: String -> IO [String]
askArgsOrInput ms = do
    args <- getArgs
    case args of
        [] -> do
            line <- askUserInput ms
            return $ [line]
        _ -> return args

getUserInput :: IO String
getUserInput = askUserInput "Args?"

askUserInput :: String -> IO String
askUserInput ms = do
    putStrLn ms
    line <- getLine
    return line


---------------------------------------------------------------------------------------------------
-- Main

--main :: IO ()
--main = do
--     args <- getArgsOrInput
--     let parsed = readExpr . head $ args
--     putStrLn $ "Parsed: " ++ (show parsed)
--     let evalResult = parsed >>= eval
--     putStrLn $ "Evaluted: " ++ (show evalResult)
--     evaled <- return $ liftM show $ evalResult
--     putStrLn $ extractValue $ trapError evaled

main :: IO ()
main = do args <- getArgs
          case length args of
               0 -> runRepl
               1 -> evalAndPrint $ args !! 0
               otherwise -> putStrLn "Program takes only 0 or 1 argument"
