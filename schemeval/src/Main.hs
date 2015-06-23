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

showHelp :: IO ()
showHelp = do
    putStrLn "-i for interactive, -e expr for eval, filename for loading and running a file"

main :: IO ()
main = do args <- getArgs
          case args of
               [] -> runRepl
               ["-i"] -> runRepl
               ["-h"] -> showHelp
               ("-e":expr:[]) -> runOne expr
               otherwise -> runFile args
