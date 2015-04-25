module Main where
import System.Environment

--askForName :: IO String
--askForName = do
--    putStrLn "What's your name?"
--    line <- getLine
--    if null line
--    then return "Unknown"
--    else return line
--
--main :: IO ()
--main = do
--    args <- getArgs
--    finalName <- case args of
--                    [] -> askForName
--                    [name] -> return name
--                    firstName:lastName:_ -> return $ firstName ++ " " ++ lastName
--    putStrLn $ "Hello, " ++ finalName

---------------------------------------------------------------------------------------------------
-- Input

getArgsOrInput :: IO [String]
getArgsOrInput = getArgsOrInputParam "Args?"

getArgsOrInputParam :: String -> IO [String]
getArgsOrInputParam ms = do
    args <- getArgs
    case args of
        [] -> do
            line <- getUserInput ms
            return $ words line
        _ -> return args

getUserInput :: String -> IO String
getUserInput ms = do
    putStrLn ms
    line <- getLine
    return line

al :: IO ()
al = do
    args <- getArgsOrInputParam "First Name?"
    putStrLn $ "I'm sorry " ++ args !! 0 ++ ", I'm afraid I can't do that."



fatty :: IO ()
fatty = do
    sizeString <- getInput "Size(cm)?"
    weightString <- getInput "Weight(kg)?"
    let size = read $ sizeString
    let weight = read $ weightString
    let imc = weight / (size * size) * 10000

    putStrLn $ "imc: " ++ show imc
    putStrLn $ if imc >= 25 then "you fat" else "you ok"

main :: IO ()
main = fatty
