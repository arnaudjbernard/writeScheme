module LispParsing where

import LispDefinition

import Control.Monad (liftM)
import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric (readFloat, readHex, readOct)
import Data.Ratio
import Data.Complex
import Control.Monad.Error (throwError)

import Debug.Trace

---------------------------------------------------------------------------------------------------
-- Symbol

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

---------------------------------------------------------------------------------------------------
-- First exercise

spaces :: Parser ()
spaces = skipMany1 space

parseSymbol :: Parser Char
parseSymbol = spaces >> symbol

---------------------------------------------------------------------------------------------------
-- String

escapedChars :: Parser Char
escapedChars = do
    char '\\'
    x <- oneOf "\"\\nrt"
    return $ case x of
        '\\' -> x
        '"' -> x
        'n' -> '\n'
        'r' -> '\r'
        't' -> '\t'

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many $ escapedChars <|> noneOf "\"\\"
    char '"'
    return $ String x


---------------------------------------------------------------------------------------------------
-- Float

parseFloat :: Parser LispVal
parseFloat = do
    x <- many1 digit
    char '.'
    y <- many1 digit
    return . Float . stringToDouble $ x ++ "." ++ y

stringToDouble :: String -> Double
stringToDouble = fst . head . readFloat

---------------------------------------------------------------------------------------------------
-- Number

parseDigital1 :: Parser LispVal
parseDigital1 = many1 digit >>= (return . Number . read)

parseDigital2 :: Parser LispVal
parseDigital2 = do
    try $ string "#d"
    x <- many1 digit
    return . Number . read $ x

parseHex :: Parser LispVal
parseHex = do
    try $ string "#x"
    x <- many1 hexDigit
    return $ Number (hex2dig x)

parseOct :: Parser LispVal
parseOct = do
    try $ string "#o"
    x <- many1 hexDigit
    return $ Number (hex2dig x)

parseBin :: Parser LispVal
parseBin = do
    try $ string "#b"
    x <- many1 (oneOf "10")
    return $ Number (bin2dig x)

hex2dig :: (Integral a) => String -> a
hex2dig = fst . head . readHex

oct2dig :: (Integral a) => String -> a
oct2dig = fst . head . readOct

bin2dig  = bin2dig' 0
bin2dig' digint "" = digint
bin2dig' digint (x:xs) = let ndigint = 2 * digint + (if x == '0' then 0 else 1) in
                         bin2dig' ndigint xs

parseNumber :: Parser LispVal
--parseNumber = liftM (Number . read) $ many1 digit
--parseNumber = do
--    d <- many1 digit
--    return . Number . read $ d
--parseNumber = many1 digit >>= \d -> return . Number . read $ d
--parseNumber = many1 digit >>= return . Number . read
parseNumber = parseDigital1 <|> parseDigital2 <|> parseHex <|> parseOct <|> parseBin

parseRatio :: Parser LispVal
parseRatio = do
    x <- many1 digit
    char '/'
    y <- many1 digit
    return $ Ratio ((read x) % (read y))

toDouble :: LispVal -> Double
toDouble(Float f) = f
toDouble(Number n) = fromIntegral n

parseComplex :: Parser LispVal
parseComplex = do
    x <- (try parseFloat <|> parseDigital1)
    char '+'
    y <- (try parseFloat <|> parseDigital1)
    char 'i'
    return $ Complex (toDouble x :+ toDouble y)

---------------------------------------------------------------------------------------------------
-- Atom

parseAtom :: Parser LispVal
parseAtom = do
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              return . Atom $ first:rest

---------------------------------------------------------------------------------------------------
-- Bool

parseBool :: Parser LispVal
parseBool = do
    char '#'
    (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

---------------------------------------------------------------------------------------------------
-- Character

parseCharacter :: Parser LispVal
parseCharacter = do
    try $ string "#\\"
    x <- try (string "newline" <|> string "space")
        <|> do { x <- anyChar; notFollowedBy alphaNum; return [x] }
    return . Character $ case x of
        "newline" -> '\n'
        "space" -> ' '
        _ -> head x

---------------------------------------------------------------------------------------------------
-- List

parseAllLists :: Parser LispVal
parseAllLists = do
    char '('
    x <- try parseList <|> parseDottedList
    char ')'
    return x

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

---------------------------------------------------------------------------------------------------
-- Quote

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
    char '`'
    x <- parseExpr
    return $ List [Atom "quasiquote", x]

parseUnQuote :: Parser LispVal
parseUnQuote = do
    char ','
    x <- parseExpr
    return $ List [Atom "unquote", x]

---------------------------------------------------------------------------------------------------
-- Parser

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> try parseComplex
        <|> try parseFloat
        <|> try parseRatio
        <|> try parseNumber
        <|> try parseBool
        <|> parseCharacter
        <|> parseQuoted
        <|> parseQuasiQuoted
        <|> parseUnQuote
        <|> parseAllLists

parseLisp :: String -> Either ParseError LispVal
parseLisp input = parse parseExpr "lisp" input

readExpr :: String -> ThrowsError LispVal
readExpr input = case parseLisp input of
     Left err -> traceShow ("parse error", err) $ throwError $ Parser err
     Right val -> traceShow ("parse success", val) $ return val
