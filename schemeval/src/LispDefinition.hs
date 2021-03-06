module LispDefinition where

import Data.Ratio
import Data.Complex
import Data.IORef

import Text.ParserCombinators.Parsec (ParseError)
import Control.Monad.Error (Error, ErrorT, noMsg, strMsg, catchError)
import System.IO (Handle)

---------------------------------------------------------------------------------------------------
-- Lisp definition

data LispVal =
      Atom String
    | List [LispVal]
    | DottedList [LispVal] LispVal
    | Number Integer
    | String String
    | Bool Bool
    | Character Char
    | Float Double
    | Ratio Rational
    | Complex (Complex Double)
    | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
    | Func { params :: [String], vararg :: (Maybe String),
             body :: [LispVal], closure :: Env }
    | IOFunc ([LispVal] -> IOThrowsError LispVal)
    | Port Handle

showVal :: LispVal -> String
showVal (Atom name) = "Atom: " ++ name
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (Number contents) = show contents
showVal (String contents) = "\"LString " ++ contents ++ "\""
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Character contents) = ['\'', contents, '\'']
showVal (Float contents) = show contents
showVal (Ratio contents) = show contents
showVal (Complex contents) = show contents
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) =
   "(lambda (" ++ unwords (map show args) ++
      (case varargs of
         Nothing -> ""
         Just arg -> " . " ++ arg) ++ ") ...)"
showVal (IOFunc _) = "<IO primitive>"
showVal (Port _)   = "<IO port>"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where
    show = showVal

---------------------------------------------------------------------------------------------------
-- Errors

type IOThrowsError = ErrorT LispError IO

data LispError =
      NumArgs Integer [LispVal]
    | TypeMismatch String LispVal
    | Parser ParseError
    | BadSpecialForm String LispVal
    | NotFunction String String
    | UnboundVar String String [String]
    | Default String

showError :: LispError -> String
showError (UnboundVar message varname boundVars)  = message ++ ": " ++ varname ++ " bound vars: " ++ show boundVars
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected
                                       ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr
showError (Default error)             = "Error: " ++ error

instance Show LispError where show = showError

instance Error LispError where
     noMsg = Default "An error has occurred"
     strMsg = Default

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

---------------------------------------------------------------------------------------------------
-- Env

type Env = IORef [(String, IORef LispVal)]

--showEnv :: Env -> IO String
showEnv env = do
    e <- readIORef env
    return e

nullEnv :: IO Env
nullEnv = newIORef []
