{-# LANGUAGE ExistentialQuantification #-}

module LispEval where

import LispDefinition

import Control.Monad.Error (throwError, catchError)
import Control.Monad (liftM)
---------------------------------------------------------------------------------------------------
-- Eval

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval val@(DottedList _ _) = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", pred, conseq, alt]) =
     do result <- eval pred
        case result of
             Bool False -> eval alt
             otherwise  -> eval conseq
eval (List ((Atom "cond"):expressions)) = evalCond expressions
eval (List ((Atom "case"):vExpr:expressions)) = eval vExpr >>= \ v -> evalCase v expressions
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

---------------------------------------------------------------------------------------------------
-- Apply

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)

---------------------------------------------------------------------------------------------------
-- Primitives

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [
        -- unary
        ("symbol?", unaryOp symbolp),
        ("string?", unaryOp stringp),
        ("number?", unaryOp numberp),
        ("bool?", unaryOp boolp),
        ("list?", unaryOp listp),
        ("symbol->string", unaryOp symbolToString),
        ("string->symbol", unaryOp stringToSymbol),
        -- bin and more op
        ("+", numericBinop (+)),
        ("-", numericBinop (-)),
        ("*", numericBinop (*)),
        ("/", numericBinop div),
        ("mod", numericBinop mod),
        ("quotient", numericBinop quot),
        ("remainder", numericBinop rem),
        -- binop
        ("=", numBoolBinop (==)),
        ("<", numBoolBinop (<)),
        (">", numBoolBinop (>)),
        ("/=", numBoolBinop (/=)),
        (">=", numBoolBinop (>=)),
        ("<=", numBoolBinop (<=)),
        ("&&", boolBoolBinop (&&)),
        ("||", boolBoolBinop (||)),
        ("string=?", strBoolBinop (==)),
        ("string<?", strBoolBinop (<)),
        ("string>?", strBoolBinop (>)),
        ("string<=?", strBoolBinop (<=)),
        ("string>=?", strBoolBinop (>=)),
        -- lisp list
        ("car", car),
        ("cdr", cdr),
        ("cons", cons),
        -- lisp equal
        ("eq?", eqv),
        ("eqv?", eqv),
        ("equal?", equal)
    ]

---------------------------------------------------------------------------------------------------
-- Unpack

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op           []  = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = mapM unpackNum params >>= return . Number . foldl1 op


---------------------------------------------------------------------------------------------------
-- Operators

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp f []  = throwError $ NumArgs 1 []
unaryOp f [v] = return $ f v
unaryOp f multipleArgs@_  = throwError $ NumArgs 1 multipleArgs

symbolp, numberp, stringp, boolp, listp :: LispVal -> LispVal
symbolp (Atom _)   = Bool True
symbolp _          = Bool False
numberp (Number _) = Bool True
numberp _          = Bool False
stringp (String _) = Bool True
stringp _          = Bool False
boolp   (Bool _)   = Bool True
boolp   _          = Bool False
listp   (List _)   = Bool True
listp   (DottedList _ _) = Bool True
listp   _          = Bool False

symbolToString, stringToSymbol :: LispVal -> LispVal
symbolToString (Atom a) = String a
symbolToString _ = Bool False
stringToSymbol (String s) = Atom s
stringToSymbol _ = Bool False

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right

numBoolBinop  = boolBinop unpackNum
strBoolBinop  = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

---------------------------------------------------------------------------------------------------
-- List

car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)]         = return x
car [DottedList (x : xs) _] = return x
car [badArg]                = throwError $ TypeMismatch "pair" badArg
car badArgList              = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)]         = return $ List xs
cdr [DottedList [_] x]      = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg]                = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)]             = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)]         = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)]         = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)]             = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)]             = return $ Bool $ (length arg1 == length arg2) &&
                                                             (all eqvPair $ zip arg1 arg2)
     where eqvPair (x1, x2) = case eqv [x1, x2] of
                                Left err -> False
                                Right (Bool val) -> val
eqv [_, _]                                 = return $ Bool False
eqv badArgList                             = throwError $ NumArgs 2 badArgList

---------------------------------------------------------------------------------------------------
-- Weak Typing Equal

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
             do unpacked1 <- unpacker arg1
                unpacked2 <- unpacker arg2
                return $ unpacked1 == unpacked2
        `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
      primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                         [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
      eqvEquals <- eqv [arg1, arg2]
      return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

evalCond :: [LispVal] -> ThrowsError LispVal
evalCond [] = throwError $ Default "Cond ended without value"
evalCond [(List [Atom "else", expr])] = eval expr
evalCond ((List [cond, expr]):xs) = case eval cond of
    Right (Bool True) -> eval expr
    Right (Bool False) -> evalCond xs
    Right notBool -> throwError $ TypeMismatch "boolean" notBool
    err -> err
evalCond ((List x):xs) = throwError $ NumArgs 2 x
evalCond (x:xs) = throwError $ TypeMismatch "list" x

evalCase :: LispVal -> [LispVal] -> ThrowsError LispVal
evalCase _ [] = throwError $ Default "Case ended without value"
evalCase _ [(List [Atom "else", expr])] = eval expr
evalCase v ((List [List conds, expr]):xs) = case evalCaseEquals v conds of
    Right True -> eval expr
    Right False -> evalCase v xs
    Left err -> Left err
evalCase _ (x:xs) = throwError $ TypeMismatch "list" x

evalCaseEquals :: LispVal -> [LispVal] -> ThrowsError Bool
evalCaseEquals _ [] = return False
evalCaseEquals v1 (x:xs) = case eval x of
    Right v2 -> case eqv [v1, v2] of
        Right (Bool True) -> return True
        Right (Bool False) -> evalCaseEquals v1 xs
        Left err -> Left err
    Left err -> Left err

