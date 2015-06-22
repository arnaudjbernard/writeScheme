{-# LANGUAGE ExistentialQuantification #-}

module LispEval where

import LispDefinition
import LispClosure

import Control.Monad.Error (throwError, catchError, runErrorT, mapErrorT)
import Control.Monad (liftM)
import Control.Monad.Trans (liftIO)

import Data.IORef


---------------------------------------------------------------------------------------------------
-- Functions

atomValue :: LispVal -> ThrowsError String
atomValue (Atom val) = return val
atomValue lispVal = throwError $ BadSpecialForm "Function takes atoms as param name" lispVal

makeFunc :: (Maybe String) -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeFunc varargs env params body =
    case mapM atomValue params of
        Left l -> throwError l
        Right paramNames -> return $ Func paramNames varargs body env

makeNormalFunc = makeFunc Nothing
makeVarArgs = makeFunc . Just . showVal

---------------------------------------------------------------------------------------------------
-- Eval

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env val@(DottedList _ _) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) =
    do
        result <- eval env pred
        case result of
            Bool False -> eval env alt
            otherwise -> eval env conseq
eval env (List [Atom "set!", Atom var, form]) =
     eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
     eval env form >>= defineVar env var
eval env (List ((Atom "cond"):expressions)) = evalCond env expressions
eval env (List ((Atom "case"):vExpr:expressions)) = eval env vExpr >>= \ v -> evalCase env v expressions
eval env (List (Atom "define" : List (Atom var : params) : body)) =
     makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
     makeVarArgs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
     makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
     makeVarArgs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
     makeVarArgs varargs env [] body
eval env (List (function : args)) = do
     func <- eval env function
     argVals <- mapM (eval env) args
     apply func argVals
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm



---------------------------------------------------------------------------------------------------
-- Apply

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args =
      if num params /= num args && varargs == Nothing
         then throwError $ NumArgs (num params) args
         else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
      where remainingArgs = drop (length params) args
            num = toInteger . length
            evalBody env = liftM last $ mapM (eval env) body
            bindVarArgs arg env = case arg of
                Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
                Nothing -> return env

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

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map makePrimitiveFunc primitives)
     where makePrimitiveFunc (var, func) = (var, PrimitiveFunc func)

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

evalCond :: Env -> [LispVal] -> IOThrowsError LispVal
evalCond env [] = throwError $ Default "Cond ended without value"
evalCond env [(List [Atom "else", expr])] = eval env expr
evalCond env ((List [cond, expr]):xs) =
    do
        condResult <- eval env cond
        case condResult of
            Bool True -> eval env expr
            Bool False -> evalCond env xs
            notBool -> throwError $ TypeMismatch "boolean" notBool
evalCond env ((List x):xs) = throwError $ NumArgs 2 x
evalCond env (x:xs) = throwError $ TypeMismatch "list" x

evalCase :: Env -> LispVal -> [LispVal] -> IOThrowsError LispVal
evalCase env _ [] = throwError $ Default "Case ended without value"
evalCase env _ [(List [Atom "else", expr])] = eval env expr
evalCase env v ((List [List conds, expr]):xs) =
    do
        condsResult <- evalCaseEquals env v conds
        case condsResult of
            True -> eval env expr
            False -> evalCase env v xs
evalCase env _ (x:xs) = throwError $ TypeMismatch "list" x

evalCaseEquals :: Env -> LispVal -> [LispVal] -> IOThrowsError Bool
evalCaseEquals env _ [] = return False
evalCaseEquals env refVal (x:xs) =
    do
        caseVal <- eval env x
        doMatch <- liftThrows $ eqv [refVal, caseVal]
        case doMatch of
            Bool True -> return True
            Bool False -> evalCaseEquals env refVal xs

