{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TypeFamilies          #-}

module Lib where

import           Control.Monad.Cont
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Map                   (Map, empty, insert, lookup, member)
import           Data.Maybe                 (fromMaybe)
import           Prelude                    hiding (lookup)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Expr
import           Text.Read                  (readMaybe)

type Name = String

data Expr
    = Mut Name ArithmeticExpr
    | Upd Name ArithmeticExpr
    | Out ArithmeticExpr
    | In Name
    | For Name Integer Integer [Expr]
    | Break
    deriving (Eq, Show)

data ArithmeticExpr
    = Lit Integer
    | Var Name
    | Add ArithmeticExpr ArithmeticExpr
    | Sub ArithmeticExpr ArithmeticExpr
    | Mul ArithmeticExpr ArithmeticExpr
    | Div ArithmeticExpr ArithmeticExpr
    | Let Name ArithmeticExpr ArithmeticExpr
    deriving (Eq, Show)

infixl 6 `Add`, `Sub`
infixl 7 `Mul`, `Div`

data MyException
    = NotInitializedVariable
    | DivideByZero
    | VariableAlreadyDefined
    | VariableNotDefined
    | ReadException
    deriving (Show, Eq)

class (MonadCont m, MonadError MyException m, MonadState (Map Name Integer) m, MonadIO m) => MonadExpr m

instance MonadExpr (ExceptT MyException (ContT (Either MyException ()) (StateT (Map Name Integer) IO)))

-- newtype ExprM a = ExprM (ExceptT MyException (StateT (Map Name Integer) IO) a) deriving Functor

toReader :: MonadExpr m => ArithmeticExpr -> ReaderT (Map Name Integer) m Integer
toReader (Lit x) = return x
toReader (Var x) = do
    r <- ask
    fromMaybe (throwError NotInitializedVariable) (return <$> lookup x r)
toReader (Add x y) = liftM2 (+) (toReader x) (toReader y)
toReader (Sub x y) = liftM2 (-) (toReader x) (toReader y)
toReader (Mul x y) = liftM2 (*) (toReader x) (toReader y)
toReader (Div x y) = liftM2 (flip div)    y' (toReader x)
  where
    y' = do
        res <- toReader y
        if res == 0 then throwError DivideByZero else return res
toReader (Let x expr1 expr2) = do
    a <- toReader expr1
    local (insert x a) (toReader expr2)

eval :: MonadExpr m => ArithmeticExpr -> m Integer
eval exprr = do
    s <- get
    runReaderT (toReader exprr) s

create :: MonadExpr m => Name -> Integer -> m ()
create x a = do
    s <- get
    if member x s then throwError VariableAlreadyDefined else modify (insert x a)

upd :: MonadExpr m => Name -> Integer -> m ()
upd x a = do
    s <- get
    if member x s then modify (insert x a) else throwError VariableNotDefined

exec :: MonadExpr m => [Expr] -> m ()
exec = mapM_ execExpr

execExpr :: MonadExpr m => Expr -> m ()
execExpr (Mut x a) = do
    res <- eval a
    create x res
execExpr (Upd x a) = do
    res <- eval a
    upd x res
execExpr (Out a) = do
    res <- eval a
    liftIO $ print res
execExpr (In x) = do
    s <- liftIO getLine
    let res = readMaybe s :: Maybe Integer
    fromMaybe (throwError ReadException) (upd x <$> res)
execExpr (For x from to exprs)
    | from > to = return ()
    | otherwise = do
        modify (insert x from)
        callCC $ \k -> do
            mapM_ (\exprr -> if exprr == Break then k () else execExpr exprr) exprs
            execExpr (For x (from + 1) to exprs)
execExpr Break = return ()


type Parser = Parsec () String

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

expr :: Parser Expr
expr =  In <$> (symbol ">" *> name) <* eof
    <|> Out <$> (symbol "<" *> arithmeticExpr) <* eof
    <|> Mut <$> (symbol "mut" *> name) <*> (symbol "=" *> arithmeticExpr) <* eof
    <|> Upd <$> name <* symbol "=" <*> arithmeticExpr <* eof

arithmeticExpr :: Parser ArithmeticExpr
arithmeticExpr = makeExprParser term operators

term :: Parser ArithmeticExpr
term =  lit
    <|> var
    <|> try letvar
    <|> parens arithmeticExpr

operators :: [[Operator Parser ArithmeticExpr]]
operators =
    [ [ InfixL (Mul <$ symbol "*")
      , InfixL (Div <$ symbol "/")]
    , [ InfixL (Add <$ symbol "+")
      , InfixL (Sub <$ symbol "-")]]

integer :: Parser Integer
integer = lexeme L.decimal

name :: Parser String
name = lexeme $ (:) <$> letterChar <*> many alphaNumChar

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

lit :: Parser ArithmeticExpr
lit = Lit <$> integer

var :: Parser ArithmeticExpr
var = Var <$> name

letvar :: Parser ArithmeticExpr
letvar = parens $ Let <$> (symbol "let" *> name)
                      <*> (symbol "=" *> arithmeticExpr)
                      <*> arithmeticExpr


e0 :: Expr
e0 = Mut "x" (Lit 1)

e1 :: Expr
e1 = In "x"

e2 :: Expr
e2 = Out (Var "x")

forr :: Integer -> Expr
forr n = For "i" 1 n [Upd "x" (Mul (Var "x") (Var "i"))]

ff :: ExceptT MyException (ContT (Either MyException ()) (StateT (Map Name Integer) IO)) () -> IO (Either MyException ())
ff a = evalStateT (runContT (runExceptT a) return) empty

ll :: [Expr]
ll = e0 : cycle [e1, e2]

test1 :: [Expr]
test1 = [e0, forr 6, e2]
