{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeOperators       #-}

module Lib (
    eval,
    partial,
    total,
    apply,
    applyOrElse,
    withDefault,
    isDefinedAt,
    orElse,
    bin,
    combinations,
    permutations,
    abParser,
    abParser_,
    intPair,
    intOrUppercase,
    zeroOrMore,
    oneOrMore,
    spaces,
    ident,
    parseSExpr,
    optLets,
    printLets
    ) where

import           Control.Applicative (Alternative, empty, (<|>))
import qualified Control.Category    as Cat (Category, id, (.))
import           Control.Monad       (liftM2, void, (<=<), (>=>))
import           Data.Char           (isAlpha, isAlphaNum, isDigit, isSpace, isUpper)
import           Data.List           (delete, find)
import           Data.Maybe          (fromMaybe, isJust)

--Block 1

data Expr = Const Integer
          | Expr :+ Expr
          | Expr :- Expr
          | Expr :* Expr
          | Expr :/ Expr
          | Expr :^ Expr
          deriving Show

data ArithmeticError = DivideByZeroError
                     | NegativeExponentError
                     deriving (Eq, Show)

eval :: Expr -> Either ArithmeticError Integer
eval (Const x) = pure x
eval (x :+ y) = liftM2 (+) (eval x) (eval y)
eval (x :- y) = liftM2 (-) (eval x) (eval y)
eval (x :* y) = liftM2 (*) (eval x) (eval y)
eval (x :/ y) = liftM2 (flip div) y' (eval x)
  where
    y' :: Either ArithmeticError Integer
    y' = case eval y of
             Right 0 -> Left DivideByZeroError
             a       -> a
eval (x :^ y) = liftM2 (flip (^)) y' (eval x)
  where
    y' :: Either ArithmeticError Integer
    y' = case eval y of
             Right a
                 | a < 0 -> Left NegativeExponentError
             a           -> a

data a ~> b = Partial   (a -> Maybe b) -- a partial function
            | Defaulted (a ~> b) b     -- a partial function with a default value

partial :: (a -> Maybe b) -> a ~> b
partial = Partial

total :: (a -> b) -> a ~> b
total f = Partial $ Just . f

apply :: (a ~> b) -> a -> Maybe b
apply (Partial f)     = f
apply (Defaulted f b) = Just . fromMaybe b . apply f

applyOrElse :: (a ~> b) -> a -> b -> b
applyOrElse f a b = fromMaybe b $ apply f a

withDefault :: (a ~> b) -> b -> (a ~> b)
withDefault f@(Partial _)   = Defaulted f
withDefault (Defaulted f _) = withDefault f

isDefinedAt :: (a ~> b) -> a -> Bool
isDefinedAt f = isJust . apply f

orElse :: (a ~> b) -> (a ~> b) -> a ~> b
orElse f g = Partial $ \x -> case apply f x of
    Nothing -> apply g x
    y       -> y

instance Cat.Category (~>) where
    id = Partial Just
    f . g = Partial $ apply f <=< apply g
--Block 2

bin :: Int -> [[Int]]
bin n | n <= 0    = [[]]
      | otherwise = bin (n - 1) >>= \xs -> [0 : xs, 1 : xs]

combinations :: Int -> Int -> [[Int]]
combinations n k
    | k < 0 || k > n = []
    | k == 0         = [[]]
    | otherwise      = [[], [n]] >>= \a ->
                       combinations (n - 1) (k - length a) >>= \c -> [a ++ c]

permutations :: [Int] -> [[Int]]
permutations [] = [[]]
permutations xs = xs >>= \a -> permutations (delete a xs) >>= \p -> [a : p]

--Block 4

data ParseError = ParseError
                deriving (Eq, Show)

newtype Monstupar s a = Monstupar { runParser :: [s] -> Either ParseError ([s], a) }

instance Functor (Monstupar s) where
    fmap f p = Monstupar $ \s -> fmap (fmap f) (runParser p s)

instance Applicative (Monstupar s) where
    pure a = Monstupar $ \s -> Right (s, a)
    p1 <*> p2 = Monstupar $ runParser p1 >=> (\(s, f) -> runParser (fmap f p2) s)

instance Alternative (Monstupar s) where
    empty = Monstupar $ \_ -> Left ParseError
    p1 <|> p2 = Monstupar $ \s -> case runParser p1 s of
        Left _ -> runParser p2 s
        res    -> res

instance Monad (Monstupar s) where
    return = pure
    p >>= k = Monstupar $ runParser p >=> \(s, a) -> runParser (k a) s

satisfy :: (s -> Bool) -> Monstupar s s
satisfy p = Monstupar f
  where
    f [] = Left ParseError
    f (x : xs)
        | p x = Right (xs, x)
        | otherwise = Left ParseError

char :: Eq s => s -> Monstupar s s
char c = satisfy (== c)

posInt :: Monstupar Char Integer
posInt = Monstupar f
  where
    f xs | null ns   = Left ParseError
         | otherwise = Right (rest, read ns)
           where
             (ns, rest) = span isDigit xs

abParser :: Monstupar Char (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Monstupar Char ()
abParser_ = void abParser

intPair :: Monstupar Char [Integer]
intPair = (\a b -> [a, b]) <$> posInt <* char ' ' <*> posInt

intOrUppercase :: Monstupar Char ()
intOrUppercase = void posInt <|> void (satisfy isUpper)

zeroOrMore :: Monstupar s a -> Monstupar s [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Monstupar s a -> Monstupar s [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

spaces :: Monstupar Char String
spaces = zeroOrMore $ satisfy isSpace

ident :: Monstupar Char String
ident = (:) <$> satisfy isAlpha <*> zeroOrMore (satisfy isAlphaNum)

trim :: Monstupar Char a -> Monstupar Char a
trim p = spaces *> p <* spaces

type Ident = String

data Atom = N Integer
          | I Ident
          deriving (Eq, Show)

data SExpr = A Atom
           | Comb [SExpr]
           deriving (Eq, Show)

parseAtom :: Monstupar Char Atom
parseAtom = N <$> posInt <|> I <$> ident

parseAtom' :: Monstupar Char SExpr
parseAtom' = A <$> parseAtom

parseComb :: Monstupar Char SExpr
parseComb = Comb <$> (char '(' *> oneOrMore parseSExpr <* char ')')

parseSExpr :: Monstupar Char SExpr
parseSExpr = trim $ parseAtom' <|> parseComb

type Let = (Ident, [Atom])

type Var = (Ident, Integer)

parseLets :: Monstupar Char [Let]
parseLets = zeroOrMore parseLet
  where
    parseLet1 :: Monstupar Char Ident
    parseLet1 = char 'l' *> char 'e' *> char 't' *> trim ident <* char '='

    parseLet2 :: Monstupar Char [Atom]
    parseLet2 = (:) <$> trim parseAtom <*> zeroOrMore (char '+' *> trim parseAtom)

    parseLet :: Monstupar Char Let
    parseLet = trim $ (,) <$> parseLet1 <*> parseLet2

foldLets :: [Let] -> Maybe [Var]
foldLets [] = Just []
foldLets ((name, atoms) : xs) = (:) <$> fmap (name,) (sum <$> traverse f atoms) <*> vars
  where
    vars = foldLets xs
    f (N atom) = Just atom
    f (I atom) = snd <$> (vars >>= find (\(a,_) -> a == atom))

optLets :: String -> String
optLets s = case runParser parseLets s of
    Left _          -> "parse error"
    Right (_, lets) -> case foldLets $ reverse lets of
        Nothing   -> "fold error"
        Just vars -> foldMap (\(name, value) -> "let " ++ name ++ " = " ++ show value ++ "\n") (reverse vars)

printLets :: String -> IO ()
printLets = putStrLn . optLets
