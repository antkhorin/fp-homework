{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE RankNTypes                #-}

module Lib where

import           Data.Char      (isDigit)
import           Data.List      (find, sort)
import           Data.Maybe     (fromMaybe)
import           Data.Semigroup (Semigroup, (<>))
import           Text.Read      (readMaybe)

--Block 1

order3 :: Ord a => (a, a, a) -> (a, a, a)
order3 (x, y, z) = let [a, b, c] = sort [x, y, z] in (a, b, c)

-- Return -1 if x <= 0
highestBit :: Integer -> Maybe Integer
highestBit x = fst <$> highestBitAdv x

-- Return (-1, 0) if x <= 0
highestBitAdv :: Integer -> Maybe (Integer, Integer)
highestBitAdv x
    | x <= 0    = Nothing
    | otherwise = Just $ f x (1, 0)
      where
        f :: Integer -> (Integer, Integer) -> (Integer, Integer)
        f 1 (y, z) = (y, z)
        f a (y, z) = f (a `div` 2) (y * 2, z + 1)

smartReplicate :: [Int] -> [Int]
smartReplicate = concatMap (\x -> replicate x x)

contains :: (Eq a) => a -> [[a]] -> [[a]]
contains = filter . elem

--Block 2

removeAt :: Integer -> [a] -> [a]
removeAt i xs = snd $ removeAtAdv i xs

removeAtAdv :: Integer -> [a] -> (Maybe a, [a])
removeAtAdv i xs = (z, zs)
  where
    ys = zip [0..] xs
    z  = snd <$> find (\(a, _) -> a == i) ys
    zs = map snd $ filter (\(a, _) -> a /= i) ys

collectEvery :: Integer -> [a] -> ([a], [a])
collectEvery i xs = (ys, zs)
  where
    as = zip [1..] xs
    ys = map snd $ filter (\(a, _) -> i > 0  && a `mod` i /= 0) as
    zs = map snd $ filter (\(a, _) -> i <= 0 || a `mod` i == 0) as

stringSum :: String -> Maybe Integer
stringSum = foldr (g . f) (Just 0) . words
  where
    f ('+' : xs@(x : _))
        | isDigit x = readMaybe xs
        | otherwise = Nothing
    f xs         = readMaybe xs
    g Nothing _         = Nothing
    g _ Nothing         = Nothing
    g (Just a) (Just b) = Just $ a + b

mergeSort :: (Ord a) => [a] -> [a]
mergeSort []  = []
mergeSort [x] = [x]
mergeSort xs  = let (ys, zs) = splitAt (length xs `div` 2) xs in merge (mergeSort ys) (mergeSort zs)
  where
    merge ys [] = ys
    merge [] zs = zs
    merge ys@(y : ys') zs@(z : zs')
        | y < z     = y : merge ys' zs
        | otherwise = z : merge ys zs'

--Block 3

data DayOfWeek = Monday
               | Tuesday
               | Wednesday
               | Thursday
               | Friday
               | Saturday
               | Sunday
               deriving (Enum, Show)

nextDay :: DayOfWeek -> DayOfWeek
nextDay Sunday = Monday
nextDay day    = succ day

afterDays :: Int -> DayOfWeek -> DayOfWeek
afterDays n day = iterate nextDay day !! (n `mod` 7)

isWeekend :: DayOfWeek -> Bool
isWeekend Saturday = True
isWeekend Sunday   = True
isWeekend _        = False

daysToParty :: DayOfWeek -> Int
daysToParty Friday = 0
daysToParty day    = 1 + daysToParty (nextDay day)

data Fighter = Knight {attack :: Integer, hp :: Integer}
             | Monster {attack :: Integer, hp :: Integer} deriving (Eq, Ord, Show)

fight :: Fighter -> Fighter -> (Fighter, Integer)
fight a b = if even c then (b, c) else (a, c)
  where
    c = rounds (min a b) (max a b)
    rounds :: Fighter -> Fighter -> Integer
    rounds f1 f2
        | hp f1 <= 0 || hp f2 <= 0 = 0
        | otherwise                = 1 + rounds f2 {hp = hp f2 - attack f1} f1

data Vector a = Vector2D a a | Vector3D a a a

to3D :: Num a => Vector a -> Vector a
to3D (Vector2D x y) = Vector3D x y 0
to3D v              = v

lengthV :: Floating a => Vector a -> a
lengthV a = let (Vector3D x y z) = to3D a in sqrt $ x ** 2 + y ** 2 + z ** 2

addV :: Num a => Vector a -> Vector a -> Vector a
addV (Vector2D x1 y1) (Vector2D x2 y2) = Vector2D (x1 + x2) (y1 + y2)
addV a b = let (Vector3D x1 y1 z1) = to3D a; (Vector3D x2 y2 z2) = to3D b in
               Vector3D (x1 + x2) (y1 + y2) (z1 + z2)
--
scalarMul :: Num a => Vector a -> Vector a -> a
scalarMul a b = let (Vector3D x1 y1 z1) = to3D a; (Vector3D x2 y2 z2) = to3D b in
                    x1 * x2 + y1 * y2 + z1 * z2
--
--
dist :: Floating a => Vector a -> Vector a -> a
dist a b = let (Vector3D x1 y1 z1) = to3D a; (Vector3D x2 y2 z2) = to3D b in
              sqrt $ (x1 - x2) ** 2 + (y1 - y2) ** 2 + (z1 - z2) ** 2
--
vecMul :: Num a => Vector a -> Vector a -> Vector a
vecMul a b = let (Vector3D x1 y1 z1) = to3D a; (Vector3D x2 y2 z2) = to3D b in
                Vector3D (y1 * z2 - z1 * y2) (z1 * x2 - z2 * x1) (x1 * y2 - x2 * y1)

data Nat = Z | S Nat deriving Show

natToInteger :: Nat -> Integer
natToInteger Z     = 0
natToInteger (S x) = 1 + natToInteger x

instance Num Nat where
  Z + y   = y
  S x + y = S $ x + y

  Z * _   = Z
  S x * y = y + x * y

  Z - _         = Z
  x - Z   = x
  (S x) - (S y) = x - y

  abs = id

  signum Z = Z
  signum _ = S Z

  fromInteger x
      | x <= 0    = Z
      | otherwise = S $ fromInteger (x - 1)

  negate _ = Z

instance Eq Nat where
  Z == Z         = True
  (S x) == (S y) = x == y
  _ == _         = False

instance Ord Nat where
  Z <= _         = True
  (S _) <= Z     = False
  (S x) <= (S y) = x <= y

isEven :: Nat -> Bool
isEven Z     = True
isEven (S x) = not $ isEven x

divModNat :: Nat -> Nat -> Maybe (Nat, Nat)
divModNat _ Z = Nothing
divModNat x y
    | a == Z && b == Z = Just (Z, x)
    | a == Z && b /= Z = Just (S Z, Z)
    | otherwise        = let Just (d, m) = divModNat a y in Just (S Z + d, m)
      where
         a = x - y
         b = S Z + x - y

divNat :: Nat -> Nat -> Maybe Nat
divNat x y = fst <$> divModNat x y

modNat :: Nat -> Nat -> Maybe Nat
modNat x y = snd <$> divModNat x y

gcdNat :: Nat -> Nat -> Nat
gcdNat x y
    | x < y     = gcdNat y x
    | x == y    = x
    | otherwise = gcdNat (x - y) y

data Tree a = Ord a => Leaf | Ord a => Node a (Tree a) (Tree a)

add :: a -> Tree a -> Tree a
add a Leaf = Node a Leaf Leaf
add a n@(Node x l r)
    | a < x     = Node x (add a l) r
    | a > x     = Node x l (add a r)
    | otherwise = n

fromList :: Ord a => [a] -> Tree a
fromList = foldr add Leaf

--Block 4

instance Foldable Tree where
  foldMap _ Leaf         = mempty
  foldMap f (Node x l r) = foldMap f l `mappend` f x `mappend` foldMap f r

  foldr _ x Leaf         = x
  foldr f z (Node x l r) = foldr f (f x $ foldr f z l) r

  null Leaf = True
  null _    = False

  length Leaf         = 0
  length (Node _ l r) = length l + 1 + length r

  elem _ Leaf = False
  elem a (Node x l r)
      | a < x     = a `elem` l
      | a > x     = a `elem` r
      | otherwise = True

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn sep = foldr f [[]] where
  f _ [] = undefined -- SuppressWarning
  f a bs@(b : bs')
      | a == sep    = [] : bs
      | otherwise = (a : b) : bs'

joinWith :: a -> [[a]] -> [a]
joinWith _ []   = []
joinWith sep xs = tail $ foldr (\ a b -> (sep : a) ++ b) [] xs

--Block 5

maybeConcat :: [Maybe [a]] -> [a]
maybeConcat = fromMaybe [] . mconcat

eitherConcat :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
eitherConcat = foldMap f where
  f (Left x)  = (x, mempty)
  f (Right y) = (mempty, y)

data NonEmpty a = a :| [a]

instance Semigroup (NonEmpty a) where
  (a :| as) <> (b :| bs) = a :| (as ++ b : bs)

newtype Identity a = Identity { runIdentity :: a }

instance Semigroup a => Semigroup (Identity a) where
  Identity a <> Identity b = Identity $ a <> b

instance Monoid a => Monoid (Identity a) where
  Identity a `mappend` Identity b   = Identity $ mappend a b
  mempty                            = Identity mempty

newtype Name = Name String deriving (Show)

instance Semigroup Name where
  Name "" <> b     = b
  a <> Name ""     = a
  Name a <> Name b = Name $ a ++ "." ++ b

instance Monoid Name where
  mappend = (<>)
  mempty  = Name ""

newtype Endo a = Endo { getEndo :: a -> a }

instance Semigroup (Endo a) where
  Endo f <> Endo g = Endo $ f . g

instance Monoid (Endo a) where
  mappend = (<>)
  mempty  = Endo id

newtype Arrow a b = Arrow { getArrow :: a -> b }

instance Semigroup b => Semigroup (Arrow a b) where
  Arrow f <> Arrow g = Arrow (\x -> f x <> g x)

instance Monoid b => Monoid (Arrow a b) where
  Arrow f `mappend` Arrow g   = Arrow (\x -> f x `mappend` g x)
  mempty                      = Arrow $ const mempty

instance Semigroup (Tree a) where
  (<>) = foldr add

instance Ord a => Monoid (Tree a) where
  mappend = (<>)
  mempty = Leaf
