{-# LANGUAGE FlexibleInstances #-}

module Lib where

newtype T f g a = MkT (f (g a)) deriving Show

instance (Functor f, Functor g) => Functor (T f g) where
  fmap h (MkT x) = MkT $ fmap (fmap h) x

instance (Applicative f, Applicative g) => Applicative (T f g) where
  pure = MkT . pure . pure
  (MkT h) <*> (MkT x) = MkT $ fmap (<*>) h <*> x

instance (Foldable f, Foldable g) => Foldable (T f g) where
  foldMap h (MkT x) = foldMap (foldMap h) x
  foldr h z (MkT x) = foldr (flip $ foldr h) z x

instance (Traversable f, Traversable g) => Traversable (T f g) where
  sequenceA (MkT x) = MkT <$> sequenceA (fmap sequenceA x)
  traverse h (MkT x) = MkT <$> traverse (traverse h) x

booleanTable :: C a => a -> IO ()
booleanTable = f ""

class C a where
  f :: String -> a -> IO ()

instance C Bool where
  f s b  = putStrLn $ s ++ "= " ++ show b

instance C a => C (Bool -> a) where
  f s g = do
    f (s ++ "True  ") (g True)
    f (s ++ "False ") (g False)


test1 :: Integer
test1 = sum $ MkT [Just 3, Nothing, Just 1, Just 10]
--14

test2 :: T [] Maybe Integer
test2 = (2*) <$> MkT [Just 3, Nothing, Just 1, Just 10]
-- MkT [ Just 6
--     , Nothing
--     , Just 2
--     , Just 20
--     ]

test3 :: T (Either a) [] Int
test3 = (4*) <$> MkT (Right [1..5])
-- MkT (Right [ 4, 8, 12, 16, 20 ])

test4 :: IO ()
test4 = booleanTable (\a b -> a && (a || b))

test5 :: IO ()
test5 = booleanTable (\a b c -> a || b || not c)
