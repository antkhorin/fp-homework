{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE UndecidableInstances #-}

module Instances where

import           Control.Applicative (liftA3)
import           Data.Foldable       (fold)
import           Data.Monoid         ((<>))
import           Prelude             (Applicative, Foldable, Functor, Monoid, Traversable,
                                      fmap, foldMap, mempty, pure, traverse, ($), (<$>),
                                      (<*>))

newtype Identity a = Identity a

instance Functor Identity where
    fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
    pure = Identity
    (Identity f) <*> (Identity a) = Identity $ f a

instance Foldable Identity where
    fold (Identity a) = a
    foldMap f (Identity a) = f a

instance Traversable Identity where
    traverse f (Identity a) = Identity <$> f a


data Either a b = Left a | Right b

instance Functor (Either a) where
    fmap _ (Left a)  = Left a
    fmap f (Right a) = Right $ f a

instance Applicative (Either a) where
    pure = Right
    (Left a)  <*> _         = Left a
    _         <*> (Left a)  = Left a
    (Right f) <*> (Right a) = Right $ f a

instance Foldable (Either a) where
    foldMap _ (Left _)  = mempty
    foldMap f (Right a) = f a

instance Traversable (Either a) where
    traverse _ (Left a)  = pure $ Left a
    traverse f (Right a) = Right <$> f a


data Tree a = Leaf | Node a (Tree a) (Tree a)

instance Functor Tree where
    fmap _ Leaf         = Leaf
    fmap f (Node a l r) = Node (f a) (f <$> l) (f <$> r)

instance Applicative Tree where
    pure a = let y = pure a in Node a y y
    Leaf           <*> _              = Leaf
    _              <*> Leaf           = Leaf
    (Node f l1 r1) <*> (Node a l2 r2) = Node (f a) (l1 <*> l2) (r1 <*> r2)

instance Foldable Tree where
    foldMap _ Leaf         = mempty
    foldMap f (Node a l r) = foldMap f l <> f a <> foldMap f r

instance Traversable Tree where
    traverse _ Leaf         = pure Leaf
    traverse f (Node a l r) = liftA3 Node (f a) (traverse f l) (traverse f r)


newtype Const a b = Const a

instance Functor (Const a) where
    fmap _ (Const a) = Const a

instance Monoid a => Applicative (Const a) where
    pure _ = Const mempty
    (Const a) <*> (Const b) = Const $ a <> b

instance Foldable (Const a) where
    foldMap _ _ = mempty

instance Traversable (Const a) where
    traverse _ (Const a) = pure $ Const a


data Pair a b = Pair a b

instance Functor (Pair a) where
    fmap f (Pair a b) = Pair a (f b)

instance Monoid a => Applicative (Pair a) where
    pure = Pair mempty
    (Pair a1 f) <*> (Pair a2 a) = Pair (a1 <> a2) (f a)

instance Foldable (Pair a) where
    foldMap f (Pair _ b) = f b

instance Traversable (Pair a) where
    traverse f (Pair a b) = Pair a <$> f b
