{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE UndecidableInstances #-}

module Monad1 where

import           Prelude ()

class Functor f where
    fmap :: (a -> b) -> f a -> f b

class Monad m where
    return     :: a -> m a
    (>>=)      :: m a -> (a -> m b) -> m b

class MonadFish m where
    returnFish :: a -> m a
    (>=>)      :: (a -> m b) -> (b -> m c) -> (a -> m c)

class MonadJoin m where
    returnJoin :: a -> m a
    join       :: m (m a) -> m a

-- instance Monad m => MonadFish m where
--     returnFish = return
--     f >=> g    = \m -> f m >>= g
--
-- instance Monad m => MonadJoin m where
--     returnJoin = return
--     join m     = m >>= id
--
-- instance MonadFish m => Monad m where
--     return  = returnFish
--     m >>= k = id >=> k $ m
