{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE UndecidableInstances #-}

module Monad2 where

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

-- instance MonadFish m => MonadJoin m where
--     returnJoin = returnFish
--     join     = id >=> id
--
-- instance (Functor m, MonadJoin m) => Monad m where
--     return  = returnJoin
--     m >>= k = join $ fmap k m
--
-- instance (Functor m, MonadJoin m) => MonadFish m where
--     returnFish = returnFish
--     f >=> g    =  join . fmap g . f
