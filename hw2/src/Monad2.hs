{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE UndecidableInstances #-}

module Monad2 where

import           Prelude (id, ($), (.))

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

instance MonadFish m => MonadJoin m where
    returnJoin = returnFish
    join     = id >=> id

    {-# LAWS
        1.  (join . returnJoin) m =
            join (returnJoin m) =
            (id >=> id) (returnJoin m) =
        2.  join . fmap returnJoin ≡ id
        3.  join . fmap join       ≡ join . join
        4*  join . fmap (fmap f)   ≡ fmap f . join
    #-}

instance (Functor m, MonadJoin m) => Monad m where
    return  = returnJoin
    m >>= k = join $ fmap k m

    {-# LAWS
        1.  m >>= return =
            join (fmap return m) =
            join (fmap returnJoin m) =
            m
        2.  return a >>= f  ≡ f a
        3.  (m >>= f) >>= g ≡ m >>= (\x -> f x >>= g)
    #-}

instance (Functor m, MonadJoin m) => MonadFish m where
    returnFish = returnFish
    f >=> g    =  join . fmap g . f

    {-# LAWS
        1.  (f >=> returnFish) m =
            join (fmap returnFish (f m)) =
            join (fmap returnJoin (f m)) =
            f m
        2.  (returnFish >=> f) m =
            join (fmap f (returnFish m)) =

        3.  (f >=> g) >=> h  ≡ f >=> (g >=> h)
    #-}
