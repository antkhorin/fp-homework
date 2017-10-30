{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE UndecidableInstances #-}

module Monad1 where

import           Prelude (id, ($))

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

instance Monad m => MonadFish m where
    returnFish = return
    f >=> g    = \m -> f m >>= g

    {-# LAWS
        1.  f >=> returnFish =
            \m -> f m >>= returnFish =
            \m -> f m >>= return =
            \m -> f m =
            f
        2.  returnFish >=> f =
            \m -> returnFish m >>= f =
            \m -> return m >>= f =
            \m -> f a =
            f
        3.  f >=> (g >=> h)
            \m -> f m >>= (g >=> h) =
            \m -> f m >>= (\n -> g n >>= h) =
            \m -> (f m >>= g) >>= h =
            \m -> (f >=> g) m >>= h =
            (f >=> g) >=> h
    #-}

instance Monad m => MonadJoin m where
    returnJoin = return
    join m     = m >>= id

    {-# LAWS
        1.  (join . returnJoin) m =
            join (returnJoin m) =
            join (return m) =
            return m >>= id =
            id m
        2.  (join . fmap returnJoin) m =
            join (fmap returnJoin m) =
            join (fmap return m) =
            fmap return m >>= id =

        3.  (join . fmap join) m =
            join (fmap join m) =
            fmap join m >>= id =
            fmap
             join . join
        4*  join . fmap (fmap f)   ≡ fmap f . join
    #-}

instance MonadFish m => Monad m where
    return  = returnFish
    m >>= k = id >=> k $ m

    {-# LAWS
        1.  m >>= return =
            (id >=> return) m =
            (id >=> returnFish) m =
            id m =
            m
        2.  return a >>= f =
            (id >=> f) (return a) =
            (returnFish >=> id >=> f) a =

        3.  (m >>= f) >>= g ≡ m >>= (\x -> f x >>= g)
    #-}
