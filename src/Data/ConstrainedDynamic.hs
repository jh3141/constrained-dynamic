{-# LANGUAGE GADTs #-}

module Data.ConstrainedDynamic where

import Data.Typeable

data ConstrainedDynamic constraints where
    ConsDyn :: Typeable a => a -> ConstrainedDynamic constraints

toDyn :: Typeable a => a -> ConstrainedDynamic b
toDyn obj = ConsDyn obj

fromDynamic :: Typeable a => ConstrainedDynamic b -> Maybe a
fromDynamic (ConsDyn obj) = cast obj
