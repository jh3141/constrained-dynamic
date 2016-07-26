{-# LANGUAGE GADTs,ConstraintKinds,RankNTypes,FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables,KindSignatures #-}
{-# LANGUAGE TypeFamilies,MultiParamTypeClasses,UndecidableInstances #-}

module Data.ConstrainedDynamic where

import Data.Typeable
import GHC.Exts (Constraint)
import Unsafe.Coerce

-- fixme should we use kind polyorphism here?
data TDict :: (* -> Constraint) -> * -> * where
    TDict :: cs t => TDict cs t

data ClassConstraint (cs :: * -> Constraint) = ClassConstraint

data ConstrainedDynamic (cs :: * -> Constraint) where
    ConsDyn :: (Typeable a, cs a, Typeable cs) =>
               a -> TDict cs a -> ConstrainedDynamic cs

-- functions that mirror the functions in Data.Dynamic
toDyn :: (Typeable a, cs a, Typeable cs) => a -> ConstrainedDynamic cs
toDyn obj = ConsDyn obj TDict

fromDynamic :: (Typeable a, cs a) => ConstrainedDynamic cs -> Maybe a
fromDynamic (ConsDyn obj _) = cast obj

dynTypeRep :: ConstrainedDynamic cs -> TypeRep
dynTypeRep (ConsDyn obj _) = typeOf obj

-- utility functions

dynConstraintType :: forall a . Typeable a => ConstrainedDynamic a -> TypeRep
dynConstraintType _ = typeOf (ClassConstraint :: ClassConstraint a)

-- extended API for handling constraints

applyClassFn :: ConstrainedDynamic cs -> (forall a . cs a => a -> b) -> b
applyClassFn (ConsDyn obj TDict) f = f obj

-- fixme: what about subtypes?
classCast :: forall a b . (Typeable a, Typeable b) =>
             ConstrainedDynamic a -> Maybe (ConstrainedDynamic b)
classCast d
    | dynConstraintType d == typeOf(ClassConstraint :: ClassConstraint b)
         = Just (unsafeCoerce d)
    | otherwise
         = Nothing

instance Typeable cs => Show (ConstrainedDynamic cs) where
    showsPrec i d = case classCast d :: Maybe (ConstrainedDynamic Show) of
                      Just (ConsDyn obj TDict) -> showsPrec i obj
                      Nothing                  -> showsPrec i (dynTypeRep d)
