{-# LANGUAGE GADTs,ConstraintKinds,RankNTypes,FlexibleInstances #-}
{-# LANGUAGE TypeFamilies,MultiParamTypeClasses,UndecidableInstances #-}

module Data.ConstrainedDynamic where

import Data.Typeable
import GHC.Exts (Constraint)

data ConstrainedDynamic cs where
    ConsDyn :: (Typeable a, cs a) => a -> ConstrainedDynamic cs

toDyn :: (Typeable a, cs a) => a -> ConstrainedDynamic cs
toDyn obj = ConsDyn obj

fromDynamic :: (Typeable a, cs a) => ConstrainedDynamic cs -> Maybe a
fromDynamic (ConsDyn obj) = cast obj

applyClassFn :: ConstrainedDynamic cs -> (forall a.cs a => a -> b) -> b
applyClassFn (ConsDyn obj) f = f obj

data MaybeHasClass (cs :: * -> Constraint) where
    JustHasClass     :: cs a => a -> MaybeHasClass cs
    DoesNotHaveClass :: MaybeHasClass cs

maybeApplyClassFn :: (forall a . cs a => a -> b) -> b -> MaybeHasClass cs -> b
maybeApplyClassFn fn _ (JustHasClass a) = fn a
maybeApplyClassFn _ def DoesNotHaveClass = def

class ClassCastable (csFrom :: * -> Constraint)
                    (csTo :: * -> Constraint)
                    (container :: (* -> Constraint) -> *) where
    classCast :: container csFrom -> MaybeHasClass csTo

instance ClassCastable a a ConstrainedDynamic where
    classCast (ConsDyn obj) = JustHasClass obj
instance {-# OVERLAPPABLE #-} ClassCastable a b ConstrainedDynamic where
    classCast (ConsDyn _) = DoesNotHaveClass

