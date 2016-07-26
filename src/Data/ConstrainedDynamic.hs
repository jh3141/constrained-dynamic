-- Copyright 2016 Julian Hall.  See LICENSE file at top level for details.

-- TODO - are we still using all of these extensions?
{-# LANGUAGE GADTs,ConstraintKinds,RankNTypes,FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables,KindSignatures #-}
{-# LANGUAGE TypeFamilies,MultiParamTypeClasses,UndecidableInstances #-}

-- | Provides a container type similar to "Data.Dynamic" but which retains
-- information about a typeclass (or other constraint) that is known to
-- be available for the type of the object contained inside.
module Data.ConstrainedDynamic (
                 -- * Types
                 ClassConstraint,ConstrainedDynamic,
                 -- * Functions that mirror functions in Data.Dynamic
                 toDyn,fromDynamic,fromDyn,dynTypeRep,
                 -- * Extended API for managing and using class constraints
                 dynConstraintType,applyClassFn,classCast
                 )
    where

import Data.Typeable
import GHC.Exts (Constraint)
import Unsafe.Coerce

-- fixme should we use kind polyorphism here?
-- note that this is not exported as a similar definition is often used elsewhere
data TDict :: (* -> Constraint) -> * -> * where
    TDict :: cs t => TDict cs t

-- | A type used to represent class constraints as values.  This exists
-- primarily so that @typeOf (ClassConstraint :: ClassConstraint cs)@ can be
-- used to obtain a 'TypeRep' that uniquely identifies a typeclass.
data ClassConstraint (cs :: * -> Constraint) = ClassConstraint

-- | A type that contains a value whose type is unknown at compile time,
-- except that it satisfies a given constraint.  For example, a value of
-- @ConstrainedDynamic Show@ could contain a value of any type for which an
-- instance of the typeclass 'Show' is available.
data ConstrainedDynamic (cs :: * -> Constraint) where
    ConsDyn :: (Typeable a, cs a, Typeable cs) =>
               a -> TDict cs a -> ConstrainedDynamic cs

--
-- functions that mirror the functions in Data.Dynamic
--

-- | Create a 'ConstrainedDynamic' for a given value.  Note that this
-- function must be used in a context where the required constraint
-- type can be determined, for example by explicitly identifying the
-- required type using the form @toDyn value :: ConstrainedDynamic TypeClass@.
toDyn :: (Typeable a, cs a, Typeable cs) => a -> ConstrainedDynamic cs
toDyn obj = ConsDyn obj TDict

-- | Extract a value 'ConstrainedDynamic' to a particular type if and only if
-- the value contained with in it has that type, returning @'Just' v@ if the
-- value @v@ has the correct type or @'Nothing'@ otherwise,
fromDynamic :: (Typeable a, cs a) => ConstrainedDynamic cs -> Maybe a
fromDynamic (ConsDyn obj _) = cast obj

-- | Extract a value 'ConstrainedDynamic' to a particular type if and only if
-- the value contained with in it has that type, returning the value if it has
-- the correct type or a default value otherwise.
fromDyn :: (Typeable a, cs a) => ConstrainedDynamic cs -> a -> a
fromDyn d def = maybe def id $ fromDynamic d

-- | Return the 'TypeRep' for the type of value contained within a
-- 'ConstrainedDynamic'.
dynTypeRep :: ConstrainedDynamic cs -> TypeRep
dynTypeRep (ConsDyn obj _) = typeOf obj

-- extended API for handling constraints

-- | Return a 'TypeRep' that uniquely identifies the type of constraint
-- used in the 'ConstrainedDynamic'.  The actual type whose representation
-- is returned is @ClassConstraint c@ where @c@ is the constraint.
dynConstraintType :: forall a . Typeable a => ConstrainedDynamic a -> TypeRep
dynConstraintType _ = typeOf (ClassConstraint :: ClassConstraint a)

-- | Apply a polymorphic function that accepts all values matching the
-- appropriate constraint to the value stored inside a 'ConstrainedDynamic'
-- and return its result.  Note that this *must* be a polymorphic function
-- with only a single argument that is constrained by the constrain, so
-- for example the function 'show' from the typeclass 'Show' is allowable,
-- but '==' from the typeclass 'Eq' would not work as it requires a
-- second argument that has the same type as the first, and it is not
-- possible to safely return the partially-applied function as its type is
-- not known in the calling context.
applyClassFn :: ConstrainedDynamic cs -> (forall a . cs a => a -> b) -> b
applyClassFn (ConsDyn obj TDict) f = f obj

-- fixme: what about subtypes?

-- | If a 'ConstrainedDynamic' has an unknown constraint variable, 'classCast'
-- can be used to convert it to a 'ConstrainedDynamic' with a known constraint.
-- For example, @classCast d :: Maybe (ConstrainedDynamic Show)@ returns
-- @'Just' d :: Maybe (ConstrainedDynamic Show)@ if @d@s constraint was 'Show'
-- or 'Nothing' if it was any other constraint.
classCast :: forall a b . (Typeable a, Typeable b) =>
             ConstrainedDynamic a -> Maybe (ConstrainedDynamic b)
classCast d
    | dynConstraintType d == typeOf(ClassConstraint :: ClassConstraint b)
         = Just (unsafeCoerce d)
    | otherwise
         = Nothing

-- | An instance of 'Show' for 'ConstrainedDynamic': delegates to the
-- contained value's definition of 'showsPrec' if the constraint is
-- 'Show', or shows the type of the contained value otherwise.
instance Typeable cs => Show (ConstrainedDynamic cs) where
    showsPrec i d = case classCast d :: Maybe (ConstrainedDynamic Show) of
                      Just (ConsDyn obj TDict) -> showsPrec i obj
                      Nothing                  -> showsPrec i (dynTypeRep d)
