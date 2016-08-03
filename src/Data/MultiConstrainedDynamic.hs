-- Copyright 2016 Julian Hall.  See LICENSE file at top level for details.

-- TODO - are we still using all of these extensions?
{-# LANGUAGE GADTs,ConstraintKinds,RankNTypes,FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables,KindSignatures,PolyKinds #-}
{-# LANGUAGE TypeFamilies,MultiParamTypeClasses,UndecidableInstances #-}
{-# LANGUAGE DataKinds,TypeOperators,FlexibleContexts #-}

-- | Provides a container type similar to "Data.Dynamic" but which retains
-- information about a list of typeclass (or other constraint) that are known to
-- be available for the type of the object contained inside.
module Data.MultiConstrainedDynamic (
                 -- * Types
                 ClassConstraint(..),MCDynamic,
                 -- * Functions that mirror functions in Data.Dynamic
                 toDyn,fromDynamic,fromDyn,dynTypeRep,
                 -- * Extended API for managing and using class constraints
                 dynConstraintTypes,dynAllConstraintTypes,
                 dynUnmatchedConstraintTypes,applyClassFn
                 )
    where

import Data.Typeable
import GHC.Exts (Constraint)
import Unsafe.Coerce
import Data.ConstrainedDynamic(ClassConstraint(..))
import Data.Type.HasClass
import Data.Type.LTDict

-- | A type that contains a value whose type is unknown at compile time,
-- but for which it is known whether or not it satisfies any of a list of
-- constraints, thus allowing operations to be performed when those
-- constraints are satisfied.
data MCDynamic (cs :: [* -> Constraint]) where
    ConsMCD :: (Typeable a, Typeable cs) =>
               a -> LTDict cs a -> MCDynamic cs

--
-- functions that mirror the functions in Data.Dynamic
--

-- | Create an 'MCDynamic' for a given value.  Note that this
-- function must be used in a context where the required list of constraint
-- types can be determined, for example by explicitly identifying the
-- required type using the form @toDyn value :: MCDynamic [TypeClass,...]@.
toDyn :: (Typeable a, Typeable cs, LTDictBuilder LTDict cs a) =>
         a -> MCDynamic (cs :: [* -> Constraint])
toDyn obj = ConsMCD obj buildLTDict

-- | Extract a value 'MCDynamic' to a particular type if and only if
-- the value contained with in it has that type, returning @'Just' v@ if the
-- value @v@ has the correct type or @'Nothing'@ otherwise,
fromDynamic :: Typeable a => MCDynamic css -> Maybe a
fromDynamic (ConsMCD obj _) = cast obj

-- | Extract a value 'MCDynamic' to a particular type if and only if
-- the value contained with in it has that type, returning the value if it has
-- the correct type or a default value otherwise.
fromDyn :: Typeable a => MCDynamic cs -> a -> a
fromDyn d def = maybe def id $ fromDynamic d

-- | Return the 'TypeRep' for the type of value contained within a
-- 'MCDynamic'.
dynTypeRep :: MCDynamic cs -> TypeRep
dynTypeRep (ConsMCD obj _) = typeOf obj

-- extended API for handling constraints

-- | Return a list of 'TypeRep's that uniquely identify the constraints
-- from an 'MCDynamic's type arguments that are satisfied by the value
-- contained inside the 'MCDynamic'.  The actual type whose representation
-- is returned for each constraint is is @ClassConstraint c@ where @c@ is
-- the constraint.
dynConstraintTypes :: LTDictConstraintLister a  => MCDynamic a -> [TypeRep]
dynConstraintTypes (ConsMCD _  ltd) = getMatchedConstraints ltd

-- | Return a list of 'TypeRep's that uniquely identify the constraints
-- from an 'MCDynamic's type arguments, whether or not they are satisfied by
-- the value contained inside the 'MCDynamic'.  The actual type whose
-- representation is returned for each constraint is is @ClassConstraint c@
-- where @c@ is the constraint.
dynAllConstraintTypes :: LTDictConstraintLister a => MCDynamic a -> [TypeRep]
dynAllConstraintTypes (ConsMCD _  ltd) = getAllConstraints ltd

-- | Return a list of 'TypeRep's that uniquely identify the constraints
-- from an 'MCDynamic's type arguments, that are not satisfied by
-- the value contained inside the 'MCDynamic'.  The actual type whose
-- representation is returned for each constraint is is @ClassConstraint c@
-- where @c@ is the constraint.
dynUnmatchedConstraintTypes :: LTDictConstraintLister a =>
                               MCDynamic a -> [TypeRep]
dynUnmatchedConstraintTypes (ConsMCD _  ltd) = getUnmatchedConstraints ltd


-- | Apply a polymorphic function that accepts all values matching a
-- constraint to the value stored inside an 'MCDynamic' wherever possible.
-- If the constraint is satisfied, returns @'Just' r@, where r is the result
-- of the function.  If the constraint is not satisfied, returns 'Nothing'.
--
-- Note that the function *must* be a polymorphic function
-- with only a single argument that is constrained by the constraint, so
-- for example the function 'show' from the typeclass 'Show' is allowable,
-- but '==' from the typeclass 'Eq' would not work as it requires a
-- second argument that has the same type as the first, and it is not
-- possible to safely return the partially-applied function as its type is
-- not known in the calling context.
applyClassFn :: LTDictSearch css cs => MCDynamic css -> ClassConstraint cs ->
                (forall a . (cs a,Typeable a) => a -> b) -> Maybe b
applyClassFn (ConsMCD obj ltd) pcs f =
    case ltdSearch pcs ltd of
      JustHasClass -> Just $ f obj
      DoesNotHaveClass -> Nothing

-- To do: cast MCDynamics between different constraint lists

-- | An instance of 'Show' for 'MCDynamic': delegates to the
-- contained value's definition of 'showsPrec' if an instance of Show
-- is available, or displays the type of the item and its list of available
-- instances otherwise.
instance (LTDictSearch css Show, LTDictConstraintLister css) =>
    Show (MCDynamic css) where
        showsPrec i d@(ConsMCD obj ltd) =
            case ltdSearch (ClassConstraint :: ClassConstraint Show) ltd of
              JustHasClass -> showsPrec i obj
              DoesNotHaveClass -> showsPrec i (dynTypeRep d) .
                                  showString " " .
                                  showsPrec i (dynConstraintTypes d)

-- | An instance of 'Eq' for 'MCDynamic': unwrap the right hand side
-- to the same type as the left (if possible) and if it matches, delegate to the
-- left hand item's instance of 'Eq' if one is available.  If either condition
-- fails, return 'False'.
instance LTDictSearch css Eq => Eq (MCDynamic css) where
    (ConsMCD objL ltd) == (ConsMCD objR _) =
        case cast objR of
          Nothing    -> False
          Just objRT ->
              case ltdSearch (ClassConstraint :: ClassConstraint Eq) ltd of
                JustHasClass     -> objL == objRT
                DoesNotHaveClass -> False
