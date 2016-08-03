-- Copyright 2016 Julian Hall.  See LICENSE file at top level for details.

-- TODO - are we still using all of these extensions?
{-# LANGUAGE GADTs,ConstraintKinds,RankNTypes,FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables,KindSignatures,PolyKinds #-}
{-# LANGUAGE TypeFamilies,MultiParamTypeClasses,UndecidableInstances #-}
{-# LANGUAGE DataKinds,TypeOperators,FlexibleContexts #-}

-- | Provides a container type similar to "Data.Dynamic" but which retains
-- information about a list of typeclass (or other constraint) that are known to
-- be available for the type of the object contained inside.
module Data.MultiConstrainedDynamic {- (
                 -- * Types
                 ClassConstraint(..),ConstrainedDynamic,
                 -- * Functions that mirror functions in Data.Dynamic
                 toDyn,fromDynamic,fromDyn,dynTypeRep,
                 -- * Extended API for managing and using class constraints
                 dynConstraintType,applyClassFn,classCast
                 ) -}
    where

import Data.Typeable
import GHC.Exts (Constraint)
import Unsafe.Coerce
import Data.ConstrainedDynamic(ClassConstraint(..))
import Data.Type.HasClass

data MaybeHasClass :: (* -> Constraint) -> * -> * where
    JustHasClass     :: cs t => MaybeHasClass cs t
    DoesNotHaveClass :: MaybeHasClass cs t

data LTDict :: [* -> Constraint] -> * -> * where
    LTDCons :: MaybeHasClass cs t -> LTDict css t -> LTDict (cs ': css) t
    LTDNil  :: LTDict '[] t

-- | A type that contains a value whose type is unknown at compile time,
-- but for which it is known whether or not it satisfies any of a list of
-- constraints, thus allowing operations to be performed when those
-- constraints are satisfied.
data MCDynamic (cs :: [* -> Constraint]) where
    ConsMCD :: (Typeable a, Typeable cs) =>
               a -> LTDict cs a -> MCDynamic cs

--
-- internal functions
--

class TypeConstraintBuilder mhctype (cs :: * -> Constraint) t (flag :: k) where
    buildTypeConstraint :: proxy flag -> mhctype cs t

instance (HasClass cs t True) =>
    TypeConstraintBuilder MaybeHasClass cs t True where
        buildTypeConstraint _ =
            case classDict (Proxy :: Proxy cs) (Proxy :: Proxy t)
                           (Proxy :: Proxy true) of
              TDict -> JustHasClass

instance {-# OVERLAPPABLE #-} f ~ False =>
    TypeConstraintBuilder MaybeHasClass cs t f where
        buildTypeConstraint _ = DoesNotHaveClass

checkClass :: forall cs t f .
              (HasClass cs t f, TypeConstraintBuilder MaybeHasClass cs t f) =>
              MaybeHasClass cs t
checkClass = buildTypeConstraint (Proxy :: Proxy f)

class LTDictBuilder dtype (css :: [(* -> Constraint)]) t where
    buildLTDict :: dtype css t

instance (HasClass cs t f,
          TypeConstraintBuilder MaybeHasClass cs t f,
          LTDictBuilder LTDict css t)
              => LTDictBuilder LTDict (cs ': css) t where
    buildLTDict = LTDCons checkClass (buildLTDict :: LTDict css t)

instance LTDictBuilder LTDict '[] t where
    buildLTDict = LTDNil

class LTDictConstraintLister (css :: [(* -> Constraint)]) where
    getAllConstraints :: LTDict css a -> [TypeRep]
    getMatchedConstraints :: LTDict css a -> [TypeRep]
    getUnmatchedConstraints :: LTDict css a -> [TypeRep]

instance LTDictConstraintLister '[] where
    getAllConstraints _ = []
    getMatchedConstraints _ = []
    getUnmatchedConstraints _ = []

instance (Typeable cs, LTDictConstraintLister css) => LTDictConstraintLister (cs ': css) where
    getAllConstraints (LTDCons _ t) =
        typeOf (ClassConstraint :: ClassConstraint cs) :
               getAllConstraints t
    getMatchedConstraints (LTDCons JustHasClass t) =
        typeOf (ClassConstraint :: ClassConstraint cs) :
               getMatchedConstraints t
    getMatchedConstraints (LTDCons DoesNotHaveClass t) =
        getMatchedConstraints t
    getUnmatchedConstraints (LTDCons DoesNotHaveClass t) =
        typeOf (ClassConstraint :: ClassConstraint cs) :
               getUnmatchedConstraints t
    getUnmatchedConstraints (LTDCons JustHasClass t) =
        getUnmatchedConstraints t

class LTDictSearch (css :: [(* -> Constraint)]) (cs :: * -> Constraint) where
    ltdSearch :: proxy cs -> LTDict css a -> MaybeHasClass cs a

instance LTDictSearch '[] cs where
    ltdSearch _ _ = DoesNotHaveClass

instance LTDictSearch (cs ': css) cs where
    ltdSearch _ (LTDCons m _) = m

instance {-# OVERLAPPABLE #-} LTDictSearch css cs =>
    LTDictSearch (unmatched ': css) cs where
        ltdSearch p (LTDCons _ t) = ltdSearch p t

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
