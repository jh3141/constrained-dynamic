-- Copyright 2016 Julian Hall.  See LICENSE file at top level for details.

-- TODO - are we still using all of these extensions?
{-# LANGUAGE GADTs,ConstraintKinds,RankNTypes,FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables,KindSignatures,PolyKinds #-}
{-# LANGUAGE TypeFamilies,MultiParamTypeClasses,UndecidableInstances #-}
{-# LANGUAGE DataKinds,TypeOperators,FlexibleContexts #-}

-- | Provides a type for keeping a list of typeclass dictionaries that may or
-- may not exist for a given type, and utility functions for working with the
-- list.
module Data.Type.LTDict where

import GHC.Exts (Constraint)
import Data.ConstrainedDynamic(ClassConstraint(..))
import Data.Typeable
import Data.Type.HasClass

-- | Optionally provide a dictionary for a constraint and a type.
data MaybeHasClass :: (* -> Constraint) -> * -> * where
    JustHasClass     :: cs t => MaybeHasClass cs t
    DoesNotHaveClass :: MaybeHasClass cs t

-- | Maintain a list of dictionaries encapsulating many constraints
-- over a single type (where they are available).
data LTDict :: [* -> Constraint] -> * -> * where
    LTDCons :: MaybeHasClass cs t -> LTDict css t -> LTDict (cs ': css) t
    LTDNil  :: LTDict '[] t

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

-- | Produce an appropriate 'MaybeHasClass' value for the context.
-- Callers should ensure that appropriate 'HasClass' instances are in scope
-- at the point of calling, or declare them as part of their context.
checkClass :: forall cs t f .
              (HasClass cs t f, TypeConstraintBuilder MaybeHasClass cs t f) =>
              MaybeHasClass cs t
checkClass = buildTypeConstraint (Proxy :: Proxy f)

-- | A class for building objects that recurse over a list of constraints.
-- Instances are provided for building 'LTDict' values, but could be used
-- for other values too.
class LTDictBuilder dtype (css :: [(* -> Constraint)]) t where
    buildLTDict :: dtype css t

instance (HasClass cs t f,
          TypeConstraintBuilder MaybeHasClass cs t f,
          LTDictBuilder LTDict css t)
              => LTDictBuilder LTDict (cs ': css) t where
    buildLTDict = LTDCons checkClass (buildLTDict :: LTDict css t)

instance LTDictBuilder LTDict '[] t where
    buildLTDict = LTDNil

-- | Functions for extracting the list of constraints in an LTDict
-- as 'TypeRep's of 'ClassConstraint' types.
class LTDictConstraintLister (css :: [(* -> Constraint)]) where
    -- Return all constraints in the list
    getAllConstraints :: LTDict css a -> [TypeRep]
    -- Return only the matched constraints in the list
    getMatchedConstraints :: LTDict css a -> [TypeRep]
    -- Return only the unmatched constraints in the list
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

-- | A class for handling searches for a specific single constraint in the
-- list.
class LTDictSearch (css :: [(* -> Constraint)]) (cs :: * -> Constraint) where
    ltdSearch :: proxy cs -> LTDict css a -> MaybeHasClass cs a

instance LTDictSearch '[] cs where
    ltdSearch _ _ = DoesNotHaveClass

instance LTDictSearch (cs ': css) cs where
    ltdSearch _ (LTDCons m _) = m

instance {-# OVERLAPPABLE #-} LTDictSearch css cs =>
    LTDictSearch (unmatched ': css) cs where
        ltdSearch p (LTDCons _ t) = ltdSearch p t

