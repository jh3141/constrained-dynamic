-- Copyright 2016 Julian Hall.  See LICENSE file at top level for details.

{-# LANGUAGE DataKinds,MultiParamTypeClasses,ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies,KindSignatures,TypeFamilies #-}
{-# LANGUAGE FlexibleInstances,PolyKinds,GADTs,UndecidableInstances #-}

module Data.Type.HasClass where

import GHC.Exts(Constraint)
import Data.Proxy

data TDict (c :: k -> Constraint) (t :: k) where
    TDict :: c t => TDict c t

class HasClass (c :: k -> Constraint) (t :: k) (b :: Bool) | c t -> b where
    classDict :: Proxy c -> Proxy t -> Proxy b -> TDict c t

instance {-# OVERLAPPABLE #-} False ~ b => HasClass c t b where
    classDict _ _ _ = undefined

class And (a :: Bool) (b :: Bool) (c :: Bool) | a b -> c
instance And True b b
instance And False b False

