-- Copyright 2016 Julian Hall.  See LICENSE file at top level for details.

{-# LANGUAGE DataKinds,MultiParamTypeClasses,ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies,KindSignatures,TypeFamilies #-}
{-# LANGUAGE FlexibleInstances,PolyKinds,GADTs,UndecidableInstances #-}

module Data.Type.HasClass where

import GHC.Exts(Constraint)
import Data.Proxy

-- | Stores a constraint context.  When pattern matched upon, the compiler
-- knows the constraint is true.
data TDict (c :: k -> Constraint) (t :: k) where
    TDict :: c t => TDict c t

-- | Identifies whether or not a type has an instance of a type class,
-- and provides a standard means for capturing a context that allows them
-- to be used.
-- Unfortunately, because instances cannot be selected on the basis of
-- whether a context matches, instances of this class need to be listed manually
-- whenever we need to modify behaviour depending on whether an instance
-- of a class is available or not.  If a class @c@ has an instance for type @t@
-- there should be an @instance HasClass c t True where classDict _ _ _  = TDict@.
-- For cases where additional context is required, see examples in the
-- @Data.Type.HasClassPreludeInstances@ module.
class HasClass (c :: k -> Constraint) (t :: k) (b :: Bool) | c t -> b where
    classDict :: Proxy c -> Proxy t -> Proxy b -> TDict c t

-- | Default instance for types and classes that do not have an available
-- instance.
instance {-# OVERLAPPABLE #-} False ~ b => HasClass c t b where
    classDict _ _ _ = undefined

-- | Combine type level 'Bool's.  @And a b c@ has an instance @And a b True@
-- if and only if both @a@ and @b@ are 'True', and @And a b False@ otherwise.
class And (a :: Bool) (b :: Bool) (c :: Bool) | a b -> c
instance And True b b
instance And False b False

