-- Copyright 2016 Julian Hall.  See LICENSE file at top level for details.

{-# LANGUAGE DataKinds,MultiParamTypeClasses,ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies,KindSignatures,TypeFamilies #-}
{-# LANGUAGE FlexibleInstances,PolyKinds #-}

module Data.Type.HasClass where

import GHC.Exts(Constraint)
    
class HasClass (c :: k -> Constraint) (t :: k) (b :: Bool) | c t -> b where

instance {-# OVERLAPPABLE #-} HasClass c t False where

type family And (a :: Bool) (b :: Bool) where
    And True True = True
    And a b = False

type family And3 (a :: Bool) (b :: Bool) (c :: Bool) where
    And3 True True True = True
    And3 a b c = False

type family Or (a :: Bool) (b :: Bool) where
    Or False False = False
    Or a b = True

type family Or3 (a :: Bool) (b :: Bool) (c :: Bool) where
    Or3 False False False = False
    Or3 a b c = True

