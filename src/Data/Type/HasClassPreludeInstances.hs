-- Copyright 2016 Julian Hall.  See LICENSE file at top level for details.

{-# LANGUAGE DataKinds,FlexibleInstances,MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts,TypeFamilies,UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes,ScopedTypeVariables #-}

-- | Provides instances of the type class 'HasClass' for common
-- single-parameter type classes contained in the Prelude.  Note that
-- instances for tuple types are *not* currently included due to the very large
-- number of somewhat complex classes that would be required.

module Data.Type.HasClassPreludeInstances where

import Data.Type.HasClass
import Data.Proxy

instance HasClass Bounded Word True where classDict _ _ _ = TDict
instance HasClass Bounded Ordering True where classDict _ _ _ = TDict
instance HasClass Bounded Int True where classDict _ _ _ = TDict
instance HasClass Bounded Char True where classDict _ _ _ = TDict
instance HasClass Bounded Bool True where classDict _ _ _ = TDict

instance HasClass Enum Word True where classDict _ _ _ = TDict
instance HasClass Enum Ordering True where classDict _ _ _ = TDict
instance HasClass Enum Integer True where classDict _ _ _ = TDict
instance HasClass Enum Int True where classDict _ _ _ = TDict
instance HasClass Enum Char True where classDict _ _ _ = TDict
instance HasClass Enum Bool True where classDict _ _ _ = TDict
instance HasClass Enum () True where classDict _ _ _ = TDict
instance HasClass Enum Float True where classDict _ _ _ = TDict
instance HasClass Enum Double True where classDict _ _ _ = TDict

instance HasClass Eq a f1 => HasClass Eq [a] f1 where
    classDict peq _ _ =
        case classDict peq (Proxy :: Proxy a) (Proxy :: Proxy f1) of
          TDict -> TDict
instance HasClass Eq Word True where classDict _ _ _ = TDict
instance HasClass Eq Ordering True where  classDict _ _ _ = TDict
instance HasClass Eq Int True where  classDict _ _ _ = TDict
instance HasClass Eq Float True where  classDict _ _ _ = TDict
instance HasClass Eq Double True where  classDict _ _ _ = TDict
instance HasClass Eq Char True where  classDict _ _ _ = TDict
instance HasClass Eq Bool True where  classDict _ _ _ = TDict
instance HasClass Eq a f1 => HasClass Eq (Maybe a) f1 where
    classDict peq _ _ =
        case classDict peq (Proxy :: Proxy a) (Proxy :: Proxy f1) of
          TDict -> TDict
instance (HasClass Eq a f1, HasClass Eq b f2, And f1 f2 f3) =>
    HasClass Eq (Either a b) f3 where
        classDict peq _ _ =
            case classDict peq (Proxy :: Proxy a) (Proxy :: Proxy f1) of
              TDict ->
                  case classDict peq (Proxy :: Proxy b) (Proxy :: Proxy f2) of
                    TDict -> TDict

instance HasClass Fractional Float True where classDict _ _ _ = TDict
instance HasClass Fractional Double True where classDict _ _ _ = TDict

instance HasClass Floating Float True where classDict _ _ _ = TDict
instance HasClass Floating Double True where classDict _ _ _ = TDict

instance HasClass Integral Word True where classDict _ _ _ = TDict
instance HasClass Integral Integer True where classDict _ _ _ = TDict
instance HasClass Integral Int True where classDict _ _ _ = TDict

instance HasClass Monoid [a] True where classDict _ _ _ = TDict
instance HasClass Monoid Ordering True where classDict _ _ _ = TDict
instance HasClass Monoid a f1 => HasClass Monoid (Maybe a) f1 where
    classDict p _ _ =
        case classDict p (Proxy :: Proxy a) (Proxy :: Proxy f1) of
          TDict -> TDict
instance HasClass Monoid b f1 => HasClass Monoid (a -> b) f1 where
    classDict p _ _ =
        case classDict p (Proxy :: Proxy b) (Proxy :: Proxy f1) of
          TDict -> TDict

instance HasClass Num Word True where classDict _ _ _ = TDict
instance HasClass Num Integer True where classDict _ _ _ = TDict
instance HasClass Num Int True where classDict _ _ _ = TDict
instance HasClass Num Float True where classDict _ _ _ = TDict
instance HasClass Num Double True where classDict _ _ _ = TDict

instance HasClass Ord a f1 => HasClass Ord [a] f1 where
    classDict p _ _ =
        case classDict p (Proxy :: Proxy a) (Proxy :: Proxy f1) of
          TDict -> TDict
instance HasClass Ord Word True where classDict _ _ _ = TDict
instance HasClass Ord Ordering True where classDict _ _ _ = TDict
instance HasClass Ord Int True where classDict _ _ _ = TDict
instance HasClass Ord Float True where classDict _ _ _ = TDict
instance HasClass Ord Double True where classDict _ _ _ = TDict
instance HasClass Ord Char True where classDict _ _ _ = TDict
instance HasClass Ord Bool True where classDict _ _ _ = TDict
instance HasClass Ord Integer True where classDict _ _ _ = TDict
instance (HasClass Ord a f1, HasClass Ord b f2, And f1 f2 f3) =>
    HasClass Ord (Either a b) f3 where
        classDict p _ _ =
            case classDict p (Proxy :: Proxy a) (Proxy :: Proxy f1) of
              TDict ->
                  case classDict p (Proxy :: Proxy b) (Proxy :: Proxy f2) of
                    TDict -> TDict
instance HasClass Ord a f1 => HasClass Ord (Maybe a) f1 where
    classDict p _ _ =
        case classDict p (Proxy :: Proxy a) (Proxy :: Proxy f1) of
          TDict -> TDict

instance HasClass Read a f1 => HasClass Read [a] f1 where
    classDict p _ _ =
        case classDict p (Proxy :: Proxy a) (Proxy :: Proxy f1) of
          TDict -> TDict
instance HasClass Read Word True where classDict _ _ _ = TDict
instance HasClass Read Ordering True where classDict _ _ _ = TDict
instance HasClass Read a f1 => HasClass Read (Maybe a) f1 where
    classDict p _ _ =
        case classDict p (Proxy :: Proxy a) (Proxy :: Proxy f1) of
          TDict -> TDict
instance HasClass Read Integer True where classDict _ _ _ = TDict
instance HasClass Read Int True where classDict _ _ _ = TDict
instance HasClass Read Float True where classDict _ _ _ = TDict
instance HasClass Read Double True where classDict _ _ _ = TDict
instance HasClass Read Char True where classDict _ _ _ = TDict
instance HasClass Read Bool True where classDict _ _ _ = TDict
instance (HasClass Read a f1, HasClass Read b f2, And f1 f2 f3) =>
    HasClass Read (Either a b) f3 where
        classDict p _ _ =
            case classDict p (Proxy :: Proxy a) (Proxy :: Proxy f1) of
              TDict ->
                  case classDict p (Proxy :: Proxy b) (Proxy :: Proxy f2) of
                    TDict -> TDict

instance HasClass Real Word True where classDict _ _ _ = TDict
instance HasClass Real Integer True where classDict _ _ _ = TDict
instance HasClass Real Int True where classDict _ _ _ = TDict
instance HasClass Real Float True where classDict _ _ _ = TDict
instance HasClass Real Double True where classDict _ _ _ = TDict

instance HasClass RealFloat Float True where classDict _ _ _ = TDict
instance HasClass RealFloat Double True where classDict _ _ _ = TDict

instance HasClass RealFrac Float True where classDict _ _ _ = TDict
instance HasClass RealFrac Double True where classDict _ _ _ = TDict

instance HasClass Show a f1 => HasClass Show [a] f1 where
    classDict p _ _ =
        case classDict p (Proxy :: Proxy a) (Proxy :: Proxy f1) of
          TDict -> TDict
instance HasClass Show Word True where classDict _ _ _ = TDict
instance HasClass Show Ordering True where classDict _ _ _ = TDict
instance HasClass Show a f1 => HasClass Show (Maybe a) f1 where
    classDict p _ _ =
        case classDict p (Proxy :: Proxy a) (Proxy :: Proxy f1) of
          TDict -> TDict
instance HasClass Show Integer True where classDict _ _ _ = TDict
instance HasClass Show Int True where classDict _ _ _ = TDict
instance HasClass Show Float True where classDict _ _ _ = TDict
instance HasClass Show Double True where classDict _ _ _ = TDict
instance HasClass Show Char True where classDict _ _ _ = TDict
instance HasClass Show Bool True where classDict _ _ _ = TDict
instance (HasClass Show a f1, HasClass Show b f2, And f1 f2 f3) =>
    HasClass Show (Either a b) f3 where
        classDict p _ _ =
            case classDict p (Proxy :: Proxy a) (Proxy :: Proxy f1) of
              TDict ->
                  case classDict p (Proxy :: Proxy b) (Proxy :: Proxy f2) of
                    TDict -> TDict
