-- Copyright 2016 Julian Hall.  See LICENSE file at top level for details.

{-# LANGUAGE DataKinds,FlexibleInstances,MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts,TypeFamilies,UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes,ScopedTypeVariables #-}

module Data.Type.HasClassPreludeInstances where

import Data.Type.HasClass
import Data.Proxy
    
instance HasClass Functor (Either b) True where classDict _ _ _ = TDict
instance HasClass Functor [] True where classDict _ _ _ = TDict
instance HasClass Functor Maybe True where classDict _ _ _ = TDict
instance HasClass Functor IO True where classDict _ _ _ = TDict
instance HasClass Functor ((->) r) True where classDict _ _ _ = TDict
instance HasClass Functor ((,) b) True where classDict _ _ _ = TDict

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
    
instance HasClass Eq Integer True where classDict _ _ _ = TDict
instance (HasClass Eq a f1, HasClass Eq b f2, And f1 f2 f3) =>
    HasClass Eq (Either a b) f3 where
        classDict peq _ pt =
            case classDict peq (Proxy :: Proxy a) (Proxy :: Proxy f1) of
              TDict ->
                  case classDict peq (Proxy :: Proxy b) (Proxy :: Proxy f2) of
                    TDict -> TDict
