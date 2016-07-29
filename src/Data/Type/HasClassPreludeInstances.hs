-- Copyright 2016 Julian Hall.  See LICENSE file at top level for details.

{-# LANGUAGE DataKinds,FlexibleInstances,MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts,TypeFamilies #-}

module Data.Type.HasClassPreludeInstances where

import Data.Type.HasClass

instance HasClass Functor (Either b) True where
instance HasClass Functor [] True where
instance HasClass Functor Maybe True where
instance HasClass Functor IO True where
instance HasClass Functor ((->) r) True where
instance HasClass Functor ((,) b) True where

instance HasClass Bounded Word True where
instance HasClass Bounded Ordering True where
instance HasClass Bounded Int True where
instance HasClass Bounded Char True where
instance HasClass Bounded Bool True where

instance HasClass Enum Word True where
instance HasClass Enum Ordering True where
instance HasClass Enum Integer True where
instance HasClass Enum Int True where
instance HasClass Enum Char True where
instance HasClass Enum Bool True where
instance HasClass Enum () True where
instance HasClass Enum Float True where
instance HasClass Enum Double True where
    
instance HasClass Eq Integer True where
instance (HasClass Eq a c1, HasClass Eq a c2, c3 ~ And c1 c2) =>
         HasClass Eq (Either a b) c3 where
