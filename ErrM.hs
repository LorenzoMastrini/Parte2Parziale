{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleInstances #-}

-- This module defines a type synonym 'Err' and pattern synonyms 'Bad' and 'Ok' for 'Left' and 'Right'

module ErrM where

import Prelude (id, const, Either(..), String)
import Control.Monad (MonadPlus(..), MonadFail(..))
import Control.Applicative (Alternative(..))



-- Error monad with "String" error messages
type Err = Either String

pattern Bad msg = Left msg
pattern Ok  a   = Right a


instance MonadFail Err where
  fail = Bad

instance Alternative Err where
  empty           = Left "Err.empty"
  (<|>) Left{}    = id
  (<|>) x@Right{} = const x

instance MonadPlus Err where
  mzero = empty
  mplus = (<|>)