module SemLinguaggio where

import AbsLinguaggio
import Data.Char (ord, readLitChar)
import Data.Map (Map, member,lookup,toList)
import qualified Data.Map as Map

type Env = Map Ident EnvEntry
data EnvEntry 
    = Evar Loc Type
    | Efun Loc [Parameter] Type
    | Econst Type
data Parameter = Parameter Loc OptModality Type Ident
data Loc = Loc {line, column :: Int}

