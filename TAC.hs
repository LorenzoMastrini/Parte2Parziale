module TAC where

import AbsLinguaggio


data TAC
  = TacBinary Operand String Operand Operand -- dest = op1 `String` op2
  | TacUnary String Operand Operand
  | TacAssign Operand Operand              -- dest = src
  | TacLabel String
  | TacGoto String
  | TacIfZ Operand String
  | TacReturn Operand
  | TacUnimplemented String  -- ^ Used when something isn't implemented
  | TacCall Operand [Operand] Operand Loc
  | TacArrayAccess Operand Operand [Operand]
  | TacDeref Operand Operand
  | TacComment String
  | TacArg Operand
  deriving (Show)

data Operand
  = Temp Int
  | ConstInt Integer
  | ConstDouble Double
  | ConstChar Char
  | ConstBool Bool
  | Var String
  deriving (Show, Eq)