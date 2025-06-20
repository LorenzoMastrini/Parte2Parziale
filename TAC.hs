module TAC where

type Temp = String
type Label = String

data Operand
    = OpTemp Temp
    | OpInt Integer
    | OpDouble Double
    | OpBool Bool
    | OpChar Char
    | OpString String
    | OpVar String
  deriving (Show, Eq)

data TAC
    = TacAssign Temp Operand
    | TacBinOp Temp Operand String Operand
    | TacUnOp Temp String Operand
    | TacGoto Label
    | TacIf Operand String Operand Label
    | TacLabel Label
    | TacCall Temp String [Operand]
    | TacParam Operand
    | TacReturn (Maybe Operand)
    | TacNop
  deriving (Show, Eq)

