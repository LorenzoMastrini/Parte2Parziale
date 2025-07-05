module TACabssynt where

import AbsLinguaggio



type TACLabel = Int

data TACType
    = TACInteger
    | TACReal
    | TACBoolean
    | TACCharacter
    | TACAddr
    | TACError
    deriving (Show)

data XAddr
    = Addr Address
    | ArrayAddr { base :: Address, offset :: Address }
    | RefAddr Address
    deriving (Show)

data Address
    = ProgVar { progVar :: TacProgVariable, addrT :: Maybe TACType }
    | TACLit { tacLit :: Literal, addrT :: Maybe TACType }
    | Temporary { tempInt :: Int, addrT :: Maybe TACType }
    deriving (Show)

data TacProgVariable = TacProgVar {
    varName :: SimpleIdent,
    varIsMutable :: Bool,
    varModality :: OptModality,
    varBasicType :: BasicType
} deriving (Show)

data TAC label
    = TACBinary Address Binary BasicType Address Address
    | TACUnary Address Unary BasicType Address
    | TACNullary Address BasicType Address
    | TACGoto TACLabel
    | TACIf Address TACLabel
    | TACIfFalse Address TACLabel
    | TACIfRel Address Binary Address TACLabel
    | TACIndexedLoad Address TacProgVariable Address
    | TACIndexedStore TacProgVariable Address Address
    | TACAddrOf Address TacProgVariable
    | TACAssign Address Address
    | TACDerefRead Address Address
    | TACDerefWrite Address Address
    | TACParam Address
    | TACPCall TACLabel Int
    | TACFCall Address TACLabel Int
    | TACReturn
    | TACReturnVal Address
    | TACAddLabel TACLabel
    | TACComment String