module TACabssynt where

import AbsLinguaggio

data TACType
    = TACInteger
    | TACReal
    | TACBoolean
    | TACCharacter
    | TACString [TACType]-- rappresentato come [TACCharacter]
    | TACPointer
    | TACArray
    | TACError
    deriving (Show)

type TACLabel = Int

data Address
    = ProgVar { progVar :: TacProgVariable, addrT :: TACType }
    | TacLit { tacLit :: Literal, addrT :: TACType }
    | Temporary { tempInt :: Int, addrT :: TACType }
    deriving (Show)

data TacProgVariable = TacProgVar {
    varName :: SimpleIdent,
    varIsMutable :: Bool,
    varModality :: OptModality,
    varBasicType :: BasicType
} deriving (Show)

data XAddr
    = Addr Address
    | ArrayAddr{base :: Address, offset :: Address}
    | RefAddr Address
    deriving (Show)

data TAC label
    = TACBinaryAss Address Binary BasicType Address Address
    | TACUnaryAss Address Unary BasicType Address
    | TACNullaryAss Address BasicType Address
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
    -- | TACRefAss      -- “l = &id ”
    -- | TACRDerefAss   -- “l1 = ∗l2 ”
    -- | TACLDerefAss   -- “∗l = r” 

int2AddrTempName :: Int -> TACType -> Address
int2AddrTempName n t = Temporary n t