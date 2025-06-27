module TACabssynt (
    module TACabssynt
) where

import AbsLinguaggio

data TacType
    = TACInteger
    | TACReal
    | TACBoolean
    | TACCharacter
    | TACString -- rappresentato come [TACCharacter]
    | TACPointer
    | TACArray
    | TACError
    deriving (Show)


type TacLabel = Int  

data TacProgVariable = TacProgVar{
    varName :: SimpleIdent,
    varIsMutable :: Bool,
    varModality :: OptModality,
    varBasicType :: BasicType
} deriving (Show)

data Address
    = ProgVar {progVar :: TacProgVariable, addrT :: TacType}
    | TacLit {tacLit :: Literal, addrT :: TacType}
    | Temporary {tempInt :: Int, addrT :: TacType}
    deriving (Show)

data XAddr = Addr Address
           | ArrayAddr{base :: Address, offset :: Address}
           | RefAddr Address
           deriving (Show)

data TAC label
    = TacBinary Address Binary BasicType Address Address
    | TacUnary Address Unary BasicType Address
    | TacNullary Address BasicType Address
    | TacGoto TacLabel
    | TacIf Address TacLabel
    | TacIfFalse Address TacLabel
    | TacIfRel Address Binary Address TacLabel
    | TacIndexedLoad Address TacProgVariable Address
    | TacIndexedStore TacProgVariable Address Address
    | TacAddrOf Address TacProgVariable
    | TacAssign Address Address
    | TacDerefRead Address Address
    | TacDerefWrite Address Address
    | TacParam Address
    | TacPCall TacLabel Int
    | TacFCall Address TacLabel Int
    | TacReturn
    | TacReturnVal Address
    | TacAddLabel TacLabel

int2AddrTempName :: Int -> TacType -> Address
int2AddrTempName n t = Temporary n t






