module TAC where

import AbsLinguaggio

-- type Temp = String
-- type Label = String

type TacType = BasicType

data TacProgVariable = TacProgVar{
  varName :: Ident,
  varIsMutable :: Bool,
  varModality :: OptModality,
  varBasicType :: BasicType
}deriving(Show)

data Address  = ProgVar {progVar :: TacProgVariable, addrT :: TacType}
              | TacLit{tacLit :: Literal, addrT :: TacType}
              | Temporary{tempInt :: Int, addrT :: TacType}

data XAddr  = Addr Address 
            | ArrayAddr{base :: Address, offset :: Address}
            | RefAddr Address


data TAC pn
      = TacBinary Address Binary BasicType Address Address
      | TacUnary  Address Unary BasicType Address
      | TacNullary Address BasicType Address
      | TacGoto pn
      | TacIf Address pn
      | TacIfFalse Address pn
      | TacIfRel Address Binary Address pn
      | TacIndexedLoad Address TacProgVariable Address
      | TacindexedStore TacProgVariable Address Address
      | TaxAddrOf Address TacProgVariable
      | TacAssigA Address Address
      | TacDerefRead Address Address
      | TacDerefWrite Address Address
      | TacParam Address
      | TacPCall pn Int
      | TacFCall Address pn Int
      | TacReturn
      | TacReturnVal Address
