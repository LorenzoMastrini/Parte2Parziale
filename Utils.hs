module Utils where

import Data.ByteString.Char8 (unpack, ByteString, pack) 
import Data.Map (Map, member, insert, lookup)
import qualified Data.Map as Map
import Data.Char
import AbsLinguaggio
import Data.List

--------------------------------------------------------------------
------------------------- DATA TYPES -------------------------------
--------------------------------------------------------------------

type Pos = (Int, Int)

type Id = String

data TACAddr  = Addr String | PointAddr String
  | DerefAddr String | AddrAt String TACAddr
  | ValAddr String
  | NoAddr
  deriving (Show, Eq)
data TACLabel = Label String | Fall | NoLab         deriving (Show, Eq)
type TACOp = String

data TAC
  = TACAssign TACAddr Type TACAddr                -- a1 = a2
  | TACUnOp TACAddr TACOp Type TACAddr            -- a1 = op a2
  | TACBinOp TACAddr TACAddr TACOp Type TACAddr   -- a1 = a2 op a3
  | TACGoto TACLabel                              -- goto l
  | TACAddLabel TACLabel                          -- l :
  | TACIf TACAddr TACLabel                        -- if a goto l
  | TACIfFalse TACAddr TACLabel                   -- ifFalse a goto l
  | TACIfBinary TACAddr Binary Type TACAddr TACLabel    -- if a1 op a2 goto l
  | TACParam TACAddr                              -- param a
  | TACCallProc TACLabel                          -- call p / n
  | TACCallFunc TACAddr Type TACLabel             -- a = call f / n
  | TACReturn TACAddr                             -- return a
  | TACException TACLabel                         -- onexceptiongoto l
  | TACExit                                       -- halt
  | TACComment String                             -- // comment
  | TACStatic TACAddr String                      -- straddr string
  | TACCode
  | TACStaticData
  deriving (Show)


--------------------------------------------------------------------
-------------------------  HELPERS ---------------------------------
--------------------------------------------------------------------

--------------------------------------------------------------------
--------------------------- TYPES ----------------------------------

leqType :: Type -> Type -> Bool
leqType ty1 ty2 =
    case ty1 of
        Basic b1 ->
            case ty2 of
                Basic b2 -> leq b1 b2 
                ErrorType    -> True
                otherwise    -> False
        ArrayTest aty1 adim1 ->
            case ty2 of
                ArrayTest aty2 adim2 -> (leqType aty1 aty2 && adim2==adim1) ||
                                    containsError aty1 || containsError aty2
                ErrorType    -> True
                otherwise    -> False
        Pointer b1 ->
            case ty2 of
                Pointer b2 ->  b1 == b2
                ErrorType    -> True
                otherwise      -> False
        ErrorType -> True
        VoidType -> False

containsError :: Type -> Bool
containsError ty =
  case ty of
      ErrorType   -> True
      ArrayTest tty _ -> containsError tty
      otherwise   -> False


equalitySupport :: Type -> Bool
equalitySupport typeE1 = case typeE1 of 
                                ArrayTest _ _ _ -> False
                                ArrayTest _ _ -> False
                                Pointer _ -> False
                                otherwise -> True

leq :: BasicType -> BasicType -> Bool
leq ty1 ty2 | ty1 == ty2 = True
                 | ty1 == BasicType_Integer && ty2 == BasicType_Real = True
                 | ty1 == BasicType_Character && ty2 == BasicType_Integer = True
                 | ty1 == BasicType_Character && ty2 == BasicType_String = True
                 | ty1 == BasicType_Character && ty2 == BasicType_Real = True
                 | otherwise = False

getMax :: BasicType -> BasicType -> BasicType
getMax ty1 ty2 | leqBasicType ty1 ty2 = ty2
                    | leq ty2 ty2 = ty1

getMaxType :: Type -> Type -> Type
getMaxType ty1 ty2 | leqType ty1 ty2 = ty2
                   | leqType ty2 ty2 = ty1

areCompatibleTypes :: Type -> Type -> Bool
areCompatibleTypes ty1 ty2 | leqType ty1 ty2 = True
                           | leqType ty2 ty1 = True
                           | otherwise = False

isAddressType :: Type -> Bool
isAddressType (Basic BasicType_String) = True
isAddressType (Pointer _) = True
isAddressType (ArrayTest _ _) = True
isAddressType _ = False

derefType :: Type -> Type
derefType (Pointer ty) = ty

pointerToType :: Type -> Type
pointerToType ty = Pointer ty

getArrayTestElemType :: Type -> Type
getArrayTestElemType (ArrayTest ty _) = ty
getArrayTestElemType t = error (show t)

getBasic :: Type -> Type
getBasic ty = case ty of
  Basic _ -> ty
  ArrayTest _ t _ -> getBasic t
  -- ArrayTestDimType _ t _ _ -> getBasic t

  Pointer t -> getBasic t

isBasic :: Type -> Bool
isBasic (Basic _) = True
isBasic _ = False

areErrors :: Type -> Type -> Bool
areErrors ty1 ty2 = isError ty1 || isError ty2

isError :: Type -> Bool
isError ty1  = (ty1 == ErrorType) || case ty1 of 
                                     ArrayTest ErrorType _ ->True
                                     otherwise -> False

numericalComp :: Type -> Bool
numericalComp ty1
          | areCompatibleTypes (Basic BasicType_Integer) ty1 &&
            ty1 /= ErrorType  = True
          | otherwise = False

comparableTypes :: Type -> Type -> Bool
comparableTypes ty1 ty2
            | areCompatibleTypes ty1 ty2  &&
              isTy1 && isBasicTypeTy2 = True
            | ty1 == (Basic BasicType_Character) && ty2 == ty1 = True
            | otherwise = False
            where isTy1 = case ty1 of
                            (Basic BasicType_Integer) -> True
                            (Basic BasicType_Real) -> True
                            (Basic BasicType_Character) -> True
                            otherwise -> False
                  isTy2 = case ty2 of
                            (Basic BasicType_Integer) -> True
                            (Basic BasicType_Real) -> True
                            (Basic BasicType_Character) -> True
                            otherwise -> False


operationReturnType :: Type -> Type -> String -> Type
operationReturnType ty1 ty2 op
    | op == "/" = Basic BasicType_Real
    | op == "^" && (leqType ty1 (Basic BasicType_Integer)) &&
                   (leqType ty2 (Basic BasicType_Integer))
                   = Basic BasicType_Integer
    | op == "%" && (leqType ty1 (Basic BasicType_Integer)) &&
                   (leqType ty2 (Basic BasicType_Integer))
                   = Basic BasicType_Integer
    | op == "%" = ErrorType
    | op == "^" = ErrorType
    | op == "+" = getMaxType (getMaxType ty1 ty2) (Basic BasicType_Integer)
    | op == "-" = getMaxType (getMaxType ty1 ty2) (Basic BasicType_Integer)
    | op == "*" = getMaxType (getMaxType ty1 ty2) (Basic BasicType_Integer)
    | otherwise = getMaxType ty1 ty2 

--------------------------

sizeOfType :: Type -> Int
sizeOfType ty = case ty of
  Basic BasicType_Integer -> 4
  Basic BasicType_Real -> 8
  Basic BasicType_Character -> 2
  Basic BasicType_Boolean -> 1
  Basic BasicType_String -> 4
  Pointer _ -> 4
  ArrayTest t n -> n * (sizeOfType t)

expr_sizeOfType :: Type -> RExpr
expr_sizeOfType ty = Lit $ IntVal $ BInteger ((0,0), v)
  where v = show $ sizeOfType ty

unityAddr :: Type -> TACAddr
unityAddr ty = case ty of
  Basic BasicType_Integer -> ValAddr "1"
  Basic BasicType_Real -> ValAddr "1.0"

---------------------------

defaultValue :: Type -> RExpr
defaultValue ty = case ty of
  Basic BasicType_Integer -> Lit $ IntVal $ BInteger ((0,0),"0")
  Basic BasicType_Real -> Lit $ RealVal $ BReal ((0,0),"0.0")
  Basic BasicType_Character -> Lit $ CharVal $ BChar ((0,0),"'\\u0000'")
  Basic BasicType_Boolean -> Lit $ BoolVal $ BBool ((0,0),"False")
  Basic BasicType_String -> Lit $ StringVal $ BString ((0,0), "\"\"")

defaultValsList :: Type -> Int -> [RExpr]
defaultValsList _ n | n <= 0 = [] -- safety check
defaultValsList ty n = take n (repeat $ defaultValue ty)


--------------------------------------------------------------------
--------------------------- VALUES ---------------------------------

getIntValue :: TACAddr -> Int
getIntValue (ValAddr v) = read v

getRealValue :: TACAddr -> Double
getRealValue (ValAddr v) = read v :: Double

getCharValue :: TACAddr -> String
getCharValue (ValAddr v) = v

--------------

maybeConvertValAddr :: TACAddr -> Type -> Type -> TACAddr
maybeConvertValAddr a ty1 ty2 | ty1 == ty2 = a
maybeConvertValAddr a ty1 ty2 = convertValAddr a ty1 ty2

convertValAddr :: TACAddr -> Type -> Type -> TACAddr
convertValAddr addr@(ValAddr a) tyFrom tyTo = case (tyFrom, tyTo) of
  (Basic BasicType_Integer, Basic BasicType_Real) -> ValAddr $ show $ getRealValue addr
  (Basic BasicType_Character, Basic BasicType_Integer) -> ValAddr $ show $ ord $ good
          where good = read (Data.ByteString.Char8.unpack (Data.ByteString.Char8.pack a)) :: Char
  (Basic BasicType_Character, Basic BasicType_Real) -> convertValAddr addr1 int_ real_
    where addr1 = convertValAddr addr char_ int_
  otherwise -> NoAddr
convertValAddr _ _ _ = NoAddr

--------------------------------------------------------------------
-- Operations on values: precompute constant vals.

intBOp :: TACOp -> TACAddr -> TACAddr -> TACAddr
intBOp op x@(ValAddr _) y@(ValAddr _) = ValAddr res
  where res = case op of
          "add" -> show ((getIntValue x) + (getIntValue y))
          "mul" -> show ((getIntValue x) * (getIntValue y))
          "sub" -> show ((getIntValue x) - (getIntValue y))
          "mod" -> show (mod (getIntValue x) (getIntValue y))
          "pow" -> show ((getIntValue x) ^ (getIntValue y))
intBOp _ _ _ = NoAddr

intUOp :: TACOp -> TACAddr -> TACAddr
intUOp op x@(ValAddr xv) = ValAddr res
  where res = case op of
          "+" -> xv
          "negate" -> show (- (getIntValue x))
intUOp _ _ = NoAddr

------------

realBOp :: TACOp -> TACAddr -> TACAddr -> TACAddr
realBOp op x@(ValAddr _) y@(ValAddr _) = ValAddr res
  where res = case op of
          "add" -> show ((getRealValue x) + (getRealValue y))
          "mul" -> show ((getRealValue x) * (getRealValue y))
          "sub" -> show ((getRealValue x) - (getRealValue y))
          "div" -> show ((getRealValue x) / (getRealValue y))
realBOp _ _ _ = NoAddr

realUOp :: TACOp -> TACAddr -> TACAddr
realUOp op x@(ValAddr xv) = ValAddr res
  where res = case op of
          "+" -> xv
          "negate" -> show (- (getRealValue x))
realUOp _ _ = NoAddr

------------

boolBOp :: TACOp -> TACAddr -> TACAddr -> TACAddr
boolBOp op x@(ValAddr bx) y@(ValAddr by) = ValAddr res
  where res = case op of
          "&&"| bx == "False" -> bx
              | otherwise     -> by
          "||"| bx == "True"  -> bx
              | otherwise     -> by
boolBOp _ _ _ = NoAddr

boolUOp :: TACOp -> TACAddr -> TACAddr
boolUOp op x@(ValAddr bx) = ValAddr res
  where res = case op of
          "!"| bx == "False" -> "True"
             | otherwise     -> "False"
boolUOp _ _ = NoAddr

------------

boolIntBinary :: Binary -> TACAddr -> TACAddr -> TACAddr
boolIntBinary op x@(ValAddr _) y@(ValAddr _) = ValAddr res
  where res = case op of
          BinaryLt _ -> show ((getIntValue x) < (getIntValue y))
          BinaryLte _ -> show ((getIntValue x) <= (getIntValue y))
          BinaryGt _ -> show ((getIntValue x) > (getIntValue y))
          BinaryGte _ -> show ((getIntValue x) >= (getIntValue y))
          BinaryNeq _ -> show ((getIntValue x) /= (getIntValue y))
          BinaryEq _ -> show ((getIntValue x) == (getIntValue y))
boolIntBinary _ _ _ = NoAddr

boolRealBinary :: Binary -> TACAddr -> TACAddr -> TACAddr
boolRealBinary op x@(ValAddr _) y@(ValAddr _) = ValAddr res
  where res = case op of
          BinaryLt _ -> show ((getRealValue x) < (getRealValue y))
          BinaryLte _ -> show ((getRealValue x) <= (getRealValue y))
          BinaryGt _ -> show ((getRealValue x) > (getRealValue y))
          BinaryGte _ -> show ((getRealValue x) >= (getRealValue y))
          BinaryNeq _ -> show ((getRealValue x) /= (getRealValue y))
          BinaryEq _ -> show ((getRealValue x) == (getRealValue y))
boolRealBinary _ _ _ = NoAddr

boolCharBinary :: Binary -> TACAddr -> TACAddr -> TACAddr
boolCharBinary op x@(ValAddr _) y@(ValAddr _) = ValAddr res
  where res = case op of
          BinaryLt _ -> show ((getCharValue x) < (getCharValue y))
          BinaryLte _ -> show ((getCharValue x) <= (getCharValue y))
          BinaryGt _ -> show ((getCharValue x) > (getCharValue y))
          BinaryGte _ -> show ((getCharValue x) >= (getCharValue y))
          BinaryNeq _ -> show ((getCharValue x) /= (getCharValue y))
          BinaryEq _ -> show ((getCharValue x) == (getCharValue y))
boolCharBinary _ _ _ = NoAddr

boolStringBinary :: Binary -> TACAddr -> TACAddr -> TACAddr
boolStringBinary op (ValAddr x) (ValAddr y) = ValAddr res
  where res = case op of
          BinaryNeq _ -> show (x /= y)
          BinaryEq _ -> show (x == y)
boolStringBinary _ _ _ = NoAddr


boolBinary :: Binary -> TACAddr -> TACAddr -> TACAddr
boolBinary op (ValAddr x) (ValAddr y) = ValAddr res
  where res = case op of
          BinaryNeq _ -> show (x /= y)
          BinaryEq _ -> show (x == y)
boolBinary _ _ _ = NoAddr

-------------------

dispatchBOp :: Type -> TACOp -> TACAddr -> TACAddr -> TACAddr
dispatchBOp ty op eVal1 eVal2 = case ty of
  Basic BasicType_Integer -> intBOp op eVal1 eVal2
  Basic BasicType_Real -> realBOp op eVal1 eVal2
  Basic BasicType_Character -> intBOp op (convertValAddr eVal1 ty int_) (convertValAddr eVal2 ty int_)

dispatchUOp :: Type -> TACOp -> TACAddr -> TACAddr
dispatchUOp ty op eVal1 = case ty of
  Basic BasicType_Integer -> intUOp op eVal1
  Basic BasicType_Real -> realUOp op eVal1
  Basic BasicType_Character -> intUOp op (convertValAddr eVal1 ty int_)

dispatchBoolBinary :: Type -> Binary -> TACAddr -> TACAddr -> TACAddr
dispatchBoolBinary ty rel eVal1 eVal2 = case ty of
  Basic BasicType_Integer -> boolIntBinary rel eVal1 eVal2
  Basic BasicType_Real -> boolRealBinary rel eVal1 eVal2
  Basic BasicType_Character -> boolCharBinary rel eVal1 eVal2
  Basic BasicType_Boolean -> boolBinary rel eVal1 eVal2
  Basic BasicType_String -> boolStringBinary rel eVal1 eVal2



--------------------------------------------------------------------
---------------------------- NAMES ---------------------------------

-- Extract the BIdent of a *simple* left expression.
getBId :: LExpr -> BIdent
getBId lexpr = case lexpr of
  LEId bIdent ->  bIdent
  LEDeref lexpr1 -> getBId lexpr1
  LEArrayTest lexpr1 _ -> getBId lexpr1
  LEBra lexpr1 -> getBId lexpr1
  LEPreIncrem lexpr1 -> getBId lexpr1
  LEPreDecr lexpr1 -> getBId lexpr1


-- Extract the name of a *simple* left expression.
getName :: LExpr -> Id
getName lexpr = case lexpr of
  LEId (BIdent ((_,_), id)) -> id
  LEDeref lexpr1 -> getName lexpr1
  LEArrayTest lexpr1 _ -> getName lexpr1
  LEBra lexpr1 -> getName lexpr1
  LEPreIncrem lexpr1 -> getName lexpr1
  LEPreDecr lexpr1 -> getName lexpr1

getId :: BIdent -> Id
getId (BIdent ((_,_), id)) = id

getIdAddress :: BIdent -> TACAddr
getIdAddress (BIdent ((l,c), id)) = Addr (id++('@':(show l))++(',':(show c)))

getIdLabel :: BIdent -> TACLabel
getIdLabel (BIdent ((l,c), id)) = Label (id++('@':(show l))++(',':(show c)))

getAddressName :: TACAddr -> Id
getAddressName (Addr name) = name


--------------------------------------------------------------------
--------------------------- POSITIONS ------------------------------

relPos :: Binary -> Pos
relPos rel = case rel of
  BinaryLt (BLe (pos,_))         -> pos
  BinaryLte (BLeEq (pos,_))     -> pos
  BinaryGt (BGr (pos,_))      -> pos
  BinaryGte (BGrEq (pos,_))  -> pos
  BinaryNeq (BNotEq (pos,_))     -> pos
  BinaryEq (BEq (pos,_))           -> pos
  
firstStmtPos :: LoopCmd -> (Pos,String)
firstStmtPos stmt =
    case stmt of 
        LoopExitOn (BExitOn (pos,s)) e   -> (pos,s)
        LoopExit (BExit (pos,s))         -> (pos,s)
        LoopContinue (BContinue (pos,s)) -> (pos,s)

firstRExprPos :: RExpr -> Pos
firstRExprPos ev =
    case ev of
        Lit v ->
            case v of
                IntVal (BInteger (pos,_))  -> pos
                RealVal (BReal (pos,_))    -> pos
                CharVal (BChar(pos,_))     -> pos
                BoolVal (BBool(pos,_))     -> pos
                StringVal (BString(pos,_)) -> pos
        RExprLeft v ->
            case v of
                LEId id@(BIdent(pos,_))    -> pos
                LEDeref le                 -> firstRExprPos (RExprLeft le)
                LEArrayTest le _               -> firstRExprPos (RExprLeft le)
                LEBra le                   -> firstRExprPos (RExprLeft le)
                LEPreIncrem le             -> firstRExprPos (RExprLeft le)
                LEPreDecr le               -> firstRExprPos (RExprLeft le)
        RExprArrayTest _ (e:_) -> firstRExprPos e
        RExprIf e _ _ -> firstRExprPos e
        RExprArrayTest (BLBra (pos,_)) _ -> pos
        RExprCreate _ _ _ e -> firstRExprPos e
        RExprAddress le -> firstRExprPos (RExprLeft le)
        RExprCall id@(BIdent(pos,_)) _ -> pos
        RExprBrack e -> firstRExprPos e
        RExprPostIncr le -> firstRExprPos (RExprLeft le)
        RExprPostDecr le -> firstRExprPos (RExprLeft le)
        RExprPow e _ _  -> firstRExprPos e
        RExprMul e _ _-> firstRExprPos e
        RExprMod e _ _-> firstRExprPos e
        RExprDiv e _ _-> firstRExprPos e
        RExprSum e _ _-> firstRExprPos e
        RExprMinus e _ _-> firstRExprPos e
        RExprUnaryMinus _ e -> firstRExprPos e
        RExprUnaryPlus _ e -> firstRExprPos e
        RExprAnd e _ _ -> firstRExprPos e
        RExprOr e _ _ -> firstRExprPos e
        RExprNot _ e -> firstRExprPos e
        RExprBinary e _ _  -> firstRExprPos e


--------------------------------------------------------------------
----------------------------- PRINTS --------------------------------

showFunction :: String -> [Type] -> String
showFunction out [] = (init out) ++ ")"
showFunction out (t:types) =
    showFunction (out++(showType t )++",") types

showType :: Type -> String
showType ty = case ty of
  Basic BasicType_Integer -> "int"
  Basic BasicType_Boolean -> "bool"
  Basic BasicType_Character -> "char"
  Basic BasicType_Real -> "real"
  Basic BasicType_String -> "string"
  ErrorType -> "error"
  VoidType -> "void"
  Pointer t -> ('°':(showType t))
  ArrayTest _ t _ ->  "ArrayTest <" ++ (showType t) ++ ">"
  -- ArrayTestDimType _ t _ _ ->  "ArrayTest <" ++ (showType t) ++ ">"
  ArrayTest _ t _ _ ->  "ArrayTest <" ++ (showType t) ++ ">"
  ArrayTest t 0 -> "ArrayTest <" ++ (showType t) ++ ">(" ++ "?" ++")"
  ArrayTest t n -> "ArrayTest <" ++ (showType t) ++ ">(" ++ (show n) ++")"

showAssignType :: Type -> String
showAssignType ty = case ty of
  Basic BasicType_Integer -> "int"
  Basic BasicType_Boolean -> "bool"
  Basic BasicType_Character -> "char"
  Basic BasicType_Real -> "real"
  Basic BasicType_String -> "addr"
  ErrorType -> error $ "showing ErrorType"
  VoidType -> error $ "showing VoidType"
  otherwise -> "addr"

showTACBinary :: Binary -> String
showTACBinary rel = case rel of
  BinaryLt _ -> "less"
  BinaryLte _-> "less_eq"
  BinaryGt _ -> "gr"
  BinaryGte _ -> "gr_eq"
  BinaryNeq _-> "not_eq"
  BinaryEq _-> "equal"

showBinary :: Binary -> String
showBinary rel = case rel of
  BinaryLt _ -> "<"
  BinaryLte _-> "<="
  BinaryGt _ -> ">"
  BinaryGte _ -> ">="
  BinaryNeq _-> "<>"
  BinaryEq _-> "="

builtIn :: String -> TACLabel
builtIn name = Label ("built_in_"++name)


--------------------------------------------------------------------
--------------------------- GENERAL --------------------------------

trust :: Maybe a -> a
trust (Just x) = x

isFuncDecl :: Stmt -> Bool
isFuncDecl (StmtDecl (FuncDef _ _ _ _ _ _)) = True
isFuncDecl (StmtDecl (ProcDef _ _ _ _)) = True
isFuncDecl _ = False


notBinary :: Binary -> Binary
notBinary rel = case rel of
  BinaryLt (BLe (pos,_))         -> BinaryGte (BGrEq(pos,">="))
  BinaryLte (BLeEq (pos,_))     -> BinaryGt (BGr (pos,">"))
  BinaryGt (BGr (pos,_))      -> BinaryLte (BLeEq (pos,"<="))
  BinaryGte (BGrEq (pos,_))  -> BinaryLt (BLe (pos,"<"))
  BinaryNeq (BNotEq (pos,_))     -> BinaryEq (BEq (pos,"=")) 
  BinaryEq (BEq (pos,_))           -> BinaryNeq (BNotEq (pos,"<>"))

-------------------------------------------------------------------
-- Private aliases:

int_ = (Basic BasicType_Integer)
real_ = (Basic BasicType_Real)
char_ = (Basic BasicType_Character)
bool_ = (Basic BasicType_Boolean)
string_ = (Basic BasicType_String)
void_ = VoidType
addr_ = (Basic BasicType_String) -- only for prints.

-- negRExpr_ cond = RExprNot not_ cond
exitOnStmt_ cond = StmtLoopCmd $ LoopExitOn exiton_ cond
preIncr_ bId = RExprLeft $ LEPreIncrem $ LEId bId
preDecr_ bId = RExprLeft $ LEPreDecr $ LEId bId
relGEq_ = BinaryGte greateq_
relLEq_ = BinaryLte lesseq_
relLess_ = BinaryLt less_
zeroRExpr_ = Lit $ IntVal $ BInteger ((0,0), "0")

eq_ = BEq ((0,0),"=")
neq_ = BNotEq ((0,0),"<>")
plus_ = BPlus ((0,0),"+")
minus_ = BMinus ((0,0),"-")
bra_ = BLBra ((0,0), "[")
lesseq_ = BLeEq ((0,0),"<=")
less_ = BLe ((0,0),"<")
greateq_ = BGrEq ((0,0),">=")
exiton_  = BExitOn ((0,0), "exiton")
not_ = BNot ((0,0),"not")

