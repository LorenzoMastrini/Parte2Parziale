module StaticAnalysis where

import AbsLinguaggio
import PrettyPrinter
import ErrorStaticAnalysis
import LexLinguaggio (Token(..), Posn(..), tokenPosn, Tok(..), TokSymbol(..))
import Data.Char (ord, readLitChar)
import Data.Map (Map, member,lookup,toList)
import Data.ByteString.Char8 (unpack, ByteString, pack)
import qualified Data.Map as Map
import Data.List (intercalate)

-- Tipo di annotazione per l'analisi statica

data TypeInfo = TInteger | TReal | TBoolean | TCharacter | TString | TVoid | TUnknown
  deriving (Eq, Ord, Show)

-------------------------------------------------
----------------- DATA & TYPE -------------------
-------------------------------------------------
{-
--type CTime = Bool
--type Names = Map Id EnvRecord

data EnvRecord =
    VarInfo {
        typ :: Type,                -- Variable Type.
        cTime :: CTime,             -- True if is const at CompileTime
        pos :: Pos,                 -- SourceLocation <line, column>
        init :: Maybe Int,          -- If the value of a constant is known at CompileTime
        const :: Bool
        } |
    FuncInfo {
        retType :: Type,              -- Function return Type
        params :: [FormalParameters], -- List of parameters
        pos :: Pos                    -- SourceLocation <line,column>
    } deriving (Eq, Show)

data Env =  Env {
  names :: Names,                   -- Map with all the name declared
  parent :: Maybe Env,              -- Parent block
  errors :: [(String, Pos)],        -- String of errors
  loop   :: Bool,                   -- True if the block is a loopBlock
  funcCheck :: (Maybe Names,        -- Names in the functions declarations
                Maybe Ident,       -- Name of the return variable
                Bool,               -- True if all the scope at the level n-1 have an initialization of the variable
                Bool,               -- True if one of the scope at the level n or n-1 initialize the variable
                Bool,               -- False if there is a continue/return/exit or exiton
                Bool),              -- True if there is a Default case.
  loopFor :: Bool                   -- True if a for stmt is found.           
} deriving (Eq, Show)

data ExprRes = ExprRes{             --ExprRes incapsulate the result of the comutation of an Expression
    eTyp :: Type,                   --Result Type
    eCTime :: CTime,                --True if the result is known at compile time
    eVal :: Maybe Int,              --The integer value that the Static Analysis module can calculate at compile time
    eConst ::  Bool                 --If the right expression is Constant
} deriving (Eq, Show)
-}

-- Semantic environment definition
type Env = (Map.Map (Ident ()) EnvEntry, [(String, Loc)])  -- (symbol table, error list)

data EnvEntry = ConstVarEntry (Type ()) Bool Loc -- (type, is constant, location)
                  | ProcFunEntry [FormalParameters ()] (Type ()) Loc -- (parameters, return type, location)
                  deriving (Eq, Show)

-------------------------------------------------
----------------- DEFAULT VALUES ----------------
-------------------------------------------------

--exprError :: ExprRes
--exprError = ExprRes ErrorType False Nothing False

-- Default functions
-- Ora monomorfa su TypeInfo
defaultFuncList :: [(Ident (), EnvEntry)]
defaultFuncList =  [ (Ident "writeInt"    (Loc 0 0) (), ProcFunEntry [FormParam ByVal [Ident "valInt" (Loc 0 0) ()] (BasicType Integer)]    (BasicType Void)      (Loc 0 0))
                   , (Ident "writeReal"   (Loc 0 0) (), ProcFunEntry [FormParam ByVal [Ident "valReal" (Loc 0 0) ()] (BasicType Real)]      (BasicType Void)      (Loc 0 0))
                   , (Ident "writeChar"   (Loc 0 0) (), ProcFunEntry [FormParam ByVal [Ident "valChar" (Loc 0 0) ()] (BasicType Character)] (BasicType Void)      (Loc 0 0))
                   , (Ident "writeString" (Loc 0 0) (), ProcFunEntry [FormParam ByVal [Ident "valString" (Loc 0 0) ()] (BasicType String)]  (BasicType Void)      (Loc 0 0))
                   , (Ident "readInt"     (Loc 0 0) (), ProcFunEntry [FormParam ByRef [Ident "valInt" (Loc 0 0) ()] (BasicType String)]     (BasicType Integer)   (Loc 0 0))
                   , (Ident "readReal"    (Loc 0 0) (), ProcFunEntry [FormParam ByRef [Ident "valReal" (Loc 0 0) ()] (BasicType String)]    (BasicType Real)      (Loc 0 0))
                   , (Ident "readChar"    (Loc 0 0) (), ProcFunEntry [FormParam ByRef [Ident "valChar" (Loc 0 0) ()] (BasicType String)]    (BasicType Character) (Loc 0 0))
                   , (Ident "readString"  (Loc 0 0) (), ProcFunEntry [FormParam ByRef [Ident "valString" (Loc 0 0) ()] (BasicType String)]  (BasicType String)    (Loc 0 0))]

defaultEnv :: Env
defaultEnv = (Map.fromList defaultFuncList, []) -- (symbol table, empty errors list)

-------------------------------------------------
-------- MAIN CATEGORIES HANDLER ----------------
-------------------------------------------------

-- The static analysis updates the informations of the AST needed in subsequent step like TAC generation

-- Called in Compiler.hs. In the environment structure there are all the possible errors.
checkProgram :: Program () -> (Env, Program TypeInfo)
checkProgram prog@(Prog ident params block) =
    let
    env = addFormalParams defaultEnv params
    (env1, paramsEnriched) = checkFormalParametersList env params
    (env2, blockEnriched) = checkBlock env1 block
    in (env2, Prog (enrichIdentUnknown ident) paramsEnriched blockEnriched)

checkFormalParametersList :: Env -> [FormalParameters ()] -> (Env, [FormalParameters TypeInfo])
checkFormalParametersList env [] = (env, [])
checkFormalParametersList env (params:paramsList) =
    let
    (env1, paramsEnriched) = checkFormalParameters env params
    (env2, paramsListEnriched) = checkFormalParametersList env paramsList
    in (env2, paramsEnriched:paramsListEnriched)

checkFormalParameters :: Env -> FormalParameters () -> (Env, FormalParameters TypeInfo)
checkFormalParameters env (FormParam mod idents ty) =
      let
        identsEnriched = map (`enrichIdent` ty) idents
        tyEnriched = enrichType ty
        env1 = foldl (\e i -> addSymbol e i (ConstVarEntry ty False (getIdentLoc i))) env idents
      in (env1, FormParam mod identsEnriched tyEnriched)

checkBlock :: Env -> Block () -> (Env, Block TypeInfo)
checkBlock env (Block constDefList varDeclList procFunDeclList compStmt) =
    let
        (env1, constDefListEnriched) = checkConstDefList env constDefList
        (env2, varDeclListEnriched) = checkVarDeclList env1 varDeclList
        (env3, procFunDeclListEnriched) = checkProcFunDeclList env2 procFunDeclList
        compStmtEnriched = enrichCompoundStatement compStmt
    in (env2, Block constDefListEnriched varDeclListEnriched procFunDeclListEnriched compStmtEnriched)

-- Iterate the list of constant definitions 
checkConstDefList :: Env -> [ConstantDefinition ()] -> (Env, [ConstantDefinition TypeInfo])
checkConstDefList env [] = (env, [])
checkConstDefList env (d:ds) =
    let
        (env1, dEnr) = checkConstDef env d
        (env2, dsEnr) = checkConstDefList env1 ds
    in (env2, dEnr:dsEnr)

-- Check if the constant identifier is already used inside the environment
checkConstDef :: Env -> ConstantDefinition () -> (Env, ConstantDefinition TypeInfo)
checkConstDef env def@(ConstDef ident@(Ident name loc ty) constVal) = case lookupByName env name of
    Just (ConstVarEntry {}) ->
        (addError env (RedeclaredVar ident, getIdentLoc ident), ConstDef (enrichIdentUnknown ident) (enrichConst constVal))
    Just (ProcFunEntry {}) ->
        (addError env (WarVarNameUsedForFunc ident, getIdentLoc ident), ConstDef (enrichIdentUnknown ident) (enrichConst constVal))
    Nothing ->
        let
            ty = inferConstantType constVal
            newEnv = addSymbol env ident (ConstVarEntry ty True (getIdentLoc ident))
        in (newEnv, ConstDef (enrichIdent ident ty) (enrichConst constVal))

-- Iterate the list of variable declarations
checkVarDeclList :: Env -> [VariableDeclaration ()] -> (Env, [VariableDeclaration TypeInfo])
checkVarDeclList env [] = (env, [])
checkVarDeclList env (d:ds) =
    let
        (env1, dEnr) = checkVarDecl env d
        (env2, dsEnr) = checkVarDeclList env1 ds
    in (env2, dEnr:dsEnr)

checkVarDecl :: Env -> VariableDeclaration () -> (Env, VariableDeclaration TypeInfo)
checkVarDecl env (VarDecl ids ty) = checkVarDeclSequence env ids ty

-- Iterate the sequence of variable declarations that has the same type (ex. a, b, c: Integer)
checkVarDeclSequence :: Env -> [Ident ()] -> Type () -> (Env, VariableDeclaration TypeInfo)
checkVarDeclSequence env (id:ids) ty =
    let
        (env1, idEnr) = checkIdent env id ty
        (env2, VarDecl idsEnr tyEnr) = checkVarDeclSequence env1 ids ty
    in (env2, VarDecl (idEnr:idsEnr) tyEnr)

-- check if the variable identifier is already been used inside the environment
checkIdent :: Env -> Ident () -> Type () -> (Env, Ident TypeInfo)
checkIdent env ident@(Ident name loc _) ty =
    let
        name = getIdentName ident
        existing = lookupByName env name
    in case existing of
        Just (ConstVarEntry _ isConst loc) ->
            if isConst then
                (addError env (RedeclaredConst ident, getIdentLoc ident), enrichIdentUnknown ident)
            else
                (addError env (RedeclaredVar ident, getIdentLoc ident), enrichIdentUnknown ident)
        Just (ProcFunEntry _ _ loc) ->
            (addError env (RedeclaredProcFun ident, getIdentLoc ident), enrichIdentUnknown ident)
        Nothing ->
            (addSymbol env ident (ConstVarEntry ty False (getIdentLoc ident)), enrichIdent ident ty)

-- Iterate the list of procedures and functions declarations
checkProcFunDeclList :: Env -> [ProcedureAndFunctionDeclaration ()] -> (Env, [ProcedureAndFunctionDeclaration TypeInfo])
checkProcFunDeclList env [] = (env, [])
checkProcFunDeclList env (d:ds) =
    let (env', dEnr) = checkProcFunDecl env d
        (env'', dsEnr) = checkProcFunDeclList env' ds
    in (env'', dEnr:dsEnr)

checkProcFunDecl :: Env -> ProcedureAndFunctionDeclaration () -> (Env, ProcedureAndFunctionDeclaration TypeInfo)
checkProcFunDecl env (ProcFunDecl heading block) =
    let env' = addProcFun env heading
    in (env', ProcFunDecl (enrichProcFunHeading heading) (enrichBlock block))

-------------------------------------------------
---------------- DECLARATIONS -------------------
-------------------------------------------------
{-

handlerVarDef :: Env -> Ident -> Type -> Expr -> Pos -> Env
handlerVarDef env id@(Ident (pos,name)) ty expr posA
    | redecl    = addressEnv
    | otherwise = addSymbol addressEnv name (VarInfo arrType False pos Nothing False)
    where (redeclEnv, redecl)      = checkEnvRedecl env id 1
          (exprEnv, eRes@(ExprRes exprType cTime valE constE)) = saExpr redeclEnv expr
          (arrEnv, tmpType, errorFounds) = convertToRedArray exprEnv  ty
          arrType                  = checkArrays tmpType exprType
          (compEnv, comp)          = checkComp exprEnv exprType arrType posA
          envCompArr | errorFounds = arrEnv
                     | otherwise   = compEnv
          (addressEnv,_)           = checkAddressOfConst envCompArr constE expr pos 1

handlerFuncDef:: Env ->Ident -> [Param] -> Ident -> Type -> [Stmt] -> Ident -> Env
handlerFuncDef env funcId@(Ident (pos,_)) params
               outId@(Ident (posOut,_)) outType stmts closeFuncId = envReturnChecked
    where envNameCheck     = checkFunctionNames env funcId closeFuncId
          envParamCheck    = checkFunctionParameters envNameCheck params
          envReturnVar     = addIfNotDeclared envParamCheck outId 1 (VarInfo retTy False posOut Nothing False)
          (envArray, retTy)= case outType of
                                  BasicType _  -> (envReturnVar, outType)
                                  otherwise -> (addError envReturnVar (NonBasicType posOut outType, posOut), ErrorType)
          tmpEnv           = returnInit envArray outId
          envStmts         = saStmts tmpEnv stmts
          envReturnChecked = checkIfReturnVariableIsInitialized envStmts


handlerProcDef:: Env ->Ident -> [Param] -> [Stmt] -> Ident -> Env
handlerProcDef env procId params  stmts closeProcId = envStmts
    where envNameCheck    = checkFunctionNames env procId closeProcId
          envParamCheck   = checkFunctionParameters envNameCheck params
          envStmts        = saStmts envParamCheck stmts

-------------------------------------------------
---------------- EXPRESSIONS --------------------
-------------------------------------------------

handlerExprCall:: Env -> Ident -> [Expr] -> (Env, ExprRes)
handlerExprCall env id@(Ident (pos,_)) exprs
    |isDecl = checkExprsAndParams env exprs par id fpos retTy
    |otherwise = (addError env (NotDeclared id,pos), exprError)
    where maybeDecl = isDeclVar env id
          (isDecl, par, retTy,fpos) = case maybeDecl of
                            Just (FuncInfo ty params poss ) -> (True, params, ty,poss)
                            Just (VarInfo _ _ _ _ _) -> (False, [], VoidType, pos)
                            Nothing -> (False,[], VoidType,pos)

handlerUnaryBool :: Env -> Expr -> (Pos, String) ->(Env, ExprRes)
handlerUnaryBool env ex (pos, op)
    |isBool = (envE1, (ExprRes (BasicType TypeBool) False Nothing False))
    |isError typeE1 = (envE1, exprError)
    |otherwise = (addError envE1 (BoolNotError pos typeE1 op, pos), exprError)
    where (envE1, eRes1@(ExprRes typeE1 cTimeE1 valE1 constE1)) = saExpr env ex
          isBool = typeE1 == (BasicType TypeBool)

handlerBinaryBool:: Env -> Expr -> Expr -> (Pos,String) -> (Env, ExprRes)
handlerBinaryBool env ex1 ex2 (pos, op)
    |areBool = (envE2, (ExprRes (BasicType TypeBool) False Nothing False))
    |areErrors typeE1 typeE2 = (envE2, exprError)
    |otherwise = (addError envE2 (BoolOpError pos typeE1 typeE2 op, pos), exprError)
    where (envE1, eRes1@(ExprRes typeE1 cTimeE1 valE1 constE1))    = saExpr env ex1
          (envE2, eRes2@(ExprRes typeE2 cTimeE2 valE2 constE2))    = saExpr envE1 ex2
          areBool = typeE1 == (BasicType TypeBool) && typeE2 == typeE1

handlerRelExpr :: Env -> Expr -> Expr -> Rel -> (Env, ExprRes)
handlerRelExpr env ex1 ex2 rel
    | comparable || applyRel = (envE2, (ExprRes (BasicType TypeBool) False Nothing False))
    | areErrors typeE1 typeE2 = (envE2, exprError)
    | otherwise = (addError envE2 (RelError pos typeE1 typeE2 rel, pos), exprError)
    where (envE1, eRe1@(ExprRes typeE1 cTimeE1 valE1 constE1))    = saExpr env ex1
          (envE2, eRe2@(ExprRes typeE2 cTimeE2 valE2 constE2))    = saExpr envE1 ex2
          comparable = comparableTypes typeE1 typeE2
          equalityCSTR = typeE1 == (BasicType TypeString) && typeE2 == (BasicType TypeChar) ||
                         typeE2 == (BasicType TypeString) && typeE1 == (BasicType TypeChar)
          pos        = relPos rel
          applyRel   = (equalityCSTR ||comparable ||
                     (typeE1 == typeE2 && equalitySupport typeE1))
                     && case rel of
                            (RelEq _) -> True
                            (RelNotEq _) -> True
                            otherwise -> False

handlerExprDiv :: Env -> Expr -> Expr -> (Pos,String)  -> (Env, ExprRes)
handlerExprDiv env ex1 ex2 op
    |isZero = (addError env (DivideByZero pos,pos), exprError)
    |otherwise = handlerBinaryExpr env ex1 ex2 op
    where (_, eRes@(ExprRes _ _ valE _)) = saExpr env ex2
          pos = firstExprPos ex2
          isZero = case valE of
                        Just 0 -> True
                        otherise -> False

handlerBinaryExpr :: Env -> Expr -> Expr ->(Pos,String) -> (Env, ExprRes)
handlerBinaryExpr env ex1 ex2 (opPos, op)
    | numericalComp e1Type && numericalComp e2Type = (resEnv,res )
    | areErrors e1Type e2Type = (e2Env, exprError)
    | otherwise = (addError e2Env (ArithmeticError e1Type e2Type opPos op , opPos), exprError)
    where (e1Env, eRes1@(ExprRes e1Type e1CTime valE1 constE1)) = saExpr env ex1
          (e2Env, eRes2@(ExprRes e2Type e2CTime valE2 constE2)) = saExpr e1Env ex2
          retTy  = operationReturnType e1Type e2Type op
          resEnv = checkArithmeticTypes e2Env e1Type e2Type retTy (opPos, op)
          value  = maybeOp op valE1 valE2
          res    = ExprRes retTy (e1CTime && e2CTime) value False

handlerUnaryExpr :: Env -> Expr -> String -> (Env, ExprRes)
handlerUnaryExpr env expr op=
    case eType of
        ErrorType -> (eEnv, eRes)
        PointerType _ -> (addError eEnv (PointerArithmetic pos, pos), exprError)
        Array _ _->   (addError eEnv (PointerArithmetic pos, pos), exprError)
        otherwise | leqType eType (BasicType TypeInt) ||eType == (BasicType TypeReal)  -> (eEnv, eRes{eVal = signVal, eTyp=newType  })
                  | True ->(addError eEnv (NonNumericValue pos, pos),exprError)
    where (eEnv,eRes@(ExprRes eType eCTime valE constE)) = saExpr env expr
          newType = getMaxType eType (BasicType TypeInt)
          pos = firstExprPos expr
          signVal | op=="-"   = maybeNegative valE
                  | otherwise = valE

handlerExprAddress :: Env -> LeftExpr -> (Env, ExprRes)
handlerExprAddress env le =
    case eType of
        ErrorType -> (eEnv, exprError)
        otherwise -> (eEnv,  (ExprRes (PointerType eType) eCTimeE Nothing constE))
        where (eEnv, eRes@(ExprRes eType eCTimeE valE constE)) = handlerLeftExpr env le
              id@(Ident (pos,name)) = getBId le

handlerExprVal:: Env -> Val -> (Env, ExprRes)
handlerExprVal env val =
    case val of
        IntVal (BInteger(_,val)) -> (env,(ExprRes (BasicType TypeInt) True (Just (read val)) False))
        RealVal   _ -> (env,( ExprRes (BasicType TypeReal)   True Nothing False))
        CharVal (BChar(_,val)) -> (env,( ExprRes (BasicType TypeChar) True (Just (ord $ good) ) False))
          where good = read (Data.ByteString.Char8.unpack (Data.ByteString.Char8.pack val)) :: Char  -- 
        BoolVal   _ -> (env,( ExprRes (BasicType TypeBool)   False Nothing False))
        StringVal _ -> (env,( ExprRes (BasicType TypeString) False Nothing False))

handlerExprArray :: Env -> [Expr] -> Type -> Pos -> (Int,Int) -> (Env, ExprRes)
handlerExprArray env [] typ _ (lenExpr, _)
    | typ == ErrorType = (env, (ExprRes (Array ErrorType lenExpr) False Nothing False))
    | otherwise = (env, (ExprRes (Array typ lenExpr) True Nothing False))

handlerExprArray env (e:expr) typ pos (lenExpr, lenSubExpr)
    |isAddressOfConst = handlerExprArray envAddress expr ErrorType pos (lenExpr,(-1))
    |eLen /= lenSubExpr  && lenSubExpr /=(-1)= handlerExprArray envNonLen expr ErrorType pos (lenExpr,(-1))
    |typ == exprType || isError typ = handlerExprArray exprEnv expr typ pos (lenExpr,lenSubExpr)
    |otherwise = handlerExprArray envNonOmo expr ErrorType pos (lenExpr,(-1))
    where (exprEnv, eRes@(ExprRes exprType cTimeE valE constE)) = saExpr env e
          envNonOmo = addError exprEnv (NonOmogeneusArray pos, pos)
          envNonLen = addError exprEnv (DifferentLenght pos, pos)
          (envAddress,isAddressOfConst)= checkAddressOfConst exprEnv constE e (firstExprPos e) 0
          eLen = getExprLen env e

handlerExprCreate:: Env -> Expr -> Type -> (Env, ExprRes)
handlerExprCreate env expr ty
    | isBT = (addError envC (NonBasicType pos ty, pos), exprError)
    | isCTime  = (envC,(ExprRes (Array ty (trust maybeInt)) False maybeInt False))
    | otherwise = (envC, exprError)
    where (envC, isCTime, maybeInt) = checkConstExpr env expr
          pos = firstExprPos expr
          isBT = case ty of (BasicType _) -> False
                            otherwise -> True

handlerLeftExpr:: Env -> LeftExpr -> (Env, ExprRes)
handlerLeftExpr env le =
    case le of
        LEId id -> handlerIdent env id
        LEDeref le -> handlerDeref env le
        LEArray le dim -> handlerArray env le dim
        LEBra le -> handlerLeftExpr env le
        LEPreIncrem le -> handlerUnaryLeftExpr env le
        LEPreDecr le-> handlerUnaryLeftExpr env le

handlerDeref :: Env -> LeftExpr -> (Env, ExprRes)
handlerDeref  env le =
    case nType of
        (PointerType t) -> (nEnv, (ExprRes t nCTime valE False))
        otherwise       -> (addError nEnv (DereferenceExcess id , pos), exprError)
    where (nEnv, eRes@(ExprRes nType nCTime valE constE))  = handlerLeftExpr env le
          id@(Ident (pos,name)) = getBId le

handlerArray :: Env -> LeftExpr -> Dim -> (Env, ExprRes)
handlerArray env le (DimArray _ expr)
    | not isDecl = (addError env (NotDeclared id, pos), exprError)
    | not (leqType typeEx (BasicType TypeInt)) = (addError envExpr (NonIntegerExprInArrayAccess posExpr, posExpr), exprError)
    | leqType typeEx (BasicType TypeInt) && cTimeEx && not gZero  = (addError envExpr (NegativeSizeIndex posExpr, posExpr), exprError)
    | otherwise =case typeE of
        (Array t _)  ->(envExpr, eRes {eTyp = t})
        otherwise       -> (addError envExpr (NotAnArray pos , pos), exprError)
    where id@(Ident (pos,name)) = getBId le
          (nEnv, eRes@(ExprRes typeE cTimeE valE constE))  = handlerLeftExpr env le
          posExpr = firstExprPos expr
          gZero = case valEx of
                    Nothing -> True
                    Just x  -> x >= 0
          (envExpr,eExpr@(ExprRes typeEx cTimeEx valEx _) )  = saExpr nEnv expr
          isDecl = case isDeclVar env id of
                        Just _ -> True
                        otherwise -> False


handlerIdent :: Env -> Ident -> (Env, ExprRes)
handlerIdent env id = case checkDeclIdent env id of
        (nEnv,Just (VarInfo ty cTime _ x const)) -> (nEnv, (ExprRes ty cTime x const))
        (nEnv,Just (FuncInfo _ _ pos))           -> (addError nEnv (FunctionAsIdentifier pos,pos), exprError)
        (nEnv,_)                                 -> (nEnv , exprError)

handlerUnaryLeftExpr :: Env -> LeftExpr -> (Env, ExprRes)
handlerUnaryLeftExpr env le
    | constE && typeE /= ErrorType = (addError env (ChangingConstVar id, pos), exprError)
    | otherwise = checkUnaryTypes eEnv typeE eRes pos
      where (eEnv, eRes@(ExprRes typeE cTimeE valE constE)) = handlerLeftExpr env le
            id@(Ident (pos,name)) = getBId le

handlerEmptyList :: Env -> Pos -> (Env, ExprRes)
handlerEmptyList env pos =
    (addError env (EmptyList pos, pos), exprError)

handlerExprIf :: Env -> Expr -> Expr -> Expr -> (Env, ExprRes)
handlerExprIf env expr1 exprBool expr2 = res
  where env1 = checkBooleanCondition env exprBool
        (env2, (ExprRes tyE1 cTE1 valE1 constE1)) = saExpr env1 expr1
        (env3, (ExprRes tyE2 cTE2 valE2 constE2)) = saExpr env2 expr2
        pos = firstExprPos expr1
        res | tyE1 == tyE2 = (env3, (ExprRes tyE1 False Nothing False))
             | otherwise = (addError env3 (IncompatibleExprs pos tyE1 tyE2, pos), exprError)

-------------------------------------------------
------------------ STATEMENTS -------------------
-------------------------------------------------

handlerStmtAssign::Env -> Expr -> Expr ->Pos -> Env
handlerStmtAssign env le@(ExprLeft left) re posA = retEnv
    where (envL, eResL@(ExprRes typeL cTimeL valL constEL)) = saExpr env le
          (envR, eResR@(ExprRes typeR cTimeR valR constER)) = saExpr envL re
          (compEnv, comp)  = checkComp envR typeR typeL posA
          constEnv = checkModifyConstant compEnv id constEL
          (addressEnv,_) = checkAddressOfConst constEnv constER re pos 1
          retEnv = checkReturnInitialized addressEnv id
          id@(Ident (pos@(l,c),name)) = getBId left

handlerStmtLoopCmd::Env -> LoopCmd -> Env
handlerStmtLoopCmd env loopS
    | loop env  = checkedEnv{funcCheck = (name, id, retBool, boolFather, False, def)}
    | loopFor env = addError checkedEnv{funcCheck = (name, id, retBool, boolFather, False, def)} (ReturnInForLoop pos cmdName,pos)
    | otherwise= addError (env{funcCheck= (name, id, retBool, boolFather, False, def)}) (NotInLoop pos cmdName, pos)
    where (pos,cmdName) = firstStmtPos loopS
          checkedEnv = checkLoopCmd env loopS
          (name, id, retBool, boolFather,_, def) = funcCheck checkedEnv

handlerElseStmts :: Env -> ElseStmt -> Env
handlerElseStmts env elseS =
    case elseS of
        StmtElseIf expr stmts elsestmts  ->  copyEnvInfo oldEnv sonEnv (Just sonEnv2)
            where oldEnv = checkBooleanCondition env expr
                  sonEnv = saStmts (addFuncsFromStmts (createEnv env)   stmts) stmts
                  sonEnv2 =  handlerElseStmts (createEnv env)  elsestmts
        StmtElse stmts -> copyEnvInfo env sonEnv Nothing
            where sonEnv = saStmts (addFuncsFromStmts (createEnv env)  stmts) stmts
        StmtNoElse -> env

handlerStmtCase :: Env -> Type -> Cases -> Int ->Pos -> Env
handlerStmtCase env baseType ca numBranch pos =
    case ca of
        CaseEps | numBranch == 0 -> addError env (WarEmptyCase pos,pos)
                | otherwise ->  env
        CaseDefault stmts ->  newEnv{funcCheck = (oldN, oldId,  oldSon, oldFather ,break, True)}
            where sonEnv  = saStmts (addFuncsFromStmts (createEnv env)  stmts) stmts
                  newEnv  = (copyEnvInfo env sonEnv Nothing)
                  (oldN, oldId, oldSon, oldFather, break, _) = funcCheck newEnv
        CaseVal expr stmts cases -> handlerStmtCase oldEnv baseType cases (numBranch+1) pos
            where oldEnv  = (copyErrors tmpEnv sonEnv ){funcCheck = (oldN, oldId,  def&&valSon, def&&(valOld || valSon),break, def)}
                  tmpEnv | not (leqType exprType baseType ) = (addError exprEnv (NonCompExpr baseType exprType nPos,nPos))
                         | otherwise = checkCaseExpr exprEnv exprType expr
                  sonEnv  = saStmts (addFuncsFromStmts (createEnv tmpEnv)  stmts) stmts
                  (oldN,oldId,valOld, _,break, def) = funcCheck tmpEnv
                  (_,_,valSon, _,_,_) = funcCheck sonEnv
                  (exprEnv, eRes@(ExprRes exprType cTime valE constE)) = saExpr env expr
                  nPos = firstExprPos expr

handlerStmtExpr::Env->Expr ->Env
handlerStmtExpr env expr =
  case saExpr env expr of
    (exprEnv, (ExprRes ErrorType _ _ _)) -> exprEnv
    (exprEnv, (ExprRes VoidType _ _ _))  -> exprEnv
    (exprEnv, (ExprRes _ _ _ _))         -> case expr of
          ExprPostIncr _             -> exprEnv
          ExprPostDecr _             -> exprEnv
          ExprLeft  (LEPreDecr _)     -> exprEnv
          ExprLeft  (LEPreIncrem _ ) -> exprEnv
          otherwise                  -> addError exprEnv (UnsusedValue (pos),pos)
  where pos = firstExprPos expr


handlerStmtForEach :: Env -> Ident -> Type -> Expr -> Env
handlerStmtForEach env (Ident (pos,name)) rTy lExpr =env5
  where (env1,rType,_) = convertToRedArray env rTy
        (env2)         = addSymbol env1 name (VarInfo rType False pos Nothing False)
        (env3, eRes@(ExprRes exprType cTime valE constE)) = saExpr env2 lExpr
        (env4, arrayType) = case exprType of
                    Array lTy _ -> (fst $ checkComp env3 lTy array pos, array)
                      where array = checkArrays rType lTy
                    otherwise   -> (addError env3 (NotAnArrayForEach exprPos ,exprPos),exprType)
        (env5)         = addSymbol env4 name (VarInfo (arrayType) False pos Nothing False)
        exprPos         = firstExprPos lExpr

handlerStmtFor :: Env -> Ident -> Type -> Pos ->Expr -> Expr -> [Stmt] -> Dir -> Env
handlerStmtFor env sId sTy pos sExpr lExpr stmts dir = env3
  where env1 = handlerConstVarDef env sId sTy sExpr pos
        (exprEnvL,(ExprRes _ cTimeL valL _)) = saExpr env1 sExpr
        (exprEnv, (ExprRes exprType cTime valE constE)) = saExpr exprEnvL lExpr
        exprPos = firstExprPos lExpr
        env2 = checkStmtFor exprEnv cTimeL sTy pos "ex1"
        env3 = checkStmtFor env2 cTime exprType exprPos "ex2"

handlerStmtReturn :: Env -> Pos -> Env
handlerStmtReturn env pos
  | forL = addError env (ReturnInForLoop pos "return",pos)
  | True = env
  where forL = loopFor env

-------------------------------------------------
------------------- CHECKS ----------------------
-------------------------------------------------

checkIfReturnVariableIsInitialized :: Env -> Env
checkIfReturnVariableIsInitialized env =
  case val of
    True -> env
    otherwise -> addError env (UnitializedVariable id, pos)
  where (_,Just id@(Ident (pos@(l,c),name)), _, val,_, _) = funcCheck env

checkArithmeticTypes :: Env -> Type -> Type -> Type -> (Pos, String) -> Env
checkArithmeticTypes env e1Type e2Type retTy (opPos,op) =
  case retTy of
        ErrorType  | not (areErrors e1Type e2Type) ->addError env (ArithmeticError e1Type e2Type opPos op , opPos)
                   |otherwise  -> env
        otherwise -> env

checkLoopCmd::Env -> LoopCmd -> Env
checkLoopCmd env (LoopExitOn _ ex) =
    checkBooleanCondition exprEnv ex
    where (exprEnv, eRes@(ExprRes exprType cTime valE constE)) = saExpr env ex
checkLoopCmd env _ = env

checkConstExpr :: Env -> Expr -> (Env,Bool, Maybe Int)
checkConstExpr env expr
    | isError typeExpr = (envExpr, False, Nothing)
    | leqType typeExpr (BasicType TypeInt) && trust value <=0 = (addError env (NegativeSize pos, pos), False, Nothing)
    | leqType typeExpr (BasicType TypeInt) && cTimeExpr = (envExpr, True, value)
    | not (leqType typeExpr (BasicType TypeInt)) = (addError envExpr (NonIntegerExpr pos, pos), False, Nothing)
    | otherwise = (addError envExpr (NonConstantExpr pos, pos), False, Nothing)
    where (envExpr, eRes@(ExprRes typeExpr cTimeExpr value constE)) = saExpr env expr
          pos = firstExprPos  expr

checkEnvRedecl :: Env -> Ident ->Int -> (Env,Bool)
checkEnvRedecl env id errorKind
    | isSymbolDeclared env id = case errorKind of
          1 -> (addError env (RedeclaredVar id, (Loc 0 0) ), True)
          2 -> (addError env (RedeclaredFunction id, (Loc 0 0)), True)
          3 -> (addError env (RedeclaredParameter id, (Loc 0 0)), True)
    | otherwise = (env, False)

checkComp :: Env -> Type -> Type -> Pos -> (Env, Bool)
checkComp env ty1 ty2 pos
    | not (leqType ty1 ty2) =
        (addError env (TypeMismatch pos ty1 ty2, pos), False)
    | otherwise = (env, True)


checkFunctionNames :: Env -> Ident -> Ident -> Env
checkFunctionNames env (Ident (_,nameB)) (Ident (posE,nameE))
    | nameB /= nameE = addError env (FunctionNameMismatch posE nameB, posE)
    | otherwise      = env

checkFunctionParameters :: Env -> [Param] -> Env
checkFunctionParameters env [] = env
checkFunctionParameters env ((Parameter id@(Ident (pos,name)) modal ty):params)
    | redeclared = checkFunctionParameters rEnv params
    | otherwise  = checkFunctionParameters (addSymbol rEnv name (VarInfo rType const  pos Nothing const)) params
   where (envRedeclared, redeclared) = checkEnvRedecl env id 3
         (rEnv, rType) = checkArrayWithDimension env ty id
         const = case modal of
                 ModConst  -> True
                 otherwise -> False

checkDeclIdent :: Env -> Ident -> (Env, Maybe EnvEntry)
checkDeclIdent env id@(Ident (pos,_))=
    case isDeclVar env id  of
        Just varInfos -> (env, Just varInfos)
        Nothing       -> (addError env (NotDeclared id, pos), Nothing)

checkExprsAndParams :: Env -> [Expr] -> [Param] ->Ident -> Pos -> Type -> (Env, ExprRes)
checkExprsAndParams  env exprs params id@(Ident (pos,_)) posDef retTy
    | length exprs /= length params = (addError envWithError (FuncArgsMismatch exprTypes paramsTypes id, pos), exprError)
    | not areComp = (addError env (FuncTypesMismatch exprTypes paramsTypes id posDef, pos), exprError)
    | otherwise = (refEnv, (ExprRes retTy False Nothing False))
    where listExprType =  map (saExpr (envWithoutErrors env) ) exprs
          envs = map fst listExprType
          envWithError = env {errors = errors env ++(foldr (++) [] ( map errors envs) )}
          paramsTypes = [ty | (Parameter _ _ ty)<- params]
          exprTypes = [ty | (_,(ExprRes ty _ _ _ ))<- listExprType]
          areComp =  all (==True) (zipWith (leqType) exprTypes paramsTypes)
          refEnv = checkModalParameters envWithError exprs ([const | (_,(ExprRes _ _ _ const))<- listExprType]) params id 1

checkModalParameters :: Env ->[Expr]-> [Bool] -> [Param] -> Ident ->Int ->Env
checkModalParameters env (e:exprs) (c:consts) (p:params) id parNum
    |constError = checkModalParameters addressEnv exprs consts params id (parNum +1)
    |c && (not constPass) = checkModalParameters (addError env (NonConstPass pos parNum id, pos)) exprs consts params id (parNum +1)
    |not refPassR && exprType /= ttype && not (areErrors exprType ttype) = checkModalParameters (addError env (NonEqRefTypes pos parNum id ttype exprType, pos)) exprs consts params id (parNum +1)
    |refPassR = checkModalParameters (addError env (NonRefLeft pos parNum id, pos)) exprs consts params id (parNum +1)
    |otherwise = checkModalParameters env exprs consts params id (parNum +1)
    where constPass = case p of
                    (Parameter _ ModConst _) -> True
                    (Parameter _ ModVal _) -> True
                    otherwise -> False
          (refPassR,ttype) = case p of
                    (Parameter _ ModRef typ) ->case e of
                                  (ExprLeft _) -> (False, typ)
                                  (ExprAddress _) -> (False, typ)
                                  otherwise -> (True, typ)
                    (Parameter _ ModValRes typ) ->case e of
                                  (ExprLeft _) -> (False, typ)
                                  (ExprAddress _) -> (False, typ)
                                  otherwise -> (True, typ)
                    otherwise -> (False, ErrorType)
          (_, (ExprRes exprType _ _ _)) = saExpr env e
          pos = firstExprPos e
          (addressEnv,constError)=  checkAddressOfConst env c e pos 2
checkModalParameters env _ _ _ _ _= env

checkCaseExpr :: Env -> Type ->Expr -> Env
checkCaseExpr env ty expr =
  case equalitySupport ty of
    True -> env
    False -> addError env (CaseExprNotEq pos ty, pos)
  where pos = firstExprPos expr

checkUnaryTypes :: Env -> Type -> ExprRes -> Pos -> (Env, ExprRes)
checkUnaryTypes env typ eRes pos =
  case typ of
          ErrorType -> (env, eRes)
          BasicType TypeInt -> (env, eRes)
          BasicType TypeReal -> (env, eRes)
          PointerType _ ->
              (addError env (PointerArithmetic pos, pos), exprError)
          Array _ _ ->
              (addError env (PointerArithmetic pos, pos), exprError)
          otherwise ->
              (addError env (NonNumericValue pos, pos), exprError)

checkAddressOfConst :: Env -> Bool -> LExpr -> Pos -> Int -> (Env, Bool)
checkAddressOfConst env const expr pos kind
    | isAddressOfConst && kind==2 = (addError env (PassingRefOfConst pos, pos), True)
    | isAddressOfConst = (addError env (AssignConstToVar pos, pos), True)
    | otherwise = (env, False)
    where (_, (ExprRes ty _ _ _)) = saExpr env expr
          isAddressOfConst = case kind of
                                  0 -> const && (case expr of
                                                    (LExpr _) -> True
                                                    otherwise -> False)
                                  otherwise -> const && (case ty of
                                                    Pointer _ -> True
                                                    --Array _ _-> True
                                                    otherwise -> False
                                                || case expr of
                                                    (LExpr _) -> True
                                                    otherwise -> False)

checkModifyConstant:: Env -> Ident -> Bool -> Env
checkModifyConstant env id@(Ident (pos@(_),_)) constLE
    |constLE   = addError env (ChangingConstVar id, pos)
    |otherwise = env

checkBooleanCondition :: Env -> Expr -> Env
checkBooleanCondition env ex
    |exprType == BasicType TypeBool || exprType == ErrorType = exprEnv
    |otherwise = addError exprEnv (NonBooleanExpr pos exprType, pos)
    where pos = firstExprPos ex
          (exprEnv, eRes@(ExprRes exprType cTime valE _)) = saExpr env ex

convertToRedArray ::Env -> Type -> (Env,Type, Bool)
convertToRedArray env (ArrayType _ ty _) = (rEnv,Array rType 0, errorFounds)
  where (rEnv, rType,errorFounds) = convertToRedArray env ty

convertToRedArray env (ArrayDimType _ ty _ expr)
  | isConst   = (rEnv2, Array (rType) (trust value), errorFounds)
  | otherwise = (rEnv2, Array rType 0, True)
  where (rEnv, isConst, value) = checkConstExpr env expr
        (rEnv2, rType, errorFounds)= convertToRedArray rEnv ty
convertToRedArray env ty = case ty of
      PointerType pt -> (nEnv, (PointerType nType), errorFounds )
        where (nEnv, nType, errorFounds) = convertToRedArray env pt
      otherwise ->  (env, ty, False)

checkArrays :: Type -> Type ->  Type
checkArrays decl rType =
  case decl of
    Array aType 0 ->
        case rType of
          Array ty dim ->  Array (checkArrays aType ty) dim
          otherwise -> decl
    PointerType ty ->
        case rType of
          PointerType tty -> PointerType (checkArrays ty tty)
          otherwise -> decl
    otherwise -> decl

checkArrayWithDimension :: Env -> Type -> Ident-> (Env, Type)
checkArrayWithDimension env ty id@(Ident (pos,_))=
  case ty of
      ArrayType _ _ _ -> (addError nEnv (FunctionWithoutDimension id, pos), rTy)
      otherwise -> (nEnv, rTy)
     where (nEnv,rTy,_) = convertToRedArray env ty

checkReturnInitialized :: Env -> Ident -> Env
checkReturnInitialized env id =
  case  isDeclVar env id of
    Nothing -> env
    Just z ->
      case valRecord of
                Just y | y==z -> nEnv
                       | otherwise -> env
                otherwise ->  env
      where (lnames, maybeVar,retBool,boolFather,break, def) = funcCheck env
            nEnv = env {funcCheck = (lnames, bid, retBool || break, boolFather || break,break,def)}
            (bid, vName) = case maybeVar of
                              Just id@(Ident (_,name)) -> (Just id, name)
                              otherwise-> (Nothing, "");
            valRecord = case lnames of
                          Nothing -> Nothing
                          Just x-> Data.Map.lookup vName x
checkMain :: Env -> Env
checkMain env =
  case getValue env "main" of
    Nothing -> addError env (NoMain (0,0),(0,0))
    otherwise -> env

checkStmtFor :: Env -> Bool -> Type -> Pos -> String -> Env
checkStmtFor env ctime ty pos kind = env2
  where intComp | kind == "ex1" = ty == (BasicType TypeInt)
                | otherwise = leqType ty (BasicType TypeInt)
        env1 | intComp = env
             | otherwise = addError env (NotIntegerType pos ty, pos)
        env2 | ctime || not intComp = env1
             | otherwise = addError env1 (NotCompileTime pos, pos)

checkDetLoop :: Env -> Pos -> Env
checkDetLoop  env  pos
  |val = addError env (InfiniteLoop pos, pos)
  |otherwise = env
  where (_,_,_,_,val,_)= funcCheck env
-}




{-
returnInit :: Env ->  Ident -> Env
returnInit env id =
  env {funcCheck = (Just (names env), Just id, False, False,True, False)}

copyErrors :: Env -> Env -> Env
copyErrors dest source = dest {errors = (errors dest)++(errors source)}
-}



--------------------------------------------------
------------- ENVIRONMENT MANAGEMENT -------------
--------------------------------------------------

-- Iterate the list of formal parameters
addFormalParams :: Env -> [FormalParameters ()] -> Env
addFormalParams env [] = env
addFormalParams env (param:params) = addFormalParams env' params
  where env' = addFormalParam env param

-- Add the formal parameter to the environment
addFormalParam :: Env -> FormalParameters () -> Env
addFormalParam env (FormParam _ [] _) = env
addFormalParam env (FormParam mod (id:ids) ty) = addFormalParam env' (FormParam mod ids ty)
  where
    id' = enrichIdent id
    ty' = enrichType ty
    env' = addSymbol env id (ConstVarEntry ty False (getIdentLoc id))

addSymbol :: Env -> Ident () -> EnvEntry -> Env
addSymbol (env, errs) name record = (Map.insert name record env, errs)

addProcFun :: Env -> ProcedureAndFunctionHeading () -> Env
addProcFun env (ProcHead ident params) =
    addIfNotDeclared env ident 2 (ProcFunEntry params (BasicType Void) (getIdentLoc ident))
addProcFun env (FunHead ident params retTy) =
    addIfNotDeclared env ident 2 (ProcFunEntry params retTy (getIdentLoc ident))

addIfNotDeclared :: Env -> Ident () -> Int -> EnvEntry -> Env
addIfNotDeclared env ident errorKind symbolInfo = addSymbol env ident symbolInfo

addError :: Env -> (Error (), Loc) -> Env
addError (env, errs) (err, pos) = (env, errs ++ [(errorType err, pos)])

{-
createEnv :: Env -> Env
createEnv oldEnv = Env (Map.fromList defaultFuncList) (Just oldEnv) [] (loop oldEnv) (funcCheck oldEnv) (loopFor oldEnv)


isSymbolDeclared :: Env -> Ident -> Bool
isSymbolDeclared (env, _) id = Map.member id env

getValue :: Env -> Id -> Maybe EnvRecord
getValue env key = Data.Map.lookup key (names env)

isDeclVar :: Env -> Ident -> Maybe EnvRecord
isDeclVar env id@(Ident (_,name))
    | isSymbolDeclared env name =  getValue env name
    | otherwise = case parent env of
                         Just pEnv -> isDeclVar pEnv id
                         Nothing -> Nothing
getExprLen :: Env -> Expr -> Int
getExprLen env e =
    case e of
      ExprArray _ (x:xs) -> length (x:xs)
      ExprCreate _ _ _ dim ->
          case saExpr env dim of
               (_,(ExprRes _ _ (Just x) _)) -> x
               otherwise -> 0
      otherwise ->
          case saExpr env e of
            (_, (ExprRes (Array _ dim) _ _ _)) -> dim
            otherwise -> 0


maybeOp :: TACOp -> Maybe Int -> Maybe Int -> Maybe Int
maybeOp "+" (Just x) (Just y)  = Just (x+y)
maybeOp "*" (Just x) (Just y)  = Just (x*y)
maybeOp "-" (Just x) (Just y)  = Just (x-y)
maybeOp "/" (Just _) (Just _)  = Nothing
maybeOp "%" (Just x) (Just 0)  = Nothing
maybeOp "%" (Just x) (Just y)  = Just (mod x y)
maybeOp "^" (Just x) (Just y)  = Just (x^y)
maybeOp _ _ _ = Nothing

maybeNegative :: Maybe Int -> Maybe Int
maybeNegative (Just x) = (Just (-x))
maybeNegative Nothing = Nothing

translateParamsToRed :: [Param] -> [Param]
translateParamsToRed [] = []
translateParamsToRed ((Parameter id mod ty):par) = (Parameter id mod tyy):translateParamsToRed par
  where (_,tyy,_) = convertToRedArray defaultEnv ty


copyEnvInfo :: Env -> Env -> Maybe Env-> Env
copyEnvInfo dest source (Just source2) =
    dest {errors = (errors dest)++(errors source)++(errors source2),
          funcCheck = (oldN, oldId,  valSource && valSource2, valOld || (valSource && valSource2),
          break && b1 && b2, def)}
    where (oldN,oldId,valOld,_,break , def) = funcCheck dest
          (_,_,valSource,_,b1,_) = funcCheck source
          (_,_,valSource2,_,b2,_) = funcCheck source2

copyEnvInfo dest source Nothing =
  dest {errors = (errors dest)++(errors source),
        funcCheck = (oldN, oldId,  valSource , valOld || valSource,
          break, def)}
  where (oldN,oldId,valOld,_,break, def) = funcCheck dest
        (_,_,valSource,_,_,_)            = funcCheck source
-}
----------------------------------------------------------------------
------------------------- FOR CO-DEFINITIONS -------------------------
----------------------------------------------------------------------

{-
addFuncToEnv :: Env -> ProcedureAndFunctionHeading -> Env
addFuncToEnv env (FunDecl id params retTy) =
    addIfNotDeclared env id 2 (ProcEntry params (Just retTy) (Loc 0 0))
addFuncToEnv env (ProcDecl id params) =
    addIfNotDeclared env id 2 (ProcEntry params Nothing (Loc 0 0))

addFuncs :: Env -> Env
addFuncs env = env

addFuncsFromParams :: Env -> [Ident] -> Env
addFuncsFromParams env [] = env
addFuncsFromParams env (id:ids) = addFuncsFromParams (addSymbol env id (ProcEntry [] (BasicType Void) (Loc 0 0))) ids

addFuncsFromStmts :: Env -> [Stmt] -> Env
addFuncsFromStmts env [] = env
addFuncsFromStmts env ((StmtDecl d):ss) =
    addFuncsFromStmts (addFuncToEnv env d) ss
addFuncsFromStmts env (_:ss) = addFuncsFromStmts env ss

envWithoutErrors :: Env -> Env
envWithoutErrors env = env {errors = []}
-}



---------------------------------------------------
---------- GETTER/SETTER/QUERIES/HELPERS ----------
---------------------------------------------------

-- Helper function to extract just the identifier name
getIdentName :: Ident ty -> String
getIdentName (Ident name _ _) = name

-- Estrae la posizione di un identificatore
getIdentLoc :: Ident ty -> Loc
getIdentLoc (Ident _ loc _) = loc  -- Default, should be overridden if Ident contains position

-- Inferisce il tipo di una costante
inferConstantType :: Constant ty -> Type ty
inferConstantType (ConstInt _ _) = BasicType Integer
inferConstantType (ConstDouble _ _) = BasicType Real
inferConstantType (ConstStr _) = BasicType String

getTypeInfo :: Constant () -> TypeInfo
getTypeInfo const = case inferConstantType const of
    BasicType Integer -> TInteger
    BasicType Real -> TReal
    BasicType String -> TString

-- Cerca se un nome è già presente nell'environment; in caso affermativo torna l'entry corrispondente
lookupByName :: Env -> String -> Maybe EnvEntry
lookupByName (env, _) name =
    let entries = Map.toList env
        matching = filter (\(Ident n _ _, _) -> n == name) entries
    in case matching of
        [] -> Nothing
        (_, entry):_ -> Just entry



----------------------------------------------------------------------
------------------------ PRETTY-ENV (PRINTER) ------------------------
----------------------------------------------------------------------

printEnv :: Env -> String
printEnv (env, errs) = let
    entries = Map.toList env
    names = map (\(Ident n _ _, _) -> n) entries
    in
    "\nSymbol Table:" ++ showNames (Map.toList env) ++
    "\nErrors:\n" ++ showErrors errs

showNames :: [(Ident ty, EnvEntry)] -> String
showNames [] = ""
showNames (((Ident name loc _), entry):s) = "\n\t" ++ show name ++ ": " ++ showEnvEntry entry ++ showNames s

showEnvEntry :: EnvEntry -> String
showEnvEntry entry = case entry of
    ConstVarEntry ty isConst loc ->
        "Var " ++ showType ty ++ " (const: " ++ show isConst ++ ") at " ++ showLoc loc
    ProcFunEntry params retTy loc -> case retTy of
        (BasicType Void) ->
            "Procedure " ++ showParams params ++ " returns: " ++ showType retTy ++ " at " ++ showLoc loc
        _ ->
            "Function " ++ showParams params ++ " returns: " ++ showType retTy ++ " at " ++ showLoc loc

showLoc :: Loc -> String
showLoc (Loc line col) = "(" ++ show line ++ ", " ++ show col ++ ")"

showParams :: [FormalParameters ty] -> String
showParams [] = "()"
showParams params = "(" ++ intercalate ", " (map showParam params) ++ ")"

showParam :: FormalParameters ty -> String
showParam (FormParam mod ids ty) =
    show mod ++ " " ++ intercalate ", " (map (show . getIdentName) ids) ++ ": " ++ showType ty

showType :: Type ty -> String
showType ty = case ty of
    (BasicType Integer)   -> "Integer"
    (BasicType Real)      -> "Real"
    (BasicType Boolean)   -> "Boolean"
    (BasicType Character) -> "Character"
    (BasicType String)    -> "String"
    (BasicType Void)      -> "Void"


showErrors :: [(String, Loc)] -> String
showErrors [] = "No errors"
showErrors errs = intercalate "\n" (map showError errs)
    where showError (msg, loc) = showLoc loc ++ ": " ++ msg






----------------------------------------------------------------------
----------------------- ENRICHED AST FUNCTIONS -----------------------
----------------------------------------------------------------------

enrichProgram :: Program () -> Program TypeInfo
enrichProgram (Prog ident params block) =
  Prog (enrichIdentUnknown ident) (enrichFormalParametersList params) (enrichBlock block)

enrichFormalParametersList :: [FormalParameters ()] -> [FormalParameters TypeInfo]
enrichFormalParametersList = map enrichFormalParameters

enrichFormalParameters :: FormalParameters () -> FormalParameters TypeInfo
enrichFormalParameters (FormParam mod idents ty) =
  FormParam mod (map (`enrichIdent`ty) idents) (enrichType ty)

enrichBlock :: Block () -> Block TypeInfo
enrichBlock (Block consts vars procs compStmt) =
    Block (map enrichConstDef consts)
    (map enrichVarDecl vars)
    (map enrichProcFunDecl procs)
    (enrichCompoundStatement compStmt)

enrichConstDef :: ConstantDefinition () -> ConstantDefinition TypeInfo
enrichConstDef (ConstDef ident c) = ConstDef (enrichIdentUnknown ident) (enrichConst c)

enrichVarDecl :: VariableDeclaration () -> VariableDeclaration TypeInfo
enrichVarDecl (VarDecl ids ty) = VarDecl (map (`enrichIdent`ty) ids) (enrichType ty)

enrichProcFunDecl :: ProcedureAndFunctionDeclaration () -> ProcedureAndFunctionDeclaration TypeInfo
enrichProcFunDecl (ProcFunDecl heading block) = ProcFunDecl (enrichProcFunHeading heading) (enrichBlock block)

enrichProcFunHeading :: ProcedureAndFunctionHeading () -> ProcedureAndFunctionHeading TypeInfo
enrichProcFunHeading (ProcHead ident params) = ProcHead (enrichIdent ident (BasicType Void)) (enrichFormalParametersList params)
enrichProcFunHeading (FunHead ident params ty) = FunHead (enrichIdent ident ty) (enrichFormalParametersList params) (enrichType ty)

enrichCompoundStatement :: CompoundStatement () -> CompoundStatement TypeInfo
enrichCompoundStatement (CompStmt stmts) = CompStmt (map enrichStatement stmts)

enrichStatement :: Statement () -> Statement TypeInfo
enrichStatement = undefined  -- Da implementare se necessario

enrichIdent :: Ident () -> Type () -> Ident TypeInfo
enrichIdent (Ident name loc ()) (BasicType Integer) = Ident name loc TInteger
enrichIdent (Ident name loc ()) (BasicType Real) = Ident name loc TReal
enrichIdent (Ident name loc ()) (BasicType Boolean) = Ident name loc TBoolean
enrichIdent (Ident name loc ()) (BasicType Character) = Ident name loc TCharacter
enrichIdent (Ident name loc ()) (BasicType String) = Ident name loc TString
enrichIdent (Ident name loc ()) (BasicType Void) = Ident name loc TVoid
--enrichIdent (Ident name loc ()) (Pointer (BasicType Integer)) = Ident name loc TInteger
--enrichIdent (Ident name loc ()) (Array  [IndexType ty] (Type ty)) = Ident name loc TInteger

enrichIdentUnknown :: Ident () -> Ident TypeInfo
enrichIdentUnknown (Ident name loc _) = Ident name loc TUnknown


enrichType :: Type () -> Type TypeInfo
enrichType (Array idxs t) = Array (map enrichIndex idxs) (enrichType t)
enrichType (Pointer t)    = Pointer (enrichType t)
enrichType (BasicType b)  = BasicType b

enrichIndex :: IndexType () -> IndexType TypeInfo
enrichIndex (IndexRange c1 c2) = IndexRange (enrichConst c1) (enrichConst c2)

enrichConst :: Constant () -> Constant TypeInfo
enrichConst (ConstId s i)      = ConstId s (enrichIdentUnknown i)
enrichConst (ConstInt s n)     = ConstInt s n
enrichConst (ConstDouble s d)  = ConstDouble s d
enrichConst (ConstStr s)       = ConstStr s

{-
    paramsAnn = map annotateParams params
    (env', blockAnn) = checkBlock (addFormalParams defaultEnv paramsAnn) (annotateBlock block)
    identAnn = annotateIdent ident
    annotateIdent (Ident name loc ()) = Ident name loc TUnknown
    annotateParams (FormParam mod idents ty) = FormParam mod (map annotateIdent idents) (annotateType ty)
    annotateType (Array idxs t) = Array (map annotateIndex idxs) (annotateType t)
    annotateType (Pointer t) = Pointer (annotateType t)
    annotateType (BasicType b) = BasicType b
    annotateIndex (IndexRange c1 c2) = IndexRange (annotateConstant c1) (annotateConstant c2)
    annotateConstant (ConstId s i) = ConstId s (annotateIdent i)
    annotateConstant (ConstInt s n) = ConstInt s n
    annotateConstant (ConstDouble s d) = ConstDouble s d
    annotateConstant (ConstStr s) = ConstStr s
    annotateBlock (Block consts vars procs compStmt) = Block (map annotateConstDef consts) (map annotateVar vars) (map annotateProc procs) (annotateCompStmt compStmt)
    annotateConstDef (ConstDef i c) = ConstDef (annotateIdent i) (annotateConstant c)
    annotateVar (VarDecl ids ty) = VarDecl (map annotateIdent ids) (annotateType ty)
    annotateProc (ProcFunDecl h b) = ProcFunDecl (annotateHeading h) (annotateBlock b)
    annotateHeading (ProcHead i ps) = ProcHead (annotateIdent i) (map annotateParams ps)
    annotateHeading (FunHead i ps t) = FunHead (annotateIdent i) (map annotateParams ps) (annotateType t)
    annotateCompStmt (CompStmt stmts) = CompStmt (map annotateStmt stmts)
    annotateStmt s = case s of
      Stmts cs -> Stmts (annotateCompStmt cs)
      RExprStmt r -> RExprStmt (annotateRExpr r)
      Assign l r -> Assign (annotateLExpr l) (annotateRExpr r)
      While r st -> While (annotateRExpr r) (annotateStmt st)
      DoWhile st r -> DoWhile (annotateStmt st) (annotateRExpr r)
      ForTo l r1 r2 st -> ForTo (annotateLExpr l) (annotateRExpr r1) (annotateRExpr r2) (annotateStmt st)
      ForDownTo l r1 r2 st -> ForDownTo (annotateLExpr l) (annotateRExpr r1) (annotateRExpr r2) (annotateStmt st)
      If r st oe -> If (annotateRExpr r) (annotateStmt st) (annotateOptElse oe)
      Case r cs -> Case (annotateRExpr r) (map annotateCaseItem cs)
    annotateOptElse oe = case oe of
      NoElse -> NoElse
      Else st -> Else (annotateStmt st)
    annotateCaseItem (CaseItem c st) = CaseItem (annotateConstant c) (annotateStmt st)
    annotateLExpr l = case l of
      LId i -> LId (annotateIdent i)
      LArr l' rs -> LArr (annotateLExpr l') (map annotateRExpr rs)
      LDeref l' -> LDeref (annotateLExpr l')
    annotateRExpr r = case r of
      Binary r1 b r2 -> Binary (annotateRExpr r1) b (annotateRExpr r2)
      Unary u r' -> Unary u (annotateRExpr r')
      Lit l -> Lit l
      FCall i rs -> FCall (annotateIdent i) (map annotateRExpr rs)
      LExpr l' -> LExpr (annotateLExpr l')
-}