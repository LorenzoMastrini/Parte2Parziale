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



-----------------------------------------------------
-------------------- DATA & TYPE --------------------
-----------------------------------------------------

{-
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


data Env = Env
  { symbols :: Map.Map SimpleIdent EnvEntry -- Symbols in the environment
  , parent  :: Maybe Env                    -- Parent environment for nested blocks of code
  , errors  :: [(String, Loc)]              -- List of errors (error message, location)
  } deriving (Eq, Show)

data EnvEntry
    = ConstVarEntry SimpleType Bool (Maybe (Constant () () ())) Loc -- (type, is constant, value if constant, location)
    | ProcFunEntry [SimpleFormalParameters] SimpleType Loc          -- (parameters, return type, location)
    deriving (Eq, Show)



--------------------------------------------------------
-------------------- DEFAULT VALUES --------------------
--------------------------------------------------------

--exprError :: ExprRes
--exprError = ExprRes ErrorType False Nothing False

-- Default functions
defaultFuncList :: [(SimpleIdent, EnvEntry)]
defaultFuncList =  [ (Ident "writeInt"    (Loc 0 0) () () (), ProcFunEntry [FormParam ByVal [Ident "valInt" (Loc 0 0) () () ()] (BasicType Integer)]    (BasicType Void)      (Loc 0 0))
                   , (Ident "writeReal"   (Loc 0 0) () () (), ProcFunEntry [FormParam ByVal [Ident "valReal" (Loc 0 0) () () ()] (BasicType Real)]      (BasicType Void)      (Loc 0 0))
                   , (Ident "writeChar"   (Loc 0 0) () () (), ProcFunEntry [FormParam ByVal [Ident "valChar" (Loc 0 0) () () ()] (BasicType Character)] (BasicType Void)      (Loc 0 0))
                   , (Ident "writeString" (Loc 0 0) () () (), ProcFunEntry [FormParam ByVal [Ident "valString" (Loc 0 0) () () ()] (BasicType String)]  (BasicType Void)      (Loc 0 0))
                   , (Ident "readInt"     (Loc 0 0) () () (), ProcFunEntry [FormParam ByRef [Ident "valInt" (Loc 0 0) () () ()] (BasicType String)]     (BasicType Integer)   (Loc 0 0))
                   , (Ident "readReal"    (Loc 0 0) () () (), ProcFunEntry [FormParam ByRef [Ident "valReal" (Loc 0 0) () () ()] (BasicType String)]    (BasicType Real)      (Loc 0 0))
                   , (Ident "readChar"    (Loc 0 0) () () (), ProcFunEntry [FormParam ByRef [Ident "valChar" (Loc 0 0) () () ()] (BasicType String)]    (BasicType Character) (Loc 0 0))
                   , (Ident "readString"  (Loc 0 0) () () (), ProcFunEntry [FormParam ByRef [Ident "valString" (Loc 0 0) () () ()] (BasicType String)]  (BasicType String)    (Loc 0 0))]

defaultEnv :: Env
defaultEnv = Env { symbols = Map.fromList defaultFuncList, parent = Nothing, errors = [] }

---------------------------------------------------------
-------------------- STATIC ANALYSIS --------------------
---------------------------------------------------------

-- The static analysis updates the informations of the AST needed in subsequent step like TAC generation

-- Called in Compiler.hs. In the environment structure there are all the possible errors.
checkProgram :: SimpleProgram -> (Env, Program TypeInfo VarInfo FunInfo)
checkProgram (Prog ident params block loc) =
    let
        (env1, paramsEnriched) = checkFormalParametersList defaultEnv params
        (env2, blockEnriched) = checkBlock env1 block
    in (env2, Prog (enrichIdentUnknown ident) paramsEnriched blockEnriched loc)

checkFormalParametersList :: Env -> [SimpleFormalParameters] -> (Env, [FormalParameters TypeInfo VarInfo FunInfo])
checkFormalParametersList env [] = (env, [])
checkFormalParametersList env (params:paramsList) =
    let
        (env1, paramsEnriched) = checkFormalParameters env params
        (env2, paramsListEnriched) = checkFormalParametersList env1 paramsList
    in (env2, paramsEnriched:paramsListEnriched)

checkFormalParameters :: Env -> SimpleFormalParameters -> (Env, FormalParameters TypeInfo VarInfo FunInfo)
checkFormalParameters env (FormParam mod idents ty) =
    let
        tyEnriched = enrichType ty
        (env1, identsEnriched) = checkIdentsList env idents ty (VarInfo Nothing False) NoFunInfo
    in (env1, FormParam mod identsEnriched tyEnriched)

checkBlock :: Env -> SimpleBlock -> (Env, Block TypeInfo VarInfo FunInfo)
checkBlock env (Block constDefList varDeclList procFunDeclList compStmt) =
    let
        -- Don't create new scope if already in a function scope
        localEnv = case parent env of
            Just _ -> env  -- Already in a function, use the same environment
            Nothing -> defaultEnv  -- Program uses the default environment
        
        -- TODO: First pass (for mutual recursion): collect all function declarations
        --envWithFuncs = collectFunctionDeclarations localEnv procFunDeclList
        -- Second pass: analyze constants, variables, and function bodies
        (env1, constDefListEnriched) = checkConstDefList localEnv constDefList
        (env2, varDeclListEnriched) = checkVarDeclList env1 varDeclList
        (env3, procFunDeclListEnriched) = checkProcFunDeclList env2 procFunDeclList
        (env4, compStmtEnriched) = checkCompoundStatement env3 compStmt
        mergedEnv = mergeEnvs env env4
    in (mergedEnv, Block constDefListEnriched varDeclListEnriched procFunDeclListEnriched compStmtEnriched)



------------------------------------------------------
-------------------- DECLARATIONS --------------------
------------------------------------------------------

-- Iterate the list of constant definitions 
checkConstDefList :: Env -> [SimpleConstantDefinition] -> (Env, [ConstantDefinition TypeInfo VarInfo FunInfo])
checkConstDefList env [] = (env, [])
checkConstDefList env (d:ds) =
    let
        (env1, dEnr) = checkConstDef env d
        (env2, dsEnr) = checkConstDefList env1 ds
    in (env2, dEnr:dsEnr)

-- Check if the constant identifier is already used inside the environment
checkConstDef :: Env -> SimpleConstantDefinition -> (Env, ConstantDefinition TypeInfo VarInfo FunInfo)
checkConstDef env def@(ConstDef ident constVal) =
    let
        ty = inferConstantType constVal
        (env1, identEnriched) = checkIdent env ident ty (VarInfo (Just constVal) True) NoFunInfo
    in (env1, ConstDef identEnriched (enrichConst constVal))

-- Iterate the list of variable declarations
checkVarDeclList :: Env -> [SimpleVariableDeclaration] -> (Env, [VariableDeclaration TypeInfo VarInfo FunInfo])
checkVarDeclList env [] = (env, [])
checkVarDeclList env (d:ds) =
    let
        (env1, dEnr) = checkVarDecl env d
        (env2, dsEnr) = checkVarDeclList env1 ds
    in (env2, dEnr:dsEnr)

checkVarDecl :: Env -> SimpleVariableDeclaration -> (Env, VariableDeclaration TypeInfo VarInfo FunInfo)
checkVarDecl env (VarDecl ids ty) = checkVarDeclSequence env ids ty

-- Iterate the sequence of variable declarations that has the same type (ex. a, b, c: Integer)
checkVarDeclSequence :: Env -> [SimpleIdent] -> SimpleType -> (Env, VariableDeclaration TypeInfo VarInfo FunInfo)
checkVarDeclSequence env [] ty = (env, VarDecl [] (enrichType ty))
checkVarDeclSequence env (id:ids) ty =
    let
        (env1, idEnr) = checkIdent env id ty (VarInfo Nothing False) NoFunInfo
        (env2, VarDecl idsEnr tyEnr) = checkVarDeclSequence env1 ids ty
    in (env2, VarDecl (idEnr:idsEnr) tyEnr)

-- Iterate the list of procedures and functions declarations
checkProcFunDeclList :: Env -> [SimpleProcedureAndFunctionDeclaration] -> (Env, [ProcedureAndFunctionDeclaration TypeInfo VarInfo FunInfo])
checkProcFunDeclList env [] = (env, [])
checkProcFunDeclList env (d:ds) =
    let (env1, dEnr) = checkProcFunDecl env d
        (env2, dsEnr) = checkProcFunDeclList env1 ds
    in (env2, dEnr:dsEnr)

checkProcFunDecl :: Env -> SimpleProcedureAndFunctionDeclaration -> (Env, ProcedureAndFunctionDeclaration TypeInfo VarInfo FunInfo)
checkProcFunDecl env (ProcFunDecl heading block) =
    let
        -- Create a scope for the function
        localEnv = enterScope env
        (localEnv1, headingEnriched) = checkProcFunHead localEnv heading
        (finalLocalEnv, blockEnriched) = checkBlock localEnv1 block
        Just outerEnv = parent finalLocalEnv -- Get the updated outer environment
        -- Propagate the errors to the outer environment
        finalOuterEnv = outerEnv { errors = errors outerEnv ++ errors finalLocalEnv }
    in (finalOuterEnv, ProcFunDecl headingEnriched blockEnriched)

checkProcFunHead :: Env -> SimpleProcedureAndFunctionHeading -> (Env, ProcedureAndFunctionHeading TypeInfo VarInfo FunInfo)
checkProcFunHead localEnv head = case head of
    (ProcHead ident params) ->
        let
            -- Necessary to populate FunInfo (but parameters will be enriched again)
            preEnrichedParams = enrichFormalParametersList params
            -- Add identifier to the parent environment (not the local one)
            Just outerEnv = parent localEnv
            (outerEnv1, identEnriched) = checkProcFunIdent outerEnv ident (BasicType Void) NoVarInfo (FunInfo (BasicType Void) params) params
            -- Also add the function identifier to the local environment for recursion
            (localEnvWithFunc, _) = checkProcFunIdent localEnv ident (BasicType Void) NoVarInfo (FunInfo (BasicType Void) params) params
            -- Add parameters only to the local environment
            (localEnv1, paramsEnriched) = checkFormalParametersList localEnvWithFunc params
            -- Update the parent of the local environment
            updatedLocalEnv = localEnv1 { parent = Just outerEnv1 }
        in (updatedLocalEnv, ProcHead identEnriched paramsEnriched)
    (FunHead ident params retTy) ->
        let
            preEnrichedParams = enrichFormalParametersList params
            retTyEnriched = enrichType retTy
            -- Add identifier to the parent environment (not the local one)
            Just outerEnv = parent localEnv
            (outerEnv1, identEnriched) = checkProcFunIdent outerEnv ident retTy NoVarInfo (FunInfo retTy params) params
            -- Also add the function identifier to the local environment for recursion
            (localEnvWithFunc, _) = checkProcFunIdent localEnv ident retTy NoVarInfo (FunInfo retTy params) params
            -- Add parameters to the local environment
            (localEnv1, paramsEnriched) = checkFormalParametersList localEnvWithFunc params
            -- Update the parent of the local environment
            updatedLocalEnv = localEnv1 { parent = Just outerEnv1 }
        in (updatedLocalEnv, FunHead identEnriched paramsEnriched retTyEnriched)

-- Given a list of identifiers, check recursively if each of them is already used inside the environment
checkIdentsList :: Env -> [SimpleIdent] -> SimpleType -> VarInfo -> FunInfo -> (Env, [Ident TypeInfo VarInfo FunInfo])
checkIdentsList env [] _ _ _ = (env, [])
checkIdentsList env (i:is) ty varInfo funInfo =
    let (env', iEnr) = checkIdent env i ty varInfo funInfo
        (envFinal, isEnr) = checkIdentsList env' is ty varInfo funInfo
    in (envFinal, iEnr : isEnr)

-- Check if the identifier is already used inside the environment. If not add it to the environment
checkIdent :: Env -> SimpleIdent -> SimpleType -> VarInfo -> FunInfo -> (Env, Ident TypeInfo VarInfo FunInfo)
checkIdent env ident@(Ident name loc _ _ _) ty varInfo@(VarInfo constVal isConst) funInfo =
    let res = lookupByName env name
    in case res of
        Just (ConstVarEntry _ entryIsConst _ _, False) ->
            if entryIsConst then
                (addError env (RedeclaredConst ident, loc), enrichIdentUnknown ident)
            else
                (addError env (RedeclaredVar ident, loc), enrichIdentUnknown ident)
        Just (ProcFunEntry {}, False) ->
            (addError env (RedeclaredProcFun ident, loc), enrichIdentUnknown ident)
        _ ->
            (addEntry env ident (ConstVarEntry ty isConst constVal loc), enrichIdent ident ty varInfo funInfo)

-- Aggiorna l'environment con la dichiarazione di una funzione/procedura e restituisce l'identificatore arricchito
checkProcFunIdent :: Env -> SimpleIdent -> SimpleType -> VarInfo -> FunInfo -> [SimpleFormalParameters] -> (Env, Ident TypeInfo VarInfo FunInfo)
checkProcFunIdent env ident ty NoVarInfo (FunInfo retTy paramsEnriched) simpleParams =
    let res = lookupByName env (getIdentName ident)
    in case res of
        Just (ConstVarEntry {}, _) ->
            (addError env (RedeclaredConst ident, getIdentLoc ident), enrichIdentUnknown ident)
        Just (ProcFunEntry {}, _) ->
            (addError env (RedeclaredProcFun ident, getIdentLoc ident), enrichIdentUnknown ident)
        Nothing ->
            if retTy == BasicType Void
                then (addEntry env ident (ProcFunEntry simpleParams (BasicType Void) (getIdentLoc ident)), enrichIdent ident ty NoVarInfo (FunInfo retTy paramsEnriched))
                else (addEntry env ident (ProcFunEntry simpleParams ty (getIdentLoc ident)), enrichIdent ident ty NoVarInfo (FunInfo retTy paramsEnriched))
checkProcFunIdent env ident ty (VarInfo _ isConst) funInfo _ =
    let res = lookupByName env (getIdentName ident)
    in case res of
        Just (ConstVarEntry {}, _) ->
            (addError env (RedeclaredConst ident, getIdentLoc ident), enrichIdentUnknown ident)
        Just (ProcFunEntry {}, _) ->
            (addError env (RedeclaredProcFun ident, getIdentLoc ident), enrichIdentUnknown ident)
        Nothing -> (addEntry env ident (ConstVarEntry ty isConst Nothing (getIdentLoc ident)), enrichIdent ident ty (VarInfo Nothing isConst) funInfo)



-----------------------------------------------------
-------------------- EXPRESSIONS --------------------
-----------------------------------------------------

checkRExpr :: Env -> SimpleRExpr -> (Env, RExpr TypeInfo VarInfo FunInfo)
checkRExpr env expr = case expr of
    Binary e1 op e2 _ ->
        let
            (env1, e1Enriched) = checkRExpr env e1
            (env2, e2Enriched) = checkRExpr env1 e2
            t1 = getRExprType e1Enriched
            t2 = getRExprType e2Enriched
            (env3, typeInfo) = checkBinaryOp env op t1 t2
        in (env3, Binary e1Enriched op e2Enriched typeInfo)
    Unary op e _ ->
        let
            (env1, eEnriched) = checkRExpr env e
            t = getRExprType eEnriched
            (env3, typeInfo) = checkUnaryOp env op t
        in (env3, Unary op eEnriched typeInfo)
    Lit lit _ -> (env, enrichLitExpr lit)
    FCall ident@(Ident name _ _ _ _) args loc -> case lookupByName env name of
        Nothing ->
            (addError env (FunNotDefined ident, loc), FCall (enrichIdentUnknown ident) [] loc) -- Function doesn't exist
        Just (ConstVarEntry {}, _) ->
            (addError env (NotAFunction ident, loc), FCall (enrichIdentUnknown ident) [] loc) -- Not a function
        Just (ProcFunEntry params retTy _, _) ->
            let
                (env1, argsEnriched, hasTypeError) = checkArgsNumWithTypeError env args (paramsToList params) loc
                identEnriched = if hasTypeError
                    then setIdentTypeInfo (enrichIdent ident retTy NoVarInfo (FunInfo retTy params)) TUnknown
                    else enrichIdent ident retTy NoVarInfo (FunInfo retTy params)
            in (env1, FCall identEnriched argsEnriched loc)
    Ref op@(UnaryRef loc) rexpr _ ->
        let
            (env1, rexprEnriched) = checkRExpr env rexpr
            isLExprResult = isLExpr rexprEnriched
        in if isLExprResult
            then let
                t = getRExprType rexprEnriched
                typeInfo = TPointer t
                in (env1, Ref op rexprEnriched typeInfo)
            else (addError env1 (NotLExpr op, loc), Ref op rexprEnriched TUnknown)
    LExpr lexpr _ ->
        let
            (env1, lexprEnriched) = checkLExpr env lexpr
            typeInfo = getLExprTypeInfo lexprEnriched
        in (env1, LExpr lexprEnriched typeInfo)

checkArgsNumWithTypeError :: Env -> [SimpleRExpr] -> [SimpleFormalParameters] -> Loc -> (Env, [RExpr TypeInfo VarInfo FunInfo], Bool)
checkArgsNumWithTypeError env [] [] loc = (env, [], False)
checkArgsNumWithTypeError env [] params loc =
    let env1 = addError env (FunArgsNumMismatch 0 (length params), loc)
    in (env1, [], True)
checkArgsNumWithTypeError env args [] loc =
    let env1 = addError env (FunArgsNumMismatch (length args) 0, loc)
    in (env1, [], True)
checkArgsNumWithTypeError env (a:as) (p:ps) loc =
    let
        (env1, argEnriched) = checkRExpr env a
        (env2, compatible) = checkArgType env1 argEnriched p loc
        (env3, argsEnriched, hasTypeErrorRest) = checkArgsNumWithTypeError env2 as ps loc
        hasTypeError = not compatible || hasTypeErrorRest
    in (env3, argEnriched : argsEnriched, hasTypeError)

checkArgType :: Env -> RExpr TypeInfo VarInfo FunInfo -> SimpleFormalParameters -> Loc -> (Env, Bool)
checkArgType env argEnriched (FormParam _ _ paramType) loc =
    let
        argType = getRExprType argEnriched
        (env1, compatible) = argCompatible env argType paramType loc
    in (env1, compatible)

checkLExpr :: Env -> SimpleLExpr -> (Env, LExpr TypeInfo VarInfo FunInfo)
checkLExpr env lexpr = case lexpr of
    LId ident _ ->
        let
            (env1, identEnriched) = identExists env ident
            typeInfo = getIdentTypeInfo identEnriched
        in (env1, LId identEnriched typeInfo)
    LArr lexpr' indices _ loc ->
        let
            (env1, lexprEnriched) = checkLExpr env lexpr'
            (env2, indicesEnriched) = checkIndices env1 indices
            isArrayResult = isArray lexprEnriched
        in if isArrayResult
            then
                let
                    t = getLExprType lexprEnriched
                    (TArray i ty) = typeInfoFromType t
                    --(env3, indicesEnrichedChecked) = checkRange env2 lexpr' indices -------------------------- TODO
                in (env2, LArr lexprEnriched indicesEnriched (typeInfoFromType ty) loc)
            else (addError env1 (NotAnArray, loc), LArr lexprEnriched indicesEnriched TUnknown loc)
    LDeref lexpr' _ loc ->
        let
            (env1, lexprEnriched) = checkLExpr env lexpr'
            isPointerResult = isPointer lexprEnriched
        in if isPointerResult
            then let
                (Pointer t) = getLExprType lexprEnriched
                typeInfo = typeInfoFromType t
                in (env1, LDeref lexprEnriched typeInfo loc)
            else (addError env1 (NotAPointer, loc), LDeref lexprEnriched TUnknown loc)

checkIndices :: Env -> [SimpleRExpr] -> (Env, [RExpr TypeInfo VarInfo FunInfo])
checkIndices env [] = (env, [])
checkIndices env (index:indices) =
    let
        (env1, indexEnriched) = checkRExpr env index
        (env2, indicesEnriched) = checkIndices env1 indices
    in (env2, indexEnriched:indicesEnriched)



----------------------------------------------------
-------------------- STATEMENTS --------------------
----------------------------------------------------

checkCompoundStatement :: Env -> SimpleCompoundStatement -> (Env, CompoundStatement TypeInfo VarInfo FunInfo)
checkCompoundStatement env (CompStmt stmts) =
    let
        (env1, stmtsEnriched) = checkStatementList env stmts
    in (env1, CompStmt stmtsEnriched)

checkStatementList :: Env -> [SimpleStatement] -> (Env, [Statement TypeInfo VarInfo FunInfo])
checkStatementList env [] = (env, [])
checkStatementList env (stmt:stmts) =
    let
        (env1, stmtEnriched) = checkStatement env stmt
        (env2, stmtsEnriched) = checkStatementList env1 stmts
    in (env2, stmtEnriched:stmtsEnriched)

checkStatement :: Env -> SimpleStatement -> (Env, Statement TypeInfo VarInfo FunInfo)
checkStatement env stmt = case stmt of
    Stmts compStmt ->
        let
            (env1, compStmtEnriched) = checkCompoundStatement env compStmt
        in (env1, Stmts compStmtEnriched)
    RExprStmt expr _ ->
        let
            (env1, exprEnriched) = checkRExpr env expr
        in (env1, RExprStmt exprEnriched (getRExprTypeInfo exprEnriched))
    Assign lexpr rexpr loc ->
        let
            (env1, lexprEnriched) = checkLExpr env lexpr
            (env2, rexprEnriched) = checkRExpr env1 rexpr
            (env3, compatible) = compatibleAssignment env2 (getLExprType lexprEnriched) (getRExprType rexprEnriched) (Loc 0 0)
        in (env3, Assign lexprEnriched rexprEnriched loc)
    While rexpr stmt ->
        let
            (env1, rexprEnriched) = checkRExpr env rexpr
            (env2, stmtEnriched) = checkStatement env1 stmt
        in (env2, While rexprEnriched stmtEnriched)
    DoWhile stmt rexpr ->
        let
            (env1, stmtEnriched) = checkStatement env stmt
            (env2, rexprEnriched) = checkRExpr env1 rexpr
        in (env2, DoWhile stmtEnriched rexprEnriched)
    ForTo lexpr rexpr1 rexpr2 stmt ->
        let
            (env1, lexprEnriched) = checkLExpr env lexpr
            (env2, rexpr1Enriched) = checkRExpr env1 rexpr1
            (env3, rexpr2Enriched) = checkRExpr env2 rexpr2
            (env4, stmtEnriched) = checkStatement env stmt
        in (env4, ForTo lexprEnriched rexpr1Enriched rexpr2Enriched stmtEnriched)

-- Verifica la compatibilità dei tipi in un'assegnazione
compatibleAssignment :: Env -> SimpleType -> SimpleType -> Loc -> (Env, Bool)
compatibleAssignment env targetType sourceType loc = case (targetType, sourceType) of
    (t1, t2) | t1 == t2 -> (env, True) -- Stesso tipo
    
    -- Casting numerici
    (BasicType Real, BasicType Integer) -> (env, True) -- int -> real
    (BasicType Integer, BasicType Real) -> (env, True)  -- real -> int (con perdita di precisione)
    
    -- Casting tra caratteri e stringhe
    (BasicType String, BasicType Character) -> (env, True) -- char -> string
    (BasicType Character, BasicType String) -> (env, True)  -- string -> char (primo carattere)
    
    -- Casting tra array e puntatori
    (Array _ elemType, Pointer ptrType) -> if elemType == ptrType 
        then (env, True)
        else let
            env1 = addError env (IncompatibleArrPtrType, loc)
        in (env1, False)
    (Pointer ptrType, Array _ elemType) -> if ptrType == elemType 
        then (env, True) 
        else let
            env1 = addError env (IncompatiblePtrArrType, loc)
        in (env1, False)
    
    -- Tipi incompatibili
    _ -> let
            env1 = addError env (IncompatibleAssignment targetType sourceType, loc)
        in (env1, False)

------------------------------------------------
-------------------- CHECKS --------------------
------------------------------------------------

identExists :: Env -> SimpleIdent -> (Env, Ident TypeInfo VarInfo FunInfo)
identExists env ident@(Ident name loc _ varInfo funInfo) = case lookupByName env name of
    Nothing ->
        (addError env (UnknownIdentifier ident, loc), enrichIdentUnknown ident) -- Identifier doesn't exist
    Just (ConstVarEntry ty isConst constVal entryLoc, _) ->
        (env, enrichIdent ident ty (VarInfo constVal isConst) NoFunInfo)
    Just (ProcFunEntry params retTy loc, _) ->
        (env, enrichIdent ident retTy NoVarInfo (FunInfo retTy params))

-------------------- Binary --------------------
checkBinaryOp :: Env -> Binary -> SimpleType -> SimpleType -> (Env, TypeInfo)
checkBinaryOp env op t1 t2 = case op of
    BinaryPlus loc      -> checkNumericOp env "+" t1 t2 loc     -- "+"
    BinaryMinus loc     -> checkNumericOp env "-" t1 t2 loc     -- "-"
    BinaryMul loc       -> checkNumericOp env "*" t1 t2 loc     -- "*"
    BinaryDiv loc       -> checkNumericOp env "/" t1 t2 loc     -- "/"
    BinaryIntDiv loc    -> checkIntegerOp env "div" t1 t2 loc   -- "div"
    BinaryMod loc       -> checkIntegerOp env "mod" t1 t2 loc   -- "mod"
    BinaryOr loc        -> checkBooleanOp env "or" t1 t2 loc    -- "or"
    BinaryAnd loc       -> checkBooleanOp env "and" t1 t2 loc   -- "and"
    BinaryEq loc        -> checkComparisonOp env "=" t1 t2 loc  -- "="
    BinaryNeq loc       -> checkComparisonOp env "<>" t1 t2 loc -- "<>"
    BinaryLt loc        -> checkComparisonOp env "<" t1 t2 loc  -- "<"
    BinaryLte loc       -> checkComparisonOp env "<=" t1 t2 loc -- "<="
    BinaryGt loc        -> checkComparisonOp env ">" t1 t2 loc  -- ">"
    BinaryGte loc       -> checkComparisonOp env ">=" t1 t2 loc -- ">="

checkNumericOp :: Env -> String -> SimpleType -> SimpleType -> Loc -> (Env, TypeInfo)
checkNumericOp env op t1 t2 loc = case (t1, t2) of
    (incomp1@(BasicType Boolean), incomp2) ->
        (addError env (IncompatibleTypes op incomp1 incomp2, loc), TUnknown)
    (incomp1@(BasicType Character), incomp2) ->
        (addError env (IncompatibleTypes op incomp1 incomp2, loc), TUnknown)
    (incomp1@(BasicType String), incomp2) ->
        (addError env (IncompatibleTypes op incomp1 incomp2, loc), TUnknown)
    (incomp1@(BasicType Void), incomp2) ->
        (addError env (IncompatibleTypes op incomp1 incomp2, loc), TUnknown)
    (incomp1, incomp2@(BasicType Boolean)) ->
        (addError env (IncompatibleTypes op incomp1 incomp2, loc), TUnknown)
    (incomp1, incomp2@(BasicType Character)) ->
        (addError env (IncompatibleTypes op incomp1 incomp2, loc), TUnknown)
    (incomp1, incomp2@(BasicType String)) ->
        (addError env (IncompatibleTypes op incomp1 incomp2, loc), TUnknown)
    (incomp1, incomp2@(BasicType Void)) ->
        (addError env (IncompatibleTypes op incomp1 incomp2, loc), TUnknown)
    _ ->
        let
            t1' = promoteNumericType t1
            t2' = promoteNumericType t2
        in if t1' == BasicType Real || t2' == BasicType Real
            then (env, TReal)
            else (env, TInteger)

checkIntegerOp :: Env -> String -> SimpleType -> SimpleType -> Loc -> (Env, TypeInfo)
checkIntegerOp env op t1 t2 loc = let
    (env1, tyInfo1) = checkIntegerType env op t1 loc
    (env2, tyInfo2) = checkIntegerType env1 op t2 loc
    in if tyInfo1 == TInteger && tyInfo2 == TInteger
        then (env2, TInteger)
        else (env2, TUnknown)

checkBooleanOp :: Env -> String -> SimpleType -> SimpleType -> Loc -> (Env, TypeInfo)
checkBooleanOp env op t1 t2 loc = let
        (env1, tyInfo1) = checkBooleanType env op t1 loc
        (env2, tyInfo2) = checkBooleanType env op t2 loc
    in if tyInfo1 == TBoolean && tyInfo2 == TBoolean
        then (env2, TBoolean)
        else (env2, TUnknown)

checkComparisonOp :: Env -> String -> SimpleType -> SimpleType -> Loc -> (Env, TypeInfo)
checkComparisonOp env "<" t1@(BasicType Boolean) t2@(BasicType Boolean) loc = (addError env (NotBooleanOperation "<" t1 t2, loc), TUnknown)
checkComparisonOp env "<=" t1@(BasicType Boolean) t2@(BasicType Boolean) loc = (addError env (NotBooleanOperation "<=" t1 t2, loc), TUnknown)
checkComparisonOp env ">" t1@(BasicType Boolean) t2@(BasicType Boolean) loc = (addError env (NotBooleanOperation ">" t1 t2, loc), TUnknown)
checkComparisonOp env ">=" t1@(BasicType Boolean) t2@(BasicType Boolean) loc = (addError env (NotBooleanOperation ">=" t1 t2, loc), TUnknown)
checkComparisonOp env op t1 t2 loc = if t1 == t2
    then (env, TBoolean)
    else case (t1, t2) of
        (BasicType Integer, BasicType Real) -> (env, TBoolean) -- implicit cast of t1
        (BasicType Real, BasicType Integer) -> (env, TBoolean) -- implicit cast of t2
        (BasicType Character, BasicType String) -> (env, TBoolean) -- implicit cast of t1
        (BasicType String, BasicType Character) -> (env, TBoolean) -- implicit cast of t2
        _ -> (addError env (ComparisonTypeMismatch op t1 t2, loc), TUnknown) -- incompatible types

-------------------- Unary --------------------
checkUnaryOp :: Env -> Unary -> SimpleType -> (Env, TypeInfo)
checkUnaryOp env op t = case op of
    UnaryPlus loc ->
        let
            (env1, typeInfo) = checkNumericType env "+" t loc
        in (env1, typeInfo)
    UnaryMinus loc ->
        let
            (env1, typeInfo) = checkNumericType env "-" t loc
        in (env1, typeInfo)
    UnaryNot loc ->
        let
            (env1, typeInfo) = checkBooleanType env "not" t loc
        in (env1, typeInfo)

checkIntegerType :: Env -> String -> SimpleType -> Loc -> (Env, TypeInfo)
checkIntegerType env op t loc = case t of
    BasicType Integer -> (env, TInteger)
    _ -> (addError env (NotIntegerType op t, loc), TUnknown)

checkBooleanType :: Env -> String -> SimpleType -> Loc -> (Env, TypeInfo)
checkBooleanType env op t loc = case t of
    BasicType Boolean -> (env, TBoolean)
    -- BasicType Integer -> (env, TBoolean) -- 0 = false, non-zero = true
    -- BasicType Real -> (env, TBoolean)    -- 0.0 = false, non-zero = true
    _ -> (addError env (NotBooleanType op t, loc), TUnknown)

checkNumericType :: Env -> String -> SimpleType -> Loc -> (Env, TypeInfo)
checkNumericType env op t loc = case t of
    BasicType Integer -> (env, TInteger)
    BasicType Real -> (env, TReal)
    _ -> (addError env (NotNumericType op t, loc), TUnknown)

-------------------- FCall --------------------
argCompatible :: Env -> SimpleType -> SimpleType -> Loc -> (Env, Bool)
argCompatible env argType paramType loc = if argType /= paramType
    then (addError env (FunArgTypeMismatch argType paramType, loc), False)
    else (env, True)

{-
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

checkCaseExpr :: Env -> Type -> Expr -> Env
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


returnInit :: Env ->  Ident -> Env
returnInit env id =
  env {funcCheck = (Just (names env), Just id, False, False,True, False)}

copyErrors :: Env -> Env -> Env
copyErrors dest source = dest {errors = (errors dest)++(errors source)}
-}



----------------------------------------------------------------
-------------------- ENVIRONMENT MANAGEMENT --------------------
----------------------------------------------------------------

-- Iterate the list of formal parameters
addFormalParams :: Env -> [SimpleFormalParameters] -> Env
addFormalParams env [] = env
addFormalParams env (param:params) = addFormalParams env' params
  where env' = addFormalParam env param

-- Add the formal parameter to the environment
addFormalParam :: Env -> SimpleFormalParameters -> Env
addFormalParam env (FormParam _ [] _) = env
addFormalParam env (FormParam mod (id:ids) ty) = addFormalParam env' (FormParam mod ids ty)
  where
    id' = enrichIdent id
    ty' = enrichType ty
    env' = addSymbol env id (ConstVarEntry ty False Nothing (getIdentLoc id))

addSymbol :: Env -> SimpleIdent -> EnvEntry -> Env
addSymbol env name record = env { symbols = Map.insert name record (symbols env) }

addProcFun :: Env -> SimpleProcedureAndFunctionHeading -> Env
addProcFun env (ProcHead ident params) =
    addSymbol env ident (ProcFunEntry params (BasicType Void) (getIdentLoc ident))
addProcFun env (FunHead ident params retTy) =
    addSymbol env ident (ProcFunEntry params retTy (getIdentLoc ident))

-- Aggiunge una entry (variabile, costante, funzione, procedura) all'environment
addEntry :: Env -> SimpleIdent -> EnvEntry -> Env
addEntry env ident entry = env { symbols = Map.insert ident entry (symbols env) }

-- Ricreo le utility rimosse per errore
addError :: Env -> (Error () () (), Loc) -> Env
addError env (err, loc) = env { errors = errors env ++ [(errorType err, loc)] }

enterScope :: Env -> Env
enterScope parentEnv = Env { symbols = Map.empty, parent = Just parentEnv, errors = [] }

mergeEnvs :: Env -> Env -> Env
mergeEnvs env1 env2 = Env
  { symbols = Map.union (symbols env2) (symbols env1)
  , parent  = parent env1
  , errors  = errors env1 ++ errors env2
  }



----------------------------------------------------------------
-------------------- ENRICHED AST FUNCTIONS --------------------
----------------------------------------------------------------

enrichProgram :: SimpleProgram -> Program TypeInfo VarInfo FunInfo
enrichProgram (Prog ident params block loc) =
  Prog (enrichIdentUnknown ident) (enrichFormalParametersList params) (enrichBlock block) loc

enrichFormalParametersList :: [SimpleFormalParameters] -> [FormalParameters TypeInfo VarInfo FunInfo]
enrichFormalParametersList = map enrichFormalParameters

enrichFormalParameters :: SimpleFormalParameters -> FormalParameters TypeInfo VarInfo FunInfo
enrichFormalParameters (FormParam mod idents ty) =
    let
        varInfo = VarInfo Nothing False
    in FormParam mod (map (\i -> enrichIdent i ty varInfo NoFunInfo) idents) (enrichType ty)

enrichBlock :: SimpleBlock -> Block TypeInfo VarInfo FunInfo
enrichBlock (Block consts vars procs compStmt) =
    Block (map enrichConstDef consts)
    (map enrichVarDecl vars)
    (map enrichProcFunDecl procs)
    (enrichCompoundStatement compStmt)

enrichConstDef :: SimpleConstantDefinition -> ConstantDefinition TypeInfo VarInfo FunInfo
enrichConstDef (ConstDef ident c) = ConstDef (enrichIdentUnknown ident) (enrichConst c)

enrichVarDecl :: SimpleVariableDeclaration -> VariableDeclaration TypeInfo VarInfo FunInfo
enrichVarDecl (VarDecl ids ty) =
    let
        varInfo = VarInfo Nothing False
    in VarDecl (map (\i -> enrichIdent i ty varInfo NoFunInfo) ids) (enrichType ty)

enrichProcFunDecl :: SimpleProcedureAndFunctionDeclaration -> ProcedureAndFunctionDeclaration TypeInfo VarInfo FunInfo
enrichProcFunDecl (ProcFunDecl heading block) = ProcFunDecl (enrichProcFunHeading heading) (enrichBlock block)

enrichProcFunHeading :: SimpleProcedureAndFunctionHeading -> ProcedureAndFunctionHeading TypeInfo VarInfo FunInfo
enrichProcFunHeading (ProcHead ident params) =
    let
        ty = BasicType Void
        paramsEnriched = enrichFormalParametersList params
        funInfo = FunInfo ty params
    in ProcHead (enrichIdent ident (BasicType Void) NoVarInfo funInfo) (enrichFormalParametersList params)
enrichProcFunHeading (FunHead ident params retTy) =
    let
        tyEnriched = enrichType retTy
        paramsEnriched = enrichFormalParametersList params
    in FunHead (enrichIdent ident retTy NoVarInfo (FunInfo retTy params)) paramsEnriched tyEnriched

enrichCompoundStatement :: SimpleCompoundStatement -> CompoundStatement TypeInfo VarInfo FunInfo
enrichCompoundStatement (CompStmt stmts) = CompStmt (map enrichStatement stmts)

enrichStatement :: SimpleStatement -> Statement TypeInfo VarInfo FunInfo
enrichStatement = undefined  -- Da implementare se necessario

enrichIndex :: IndexType () () () -> IndexType TypeInfo VarInfo FunInfo
enrichIndex (IndexRange c1 c2) = IndexRange (enrichConst c1) (enrichConst c2)

enrichConst :: Constant () () () -> Constant TypeInfo VarInfo FunInfo
enrichConst (ConstId s i)      = ConstId s (enrichIdentUnknown i)
enrichConst (ConstInt s n)     = ConstInt s n
enrichConst (ConstDouble s d)  = ConstDouble s d
enrichConst (ConstStr s)       = ConstStr s

enrichIdent :: SimpleIdent -> SimpleType -> VarInfo -> FunInfo -> Ident TypeInfo VarInfo FunInfo
enrichIdent (Ident name loc _ _ _) ty = Ident name loc (typeInfoFromType ty)

enrichIdentUnknown :: SimpleIdent -> Ident TypeInfo VarInfo FunInfo
enrichIdentUnknown (Ident name loc _ _ _) = Ident name loc TUnknown NoVarInfo NoFunInfo

typeInfoFromType :: SimpleType -> TypeInfo
typeInfoFromType ty = case ty of
    Array indexTy arrTy -> TArray indexTy arrTy
    Pointer ptrTy       -> TPointer ptrTy
    BasicType Integer   -> TInteger
    BasicType Real      -> TReal
    BasicType Boolean   -> TBoolean
    BasicType Character -> TCharacter
    BasicType String    -> TString
    BasicType Void      -> TVoid

enrichType :: SimpleType -> Type TypeInfo VarInfo FunInfo
enrichType (Array idxs t) = Array (map enrichIndex idxs) (enrichType t)
enrichType (Pointer t)    = Pointer (enrichType t)
enrichType (BasicType b)  = BasicType b

enrichLitExpr :: Literal -> RExpr TypeInfo VarInfo FunInfo
enrichLitExpr lit = case lit of
    LiteralInteger _ -> Lit lit TInteger
    LiteralDouble _  -> Lit lit TReal
    LiteralChar _    -> Lit lit TCharacter
    LiteralString _  -> Lit lit TString
    Literal_true     -> Lit lit TBoolean
    Literal_false    -> Lit lit TBoolean

-- Extract type from RExpr
getRExprType :: RExpr TypeInfo VarInfo FunInfo -> SimpleType
getRExprType = typeFromTypeInfo . getRExprTypeInfo

-- Extract type info from RExpr
getRExprTypeInfo :: RExpr TypeInfo VarInfo FunInfo -> TypeInfo
getRExprTypeInfo (Binary _ _ _ ty)  = ty
getRExprTypeInfo (Unary _ _ ty)     = ty
getRExprTypeInfo (Lit _ ty)         = ty
getRExprTypeInfo (FCall ident _ _)  = getIdentTypeInfo ident
getRExprTypeInfo (Ref _ _ ty)       = ty
getRExprTypeInfo (LExpr lexpr ty)   = getLExprTypeInfo lexpr

getLExprType :: LExpr TypeInfo VarInfo FunInfo -> SimpleType
getLExprType = typeFromTypeInfo . getLExprTypeInfo

-- Extract type info from LExpr
getLExprTypeInfo :: LExpr TypeInfo VarInfo FunInfo -> TypeInfo
getLExprTypeInfo (LId ident _) = getIdentTypeInfo ident
getLExprTypeInfo (LArr lexpr _ _ _) = case getLExprTypeInfo lexpr of
    TArray _ arrTy -> typeInfoFromType arrTy
    _              -> TUnknown
getLExprTypeInfo (LDeref lexpr _ _) = case getLExprTypeInfo lexpr of
    TPointer ptrTy -> typeInfoFromType ptrTy
    _              -> TUnknown

getArrIndicesType :: IndexType TypeInfo VarInfo FunInfo -> SimpleIndexType
getArrIndicesType (IndexRange c1 c2) = IndexRange (toSimpleConstant c1) (toSimpleConstant c2)

-- Funzione di supporto per convertire una costante arricchita in una semplice
toSimpleConstant :: Constant ty varInfo funInfo -> Constant () () ()
toSimpleConstant (ConstId s i)      = ConstId s (toSimpleIdent i)
toSimpleConstant (ConstInt s n)     = ConstInt s n
toSimpleConstant (ConstDouble s d)  = ConstDouble s d
toSimpleConstant (ConstStr str)     = ConstStr str

-- Funzione di supporto per convertire un identificatore arricchito in uno semplice
toSimpleIdent :: Ident ty varInfo funInfo -> Ident () () ()
toSimpleIdent (Ident name loc _ _ _) = Ident name loc () () ()

-- Helper function to get TypeInfo from Ident
getIdentTypeInfo :: Ident TypeInfo VarInfo FunInfo -> TypeInfo
getIdentTypeInfo (Ident _ _ typeInfo _ _) = typeInfo

-- Helper function to convert TypeInfo to SimpleType
typeFromTypeInfo :: TypeInfo -> SimpleType
typeFromTypeInfo TInteger = BasicType Integer
typeFromTypeInfo TReal = BasicType Real
typeFromTypeInfo TBoolean = BasicType Boolean
typeFromTypeInfo TCharacter = BasicType Character
typeFromTypeInfo TString = BasicType String
typeFromTypeInfo TVoid = BasicType Void
typeFromTypeInfo TUnknown = BasicType Integer  -- Default fallback for unknown types
typeFromTypeInfo (TArray idxs t) = Array idxs t
typeFromTypeInfo (TPointer t) = Pointer t

setRExprTypeInfo :: RExpr TypeInfo VarInfo FunInfo -> TypeInfo -> RExpr TypeInfo VarInfo FunInfo
setRExprTypeInfo (Binary e1 op e2 _) ty = Binary e1 op e2 ty
setRExprTypeInfo (Unary op e _) ty = Unary op e ty
setRExprTypeInfo (Lit lit _) ty = Lit lit ty
setRExprTypeInfo (FCall ident args loc) ty = FCall (setIdentTypeInfo ident ty) args loc
setRExprTypeInfo (LExpr lexpr _) ty = LExpr (setLExprTypeInfo lexpr ty) ty

setIdentTypeInfo :: Ident TypeInfo VarInfo FunInfo -> TypeInfo -> Ident TypeInfo VarInfo FunInfo
setIdentTypeInfo (Ident name loc _ v f) ty = Ident name loc ty v f

setLExprTypeInfo :: LExpr TypeInfo VarInfo FunInfo -> TypeInfo -> LExpr TypeInfo VarInfo FunInfo
setLExprTypeInfo (LId ident identTy) ty = LId (setIdentTypeInfo ident ty) identTy
setLExprTypeInfo (LArr lexpr indices identTy loc) ty = LArr (setLExprTypeInfo lexpr ty) indices identTy loc
setLExprTypeInfo (LDeref lexpr identTy loc) ty = LDeref (setLExprTypeInfo lexpr ty) identTy loc



-----------------------------------------------------------
-------------------- SUPPORT FUNCTIONS --------------------
-----------------------------------------------------------

-- Extract the identifier name
getIdentName :: SimpleIdent -> String
getIdentName (Ident name _ _ _ _) = name

-- Extract the location of an identificatore
getIdentLoc :: SimpleIdent -> Loc
getIdentLoc (Ident _ loc _ _ _) = loc

-- Infer the type of a constant
inferConstantType :: Constant () () () -> SimpleType
inferConstantType (ConstInt _ _) = BasicType Integer
inferConstantType (ConstDouble _ _) = BasicType Real
inferConstantType (ConstStr _) = BasicType String

-- Search the name is already in the environmenta and returns it if present
lookupByName :: Env -> String -> Maybe (EnvEntry, Bool)
lookupByName env name =
    let
        -- Filter current scope
        matchingEntries = filter (\(Ident n _ _ _ _, _) -> n == name) (Map.toList (symbols env))
    in
    case matchingEntries of
        -- Found in current scope
        (_, entry):_ -> Just (entry, False)
        -- Not found, check parent
        [] ->
            case parent env of
                Nothing -> Nothing -- No parent present
                Just p ->
                    -- Recursively search in parent
                    case lookupByName p name of
                        Nothing -> Nothing -- Not found in parent chain
                        Just (entry, _) -> Just (entry, True)
    
-- Promote a numeric type to the sup
promoteNumericType :: SimpleType -> SimpleType
promoteNumericType t = case t of
    BasicType Integer -> BasicType Integer
    BasicType Real -> BasicType Real


paramsToList :: [SimpleFormalParameters] -> [SimpleFormalParameters]
paramsToList = concatMap expandParam
  where
    expandParam (FormParam mod idents ty) = [FormParam mod [ident] ty | ident <- idents]

isLExpr :: RExpr TypeInfo VarInfo FunInfo -> Bool
isLExpr (LExpr {}) = True
isLExpr _          = False

isPointer :: LExpr TypeInfo VarInfo FunInfo -> Bool
isPointer (LId _ (TPointer {}))         = True
isPointer (LArr _ _ (TPointer {}) _)    = True
isPointer (LDeref _ (TPointer {}) _)    = True
isPointer _                             = False

isArray :: LExpr TypeInfo VarInfo FunInfo -> Bool
isArray (LId _ (TArray {}))         = True
isArray (LArr _ _ (TArray {}) _)    = True
isArray (LDeref _ (TArray {}) _)    = True
isArray _                           = False


-----------------------------------------------------------
-------------------- PRINT ENVIRONMENT --------------------
-----------------------------------------------------------

printEnv :: Env -> String
printEnv env =
    let
        entries = Map.toList (symbols env)
        errs = errors env
    in "\nSymbol Table:" ++ showNames entries ++ "\nErrors:\n" ++ showErrors errs

showNames :: [(SimpleIdent, EnvEntry)] -> String
showNames [] = ""
showNames ((Ident name _ _ _ _, entry):s) = "\n\t" ++ show name ++ ": " ++ showEnvEntry entry ++ showNames s

showEnvEntry :: EnvEntry -> String
showEnvEntry entry = case entry of
    ConstVarEntry ty isConst constVal loc ->
        "Var " ++ showType ty ++ " (const: " ++ show isConst ++ ", value: " ++ show constVal ++ ") at " ++ showLoc loc
    ProcFunEntry params retTy loc -> case retTy of
        (BasicType Void) ->
            "Procedure " ++ showParams params ++ " returns: " ++ showType retTy ++ " at " ++ showLoc loc
        _ ->
            "Function " ++ showParams params ++ " returns: " ++ showType retTy ++ " at " ++ showLoc loc

showLoc :: Loc -> String
showLoc (Loc line col) = "(" ++ show line ++ ", " ++ show col ++ ")"

showParams :: [SimpleFormalParameters] -> String
showParams [] = "()"
showParams params = "(" ++ intercalate ", " (map showParam params) ++ ")"

showParam :: SimpleFormalParameters -> String
showParam (FormParam mod ids ty) =
    show mod ++ " " ++ intercalate ", " (map (show . getIdentName) ids) ++ ": " ++ showType ty

showType :: SimpleType -> String
showType ty = case ty of
    Array _ arrTy       -> "Array of " ++ showType arrTy
    Pointer ptrTy       -> "Pointer to " ++ showType ptrTy
    BasicType Integer   -> "Integer"
    BasicType Real      -> "Real"
    BasicType Boolean   -> "Boolean"
    BasicType Character -> "Character"
    BasicType String    -> "String"
    BasicType Void      -> "Void"

showErrors :: [(String, Loc)] -> String
showErrors [] = "No errors"
showErrors errs = intercalate "\n" (map showError errs)
    where showError (msg, loc) = showLoc loc ++ ": " ++ msg


-- TODO: MUTUAL RECURSION (TWO-PASS ANALYSIS)
{-
-- First pass: collect all function declarations (headers only)
collectFunctionDeclarations :: Env -> [SimpleProcedureAndFunctionDeclaration] -> Env
collectFunctionDeclarations env [] = env
collectFunctionDeclarations env (decl:decls) =
    let
        env' = collectFunctionDeclaration env decl
    in collectFunctionDeclarations env' decls

collectFunctionDeclaration :: Env -> SimpleProcedureAndFunctionDeclaration -> Env
collectFunctionDeclaration env (ProcFunDecl heading _) =
    let
        -- Add function to environment without analyzing the body
        (env', _) = checkProcFunHeadOnly env heading
    in env'

-- Check only the function header and add it to environment (for forward declarations)
checkProcFunHeadOnly :: Env -> SimpleProcedureAndFunctionHeading -> (Env, ProcedureAndFunctionHeading TypeInfo VarInfo FunInfo)
checkProcFunHeadOnly env head = case head of
    (ProcHead ident params) ->
        let
            preEnrichedParams = enrichFormalParametersList params
            (env', identEnriched) = checkProcFunIdent env ident (BasicType Void) NoVarInfo (FunInfo (enrichType (BasicType Void)) preEnrichedParams) params
        in (env', ProcHead identEnriched (enrichFormalParametersList params))
    (FunHead ident params retTy) ->
        let
            preEnrichedParams = enrichFormalParametersList params
            retTyEnriched = enrichType retTy
            (env', identEnriched) = checkProcFunIdent env ident retTy NoVarInfo (FunInfo retTyEnriched preEnrichedParams) params
        in (env', FunHead identEnriched (enrichFormalParametersList params) retTyEnriched)
-}