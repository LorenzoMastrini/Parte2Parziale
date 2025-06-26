module StaticAnalysis where

import AbsLinguaggio
import ErrorStaticAnalysis hiding (showTy)
import Data.Map (Map, toList)
import qualified Data.Map as Map
import Data.List (intercalate, nub)
import Data.Maybe (isJust, isNothing)



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
checkProgram :: SimpleProgram -> (Env, EnrichedProgram)
checkProgram (Prog ident params block loc) =
    let
        (env1, paramsEnriched) = checkFormalParametersList defaultEnv params
        (env2, blockEnriched) = checkBlock env1 block Nothing
    in (env2, Prog (enrichIdentUnknown ident) paramsEnriched blockEnriched loc)

checkFormalParametersList :: Env -> [SimpleFormalParameters] -> (Env, [EnrichedFormalParameters])
checkFormalParametersList env [] = (env, [])
checkFormalParametersList env (params:paramsList) =
    let
        (env1, paramsEnriched) = checkFormalParameters env params
        (env2, paramsListEnriched) = checkFormalParametersList env1 paramsList
    in (env2, paramsEnriched:paramsListEnriched)

checkFormalParameters :: Env -> SimpleFormalParameters -> (Env, EnrichedFormalParameters)
checkFormalParameters env (FormParam mod idents ty) =
    let
        tyEnriched = enrichType ty
        (env1, identsEnriched) = checkIdentsList env idents ty (VarInfo Nothing False) NoFunInfo
    in (env1, FormParam mod identsEnriched tyEnriched)

checkBlock :: Env -> SimpleBlock -> Maybe SimpleType -> (Env, EnrichedBlock)
checkBlock env (Block constDefList varDeclList procFunDeclList compStmt) typeIfInsideFun =
    let
        -- Don't create new scope if already in a function scope
        localEnv = case parent env of
            Just _ -> env  -- Already in a function, use the same environment
            Nothing -> env  -- Program uses the default environment
        
        -- TODO: First pass (for mutual recursion): collect all function declarations
        --envWithFuncs = collectFunctionDeclarations localEnv procFunDeclList
        -- Second pass: analyze constants, variables, and function bodies
        (env1, constDefListEnriched) = checkConstDefList localEnv constDefList
        (env2, varDeclListEnriched) = checkVarDeclList env1 varDeclList
        (env3, procFunDeclListEnriched) = checkProcFunDeclList env2 procFunDeclList
        (env4, compStmtEnriched) = checkCompoundStatement env3 compStmt False typeIfInsideFun
        mergedEnv = mergeEnvs env env4
    in (mergedEnv, Block constDefListEnriched varDeclListEnriched procFunDeclListEnriched compStmtEnriched)



------------------------------------------------------
-------------------- DECLARATIONS --------------------
------------------------------------------------------

-- Iterate the list of constant definitions
checkConstDefList :: Env -> [SimpleConstantDefinition] -> (Env, [EnrichedConstantDefinition])
checkConstDefList env [] = (env, [])
checkConstDefList env (d:ds) =
    let
        (env1, dEnr) = checkConstDef env d
        (env2, dsEnr) = checkConstDefList env1 ds
    in (env2, dEnr:dsEnr)

-- Check if the constant identifier is already used inside the environment
checkConstDef :: Env -> SimpleConstantDefinition -> (Env, EnrichedConstantDefinition)
checkConstDef env def@(ConstDef ident constVal) =
    let
        ty = inferConstantType constVal
        (env1, identEnriched) = checkIdent env ident ty (VarInfo (Just constVal) True) NoFunInfo
    in if ty /= BasicType Void -- The value is not constant
        then (env1, ConstDef identEnriched (enrichConst constVal))
        else case constVal of -- As the syntax is defined, it must be an identifier
            (ConstId _ (Ident name loc _ _ _)) -> (addError env1 (NotConstantValue name, loc), ConstDef identEnriched (enrichConst constVal))

-- Iterate the list of variable declarations
checkVarDeclList :: Env -> [SimpleVariableDeclaration] -> (Env, [EnrichedVariableDeclaration])
checkVarDeclList env [] = (env, [])
checkVarDeclList env (d:ds) =
    let
        (env1, dEnr) = checkVarDecl env d
        (env2, dsEnr) = checkVarDeclList env1 ds
    in (env2, dEnr:dsEnr)

checkVarDecl :: Env -> SimpleVariableDeclaration -> (Env, EnrichedVariableDeclaration)
checkVarDecl env (VarDecl ids ty) = checkVarDeclSequence env ids ty

-- Iterate the sequence of variable declarations that has the same type (ex. a, b, c: Integer)
checkVarDeclSequence :: Env -> [SimpleIdent] -> SimpleType -> (Env, EnrichedVariableDeclaration)
checkVarDeclSequence env [] ty = (env, VarDecl [] (enrichType ty))
checkVarDeclSequence env (id:ids) ty =
    let
        -- Check for negative indices if the type is an array, then check identifier for redeclarations
        (envWithId, idEnr) = case ty of
            Array indexTypes _ ->
                let
                    loc = getIdentLoc id
                    (env1, hasNegativeIndex) = checkNegativeIndex env indexTypes loc -- Check for negative indices
                    (env2, baseIdEnr) = checkIdent env1 id ty (VarInfo Nothing False) NoFunInfo
                    finalIdEnr = if hasNegativeIndex
                        then setIdentTypeInfo baseIdEnr Nothing -- If (at least) one index is negative, set identifier's type as Nothing
                        else baseIdEnr
                in (env2, finalIdEnr)
            _ -> checkIdent env id ty (VarInfo Nothing False) NoFunInfo -- Not an array, check just the identifier
        -- Recursive call
        (finalEnv, VarDecl idsEnr tyEnr) = checkVarDeclSequence envWithId ids ty
    in
        (finalEnv, VarDecl (idEnr : idsEnr) tyEnr) -- Final result

-- Iterate the list of procedures and functions declarations
checkProcFunDeclList :: Env -> [SimpleProcedureAndFunctionDeclaration] -> (Env, [EnrichedProcedureAndFunctionDeclaration])
checkProcFunDeclList env [] = (env, [])
checkProcFunDeclList env (d:ds) =
    let (env1, dEnr) = checkProcFunDecl env d
        (env2, dsEnr) = checkProcFunDeclList env1 ds
    in (env2, dEnr:dsEnr)

checkProcFunDecl :: Env -> SimpleProcedureAndFunctionDeclaration -> (Env, EnrichedProcedureAndFunctionDeclaration)
checkProcFunDecl env (ProcFunDecl heading block) =
    let
        -- Create a scope for the function
        localEnv = enterScope env
        (localEnv1, headingEnriched) = checkProcFunHead localEnv heading
        (finalLocalEnv, blockEnriched) = checkBlock localEnv1 block (getFunHeadType headingEnriched)
        envHasReturn = hasReturnStmt finalLocalEnv headingEnriched blockEnriched
        Just outerEnv = parent envHasReturn -- Get the updated outer environment
        finalOuterEnv = outerEnv { errors = errors outerEnv ++ errors envHasReturn } -- Propagate the errors to the outer environment
    in (finalOuterEnv, ProcFunDecl headingEnriched blockEnriched)


checkProcFunHead :: Env -> SimpleProcedureAndFunctionHeading -> (Env, EnrichedProcedureAndFunctionHeading)
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
    (FunHead ident params retTy loc) ->
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
        in (updatedLocalEnv, FunHead identEnriched paramsEnriched retTyEnriched loc)

-- Given a list of identifiers, check recursively if each of them is already used inside the environment
checkIdentsList :: Env -> [SimpleIdent] -> SimpleType -> VarInfo -> FunInfo -> (Env, [EnrichedIdent])
checkIdentsList env [] _ _ _ = (env, [])
checkIdentsList env (i:is) ty varInfo funInfo =
    let (env', iEnr) = checkIdent env i ty varInfo funInfo
        (envFinal, isEnr) = checkIdentsList env' is ty varInfo funInfo
    in (envFinal, iEnr : isEnr)

-- Check if the identifier is already used inside the environment. If not, add it to the environment
checkIdent :: Env -> SimpleIdent -> SimpleType -> VarInfo -> FunInfo -> (Env, EnrichedIdent)
checkIdent env ident@(Ident name loc _ _ _) ty varInfo@(VarInfo constVal isConst) funInfo =
    let res = lookupByName env name
    in case res of
        Just (ConstVarEntry _ entryIsConst _ _, False) ->
            (addError env (RedeclaredConstVar ident entryIsConst, loc), enrichIdentUnknown ident)
        Just (ProcFunEntry _ _ existingFunLoc, False) ->
            (addError env (RedeclaredProcFun ident existingFunLoc, loc), enrichIdentUnknown ident)
        _ ->
            (addEntry env ident (ConstVarEntry ty isConst constVal loc), enrichIdent ident (Just ty) varInfo funInfo)

-- Update the environment with the declaration of a procedure/function and return its enriched identifier
checkProcFunIdent :: Env -> SimpleIdent -> SimpleType -> VarInfo -> FunInfo -> [SimpleFormalParameters] -> (Env, EnrichedIdent)
checkProcFunIdent env ident ty NoVarInfo (FunInfo retTy paramsEnriched) simpleParams =
    let res = lookupByName env (getIdentName ident)
    in case res of
        Just (ConstVarEntry _ isConst _ _, _) ->
            (addError env (RedeclaredConstVar ident isConst, getIdentLoc ident), enrichIdentUnknown ident)
        Just (ProcFunEntry _ _ existingFunLoc, _) ->
            (addError env (RedeclaredProcFun ident existingFunLoc, getIdentLoc ident), enrichIdentUnknown ident)
        Nothing ->
            if retTy == BasicType Void
                then (addEntry env ident (ProcFunEntry simpleParams (BasicType Void) (getIdentLoc ident)), enrichIdent ident (Just ty) NoVarInfo (FunInfo retTy paramsEnriched))
                else (addEntry env ident (ProcFunEntry simpleParams ty (getIdentLoc ident)), enrichIdent ident (Just ty) NoVarInfo (FunInfo retTy paramsEnriched))
checkProcFunIdent env ident ty (VarInfo _ isConst) funInfo _ =
    let res = lookupByName env (getIdentName ident)
    in case res of
        Just (ConstVarEntry _ entryIsConst _ _, _) ->
            (addError env (RedeclaredConstVar ident entryIsConst, getIdentLoc ident), enrichIdentUnknown ident)
        Just (ProcFunEntry _ _ existingFunLoc, _) ->
            (addError env (RedeclaredProcFun ident existingFunLoc, getIdentLoc ident), enrichIdentUnknown ident)
        Nothing -> (addEntry env ident (ConstVarEntry ty isConst Nothing (getIdentLoc ident)), enrichIdent ident (Just ty) (VarInfo Nothing isConst) funInfo)


-----------------------------------------------------
-------------------- EXPRESSIONS --------------------
-----------------------------------------------------

checkRExpr :: Env -> SimpleRExpr -> (Env, EnrichedRExpr)
checkRExpr env expr = case expr of
    Binary e1 op e2 _ ->
        let
            (env1, e1Enriched) = checkRExpr env e1
            (env2, e2Enriched) = checkRExpr env1 e2
            t1 = getRExprType e1Enriched
            t2 = getRExprType e2Enriched
            (env3, ty) = checkBinaryOp env2 op t1 t2
        in (env3, Binary e1Enriched op e2Enriched ty)
    Unary op e _ ->
        let
            (env1, eEnriched) = checkRExpr env e
            t = getRExprType eEnriched
            (env3, ty) = checkUnaryOp env1 op t
        in (env3, Unary op eEnriched ty)
    Lit lit _ -> (env, enrichLitExpr lit)
    FCall ident@(Ident name _ _ _ _) args loc -> case lookupByName env name of
        Nothing ->
            (addError env (FunNotDefined ident, loc), FCall (enrichIdentUnknown ident) [] loc) -- Function doesn't exist
        Just (ConstVarEntry {}, _) ->
            (addError env (NotAFunction ident, loc), FCall (enrichIdentUnknown ident) [] loc) -- Not a function
        Just (ProcFunEntry params retTy _, _) ->
            let
                (env1, argsEnriched, hasTypeError) = checkArgsNumWithTypeError env args (paramsToList params) (length args) (length (paramsToList params)) loc
                identEnriched = if hasTypeError
                    then setIdentTypeInfo (enrichIdent ident (Just retTy) NoVarInfo (FunInfo retTy params)) Nothing
                    else enrichIdent ident (Just retTy) NoVarInfo (FunInfo retTy params)
            in (env1, FCall identEnriched argsEnriched loc)
    Ref op@(UnaryRef loc) rexpr _ ->
        let
            (env1, rexprEnriched) = checkRExpr env rexpr
            isLExprResult = isLExpr rexprEnriched
        in if isLExprResult
            then let
                mTy = getRExprType rexprEnriched
                refType = case mTy of
                            Just t -> Just (Pointer t)
                            Nothing -> Nothing
                in (env1, Ref op rexprEnriched refType)
            else (addError env1 (NotLExpr op, loc), Ref op rexprEnriched Nothing)
    LExpr lexpr _ ->
        let
            (env1, lexprEnriched) = checkLExpr env lexpr
            ty = getLExprType lexprEnriched
        in (env1, LExpr lexprEnriched ty)
    _ -> error $ "DEBUG: checkStatement pattern non gestito: " ++ show expr

checkArgsNumWithTypeError :: Env -> [SimpleRExpr] -> [SimpleFormalParameters] -> Int -> Int -> Loc -> (Env, [EnrichedRExpr], Bool)
checkArgsNumWithTypeError env [] [] origArgs origParams _ = (env, [], False)
checkArgsNumWithTypeError env [] params origArgs origParams loc =
    let env1 = addError env (FunArgsNumMismatch origArgs origParams, loc)
    in (env1, [], True)
checkArgsNumWithTypeError env args [] origArgs origParams loc =
    let env1 = addError env (FunArgsNumMismatch origArgs origParams, loc)
    in (env1, [], True)
checkArgsNumWithTypeError env (a:as) (p:ps) origArgs origParams loc =
    let
        (env1, argEnriched) = checkRExpr env a
        (env2, compatible) = checkArgType env1 argEnriched p loc
        (env3, argsEnriched, hasTypeErrorRest) = checkArgsNumWithTypeError env2 as ps origArgs origParams loc
        hasTypeError = not compatible || hasTypeErrorRest
    in (env3, argEnriched : argsEnriched, hasTypeError)

checkArgType :: Env -> EnrichedRExpr -> SimpleFormalParameters -> Loc -> (Env, Bool)
checkArgType env argEnriched (FormParam _ _ paramType) loc =
    let
        argType = getRExprType argEnriched
        (env1, compatible) = argCompatible env argType paramType loc
    in (env1, compatible)

checkLExpr :: Env -> SimpleLExpr -> (Env, EnrichedLExpr)
checkLExpr env lexpr = case lexpr of
    LId ident _ ->
        let
            (env1, identEnriched) = identExists env ident
            ty = getIdentTypeInfo identEnriched
        in (env1, LId identEnriched ty)
    LArr lexpr' indices _ loc ->
        let
            (env1, lexprEnriched) = checkLExpr env lexpr'
            (env2, indicesEnriched) = checkIndices env1 indices
            isArrayResult = isArray lexprEnriched
            childHasUnknown = isNothing (getLExprType lexprEnriched) || any (isNothing . getRExprType) indicesEnriched
        in if isArrayResult && not childHasUnknown
            then
                let
                    Just (Array i ty) = getLExprType lexprEnriched
                    (env3, hasError) = checkRange env2 lexprEnriched indicesEnriched loc
                    finalType = if hasError then Nothing else Just ty
                in (env3, LArr lexprEnriched indicesEnriched finalType loc)
            else (addError env2 (NotAnArray, loc), LArr lexprEnriched indicesEnriched Nothing loc)
    LDeref lexpr' _ loc ->
        let
            (env1, lexprEnriched) = checkLExpr env lexpr'
            isPointerResult = isPointer lexprEnriched
        in if isPointerResult
            then let
                Just (Pointer t) = getLExprType lexprEnriched
                in (env1, LDeref lexprEnriched (Just t) loc)
            else (addError env1 (NotAPointer, loc), LDeref lexprEnriched Nothing loc)

checkIndices :: Env -> [SimpleRExpr] -> (Env, [EnrichedRExpr])
checkIndices env [] = (env, [])
checkIndices env (index:indices) =
    let
        (env1, indexEnriched) = checkRExpr env index
        (env2, indicesEnriched) = checkIndices env1 indices
    in (env2, indexEnriched:indicesEnriched)

checkRange :: Env -> EnrichedLExpr -> [EnrichedRExpr] -> Loc -> (Env, Bool)
checkRange env lexpr indices loc =
    case getLExprType lexpr of
        Just (Array indTy _) ->
            let
                (min, max) = getArrRange env indTy
                values = getValFromIndices indices
                allKnown = all isJust values -- If all indices are known, check the bounds of the array
                outOfBounds = [v | Just v <- values, v < min || v > max]
            in if allKnown
               then if null outOfBounds
                    then (env, False)
                    else (addError env (OutOfBound outOfBounds loc, loc), True)
               else (env, False) -- Indices not known: no static error
        _ -> (addError env (NotAnArray, loc), False)


        
----------------------------------------------------
-------------------- STATEMENTS --------------------
----------------------------------------------------

checkCompoundStatement :: Env -> SimpleCompoundStatement -> Bool -> Maybe SimpleType -> (Env, EnrichedCompoundStatement)
checkCompoundStatement env (CompStmt stmts) isLoop  typeIfInsideFun =
    let
        (env1, stmtsEnriched) = checkStatementList env stmts isLoop typeIfInsideFun
    in (env1, CompStmt stmtsEnriched)

checkStatementList :: Env -> [SimpleStatement] -> Bool -> Maybe SimpleType -> (Env, [EnrichedStatement])
checkStatementList env [] isLoop typeIfInsideFun = (env, [])
checkStatementList env (stmt:stmts) isLoop typeIfInsideFun =
    let
        (env1, stmtEnriched) = checkStatement env stmt isLoop typeIfInsideFun
        (env2, stmtsEnriched) = checkStatementList env1 stmts isLoop typeIfInsideFun
    in (env2, stmtEnriched:stmtsEnriched)

checkStatement :: Env -> SimpleStatement -> Bool -> Maybe SimpleType -> (Env, EnrichedStatement)
checkStatement env stmt isLoop typeIfInsideFun = case stmt of
    Stmts compStmt ->
        let
            (env1, compStmtEnriched) = checkCompoundStatement env compStmt isLoop typeIfInsideFun
        in (env1, Stmts compStmtEnriched)
    RExprStmt expr _ ->
        let
            (env1, exprEnriched) = checkRExpr env expr
        in (env1, RExprStmt exprEnriched (getRExprType exprEnriched))
    Assign lexpr rexpr _ loc ->
        let
            (env1, lexprEnriched) = checkLExpr env lexpr
            (env2, rexprEnriched) = checkRExpr env1 rexpr
        in if isConstant lexprEnriched
            then (addError env2 (AssignmentToConstant, loc), Assign lexprEnriched rexprEnriched Nothing loc) -- If the checks didn't pass as defined in the grammar, it must be a constant
            else let
                (env3, assTy) = compatibleAssignment env2 (getLExprType lexprEnriched) (getRExprType rexprEnriched) loc
                in (env3, Assign lexprEnriched rexprEnriched assTy loc)
    If cond thenStmt optElse loc ->
        let
            (env1, condEnriched) = checkRExpr env cond
            (env2, thenEnriched) = checkStatement env1 thenStmt isLoop typeIfInsideFun
            (env3, optElseEnriched) = checkOptElse env2 optElse isLoop typeIfInsideFun
            envFinal = if getRExprType condEnriched == Just (BasicType Boolean)
                then env3
                else addError env3 (NotBooleanCondition "if" (getRExprType condEnriched), loc)
        in (envFinal, If condEnriched thenEnriched optElseEnriched loc)
    While rexpr stmt loc ->
        let
            (env1, rexprEnriched) = checkRExpr env rexpr
            (env2, stmtEnriched) = checkStatement env1 stmt True typeIfInsideFun
        in if isBoolean rexprEnriched
            then (env2, While rexprEnriched stmtEnriched loc)
            else (addError env2 (NotBooleanCondition "while" (getRExprType rexprEnriched), loc), While rexprEnriched stmtEnriched loc)
    DoWhile stmt rexpr loc ->
        let
            (env1, rexprEnriched) = checkRExpr env rexpr
            (env2, stmtEnriched) = checkStatement env1 stmt True typeIfInsideFun
        in if isBoolean rexprEnriched
            then (env2, DoWhile stmtEnriched rexprEnriched loc)
            else (addError env2 (NotBooleanCondition "repeat-until" (getRExprType rexprEnriched), loc), DoWhile stmtEnriched rexprEnriched loc)
    ForTo lexpr rexpr1 rexpr2 stmt ->
        let
            (env1, lexprEnriched) = checkLExpr env lexpr
            (env2, rexpr1Enriched) = checkRExpr env1 rexpr1
            (env3, rexpr2Enriched) = checkRExpr env2 rexpr2
            (env4, stmtEnriched) = checkStatement env stmt True typeIfInsideFun
        in (env4, ForTo lexprEnriched rexpr1Enriched rexpr2Enriched stmtEnriched)
    ReturnStmt rexpr _ loc ->
        let (env1, rexprEnriched) = checkRExpr env rexpr in
        case typeIfInsideFun of
            Nothing ->
                (addError env1 (NotInFunction, loc), ReturnStmt rexprEnriched Nothing loc)
            Just (BasicType Void) ->
                (addError env1 (ReturnInsideProcedure, loc), ReturnStmt rexprEnriched Nothing loc)
            Just _ ->
                if getRExprType rexprEnriched == typeIfInsideFun
                    then (env1, ReturnStmt rexprEnriched typeIfInsideFun loc)
                    else (addError env1 (ReturnTypeMismatch (getRExprType rexprEnriched) typeIfInsideFun, loc), ReturnStmt rexprEnriched typeIfInsideFun loc)
    BreakStmt loc -> if isLoop
        then (env, BreakStmt loc)
        else (addError env (NotInLoop "break", loc), BreakStmt loc)
    ContinueStmt loc -> if isLoop
        then (env, ContinueStmt loc)
        else (addError env (NotInLoop "continue", loc), ContinueStmt loc)

-- Check the type compatibility in an assignment
compatibleAssignment :: Env -> Maybe SimpleType -> Maybe SimpleType -> Loc -> (Env, Maybe SimpleType)
compatibleAssignment env (Just targetType) (Just sourceType) loc = case (targetType, sourceType) of
    (t1, t2) | t1 == t2 -> (env, Just targetType) -- Same type: compatible
    -- Numeric cast
    (BasicType Real, BasicType Integer) -> (env, Just targetType) -- int -> real
    (BasicType Integer, BasicType Real) -> (env, Just targetType) -- real -> int (loss of precision)
    -- Cast between characters and strings
    (BasicType String, BasicType Character) -> (env, Just targetType) -- char -> string
    -- Cast between arrays and pointers
    (Array _ elemType, Pointer ptrType) -> if elemType == ptrType
        then (env, Just targetType)
        else (addError env (IncompatibleArrPtrType, loc), Nothing)
    (Pointer ptrType, Array _ elemType) -> if ptrType == elemType
        then (env, Just targetType)
        else (addError env (IncompatiblePtrArrType, loc), Nothing)
    _ -> (addError env (IncompatibleAssignment targetType sourceType, loc), Nothing) -- If it's not any of the above, the LExpr is actually a constant
compatibleAssignment env _ _ loc = (env, Nothing)

checkOptElse :: Env -> SimpleOptElse -> Bool -> Maybe SimpleType -> (Env, EnrichedOptElse)
checkOptElse env optElse isLoop typeIfInsideFun = case optElse of
    NoElse -> (env, NoElse)
    Else stmt ->
        let (env1, stmtEnriched) = checkStatement env stmt isLoop typeIfInsideFun
        in (env1, Else stmtEnriched)



------------------------------------------------
-------------------- CHECKS --------------------
------------------------------------------------

identExists :: Env -> SimpleIdent -> (Env, EnrichedIdent)
identExists env ident@(Ident name loc _ varInfo funInfo) = case lookupByName env name of
    Nothing ->
        (addError env (UnknownIdentifier ident, loc), enrichIdentUnknown ident) -- Identifier doesn't exist
    Just (ConstVarEntry ty isConst constVal entryLoc, _) ->
        (env, enrichIdent ident (Just ty) (VarInfo constVal isConst) NoFunInfo)
    Just (ProcFunEntry params retTy loc, _) ->
        (env, enrichIdent ident (Just retTy) NoVarInfo (FunInfo retTy params))

-------------------- Binary --------------------
checkBinaryOp :: Env -> Binary -> Maybe SimpleType -> Maybe SimpleType -> (Env, Maybe SimpleType)
checkBinaryOp env op (Just t1) (Just t2) = case op of
    BinaryPlus loc      -> checkNumericOp env "+" t1 t2 loc
    BinaryMinus loc     -> checkNumericOp env "-" t1 t2 loc
    BinaryMul loc       -> checkNumericOp env "*" t1 t2 loc
    BinaryDiv loc       -> checkNumericOp env "/" t1 t2 loc
    BinaryIntDiv loc    -> checkIntegerOp env "div" t1 t2 loc
    BinaryMod loc       -> checkIntegerOp env "mod" t1 t2 loc
    BinaryOr loc        -> checkBooleanOp env "or" t1 t2 loc
    BinaryAnd loc       -> checkBooleanOp env "and" t1 t2 loc
    BinaryEq loc        -> checkComparisonOp env "=" t1 t2 loc
    BinaryNeq loc       -> checkComparisonOp env "<>" t1 t2 loc
    BinaryLt loc        -> checkComparisonOp env "<" t1 t2 loc
    BinaryLte loc       -> checkComparisonOp env "<=" t1 t2 loc
    BinaryGt loc        -> checkComparisonOp env ">" t1 t2 loc
    BinaryGte loc       -> checkComparisonOp env ">=" t1 t2 loc
checkBinaryOp env _ _ _ = (env, Nothing)

checkNumericOp :: Env -> String -> SimpleType -> SimpleType -> Loc -> (Env, Maybe SimpleType)
checkNumericOp env op t1 t2 loc = case (t1, t2) of
    (incomp1@(BasicType Boolean), incomp2)      -> (addError env (IncompatibleTypes op incomp1 incomp2, loc), Nothing)
    (incomp1@(BasicType Character), incomp2)    -> (addError env (IncompatibleTypes op incomp1 incomp2, loc), Nothing)
    (incomp1@(BasicType String), incomp2)       -> (addError env (IncompatibleTypes op incomp1 incomp2, loc), Nothing)
    (incomp1@(BasicType Void), incomp2)         -> (addError env (IncompatibleTypes op incomp1 incomp2, loc), Nothing)
    (incomp1, incomp2@(BasicType Boolean))      -> (addError env (IncompatibleTypes op incomp1 incomp2, loc), Nothing)
    (incomp1, incomp2@(BasicType Character))    -> (addError env (IncompatibleTypes op incomp1 incomp2, loc), Nothing)
    (incomp1, incomp2@(BasicType String))       -> (addError env (IncompatibleTypes op incomp1 incomp2, loc), Nothing)
    (incomp1, incomp2@(BasicType Void))         -> (addError env (IncompatibleTypes op incomp1 incomp2, loc), Nothing)
    _ ->
        let
            t1' = promoteNumericType t1
            t2' = promoteNumericType t2
        in if t1' == BasicType Real || t2' == BasicType Real
            then (env, Just (BasicType Real))
            else (env, Just (BasicType Integer))

checkIntegerOp :: Env -> String -> SimpleType -> SimpleType -> Loc -> (Env, Maybe SimpleType)
checkIntegerOp env op t1 t2 loc = let
    (env1, mTy1) = checkIntegerType env op t1 loc
    (env2, mTy2) = checkIntegerType env1 op t2 loc
    in if isJust mTy1 && isJust mTy2
        then (env2, Just (BasicType Integer))
        else (env2, Nothing)

checkBooleanOp :: Env -> String -> SimpleType -> SimpleType -> Loc -> (Env, Maybe SimpleType)
checkBooleanOp env op t1 t2 loc =
    let
        (env1, mTy1) = checkBooleanType env op t1 loc
        (env2, mTy2) = checkBooleanType env1 op t2 loc
    in if isJust mTy1 && isJust mTy2
        then (env2, Just (BasicType Boolean))
        else (env2, Nothing)

checkComparisonOp :: Env -> String -> SimpleType -> SimpleType -> Loc -> (Env, Maybe SimpleType)
checkComparisonOp env "<" t1@(BasicType Boolean) t2@(BasicType Boolean) loc = (addError env (NotBooleanOperation "<" t1 t2, loc), Nothing)
checkComparisonOp env "<=" t1@(BasicType Boolean) t2@(BasicType Boolean) loc = (addError env (NotBooleanOperation "<=" t1 t2, loc), Nothing)
checkComparisonOp env ">" t1@(BasicType Boolean) t2@(BasicType Boolean) loc = (addError env (NotBooleanOperation ">" t1 t2, loc), Nothing)
checkComparisonOp env ">=" t1@(BasicType Boolean) t2@(BasicType Boolean) loc = (addError env (NotBooleanOperation ">=" t1 t2, loc), Nothing)
checkComparisonOp env op t1 t2 loc = if t1 == t2
    then (env, Just (BasicType Boolean))
    else case (t1, t2) of
        (BasicType Integer, BasicType Real)     -> (env, Just (BasicType Boolean)) -- implicit cast of t1
        (BasicType Real, BasicType Integer)     -> (env, Just (BasicType Boolean)) -- implicit cast of t2
        (BasicType Character, BasicType String) -> (env, Just (BasicType Boolean)) -- implicit cast of t1
        (BasicType String, BasicType Character) -> (env, Just (BasicType Boolean)) -- implicit cast of t2
        _ -> (addError env (ComparisonTypeMismatch op t1 t2, loc), Nothing)

-------------------- Unary --------------------
checkUnaryOp :: Env -> Unary -> Maybe SimpleType -> (Env, Maybe SimpleType)
checkUnaryOp env op (Just t) = case op of
    UnaryPlus loc   -> checkNumericType env "+" t loc
    UnaryMinus loc  -> checkNumericType env "-" t loc
    UnaryNot loc    -> checkBooleanType env "not" t loc
checkUnaryOp env _ Nothing = (env, Nothing)

checkIntegerType :: Env -> String -> SimpleType -> Loc -> (Env, Maybe SimpleType)
checkIntegerType env op t loc = case t of
    BasicType Integer -> (env, Just (BasicType Integer))
    _ -> (addError env (NotIntegerType op t, loc), Nothing)

checkBooleanType :: Env -> String -> SimpleType -> Loc -> (Env, Maybe SimpleType)
checkBooleanType env op t loc = case t of
    BasicType Boolean -> (env, Just (BasicType Boolean))
    -- BasicType Integer -> (env, Just (BasicType Boolean)) -- 0 = false, non-zero = true
    -- BasicType Real    -> (env, Just (BasicType Boolean)) -- 0.0 = false, non-zero = true
    _ -> (addError env (NotBooleanType op t, loc), Nothing)

checkNumericType :: Env -> String -> SimpleType -> Loc -> (Env, Maybe SimpleType)
checkNumericType env op t loc = case t of
    BasicType Integer -> (env, Just (BasicType Integer))
    BasicType Real -> (env, Just (BasicType Real))
    _ -> (addError env (NotNumericType op t, loc), Nothing)

-------------------- FCall --------------------
argCompatible :: Env -> Maybe SimpleType -> SimpleType -> Loc -> (Env, Bool)
argCompatible env (Just argType) paramType loc = if argType /= paramType
    then (addError env (FunArgTypeMismatch argType paramType, loc), False)
    else (env, True)
argCompatible env Nothing _ loc = (env, False)


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
  where env' = addSymbol env id (ConstVarEntry ty False Nothing (getIdentLoc id))

addSymbol :: Env -> SimpleIdent -> EnvEntry -> Env
addSymbol env name record = env { symbols = Map.insert name record (symbols env) }

addProcFun :: Env -> SimpleProcedureAndFunctionHeading -> Env
addProcFun env (ProcHead ident params) =
    addSymbol env ident (ProcFunEntry params (BasicType Void) (getIdentLoc ident))
addProcFun env (FunHead ident params retTy _) =
    addSymbol env ident (ProcFunEntry params retTy (getIdentLoc ident))

-- Add an entry (variable, constant, function or procedure) to the environment
addEntry :: Env -> SimpleIdent -> EnvEntry -> Env
addEntry env ident entry = env { symbols = Map.insert ident entry (symbols env) }

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

enrichProgram :: SimpleProgram -> EnrichedProgram
enrichProgram (Prog ident params block loc) =
  Prog (enrichIdentUnknown ident) (enrichFormalParametersList params) (enrichBlock block) loc

enrichFormalParametersList :: [SimpleFormalParameters] -> [EnrichedFormalParameters]
enrichFormalParametersList = map enrichFormalParameters

enrichFormalParameters :: SimpleFormalParameters -> EnrichedFormalParameters
enrichFormalParameters (FormParam mod idents ty) =
    let
        varInfo = VarInfo Nothing False
    in FormParam mod (map (\i -> enrichIdent i (Just ty) varInfo NoFunInfo) idents) (enrichType ty)

enrichBlock :: SimpleBlock -> EnrichedBlock
enrichBlock (Block consts vars procs compStmt) =
    Block (map enrichConstDef consts)
    (map enrichVarDecl vars)
    (map enrichProcFunDecl procs)
    (enrichCompoundStatement compStmt)

enrichConstDef :: SimpleConstantDefinition -> EnrichedConstantDefinition
enrichConstDef (ConstDef ident c) = ConstDef (enrichIdentUnknown ident) (enrichConst c)

enrichVarDecl :: SimpleVariableDeclaration -> EnrichedVariableDeclaration
enrichVarDecl (VarDecl ids ty) =
    let
        varInfo = VarInfo Nothing False
    in VarDecl (map (\i -> enrichIdent i (Just ty) varInfo NoFunInfo) ids) (enrichType ty)

enrichProcFunDecl :: SimpleProcedureAndFunctionDeclaration -> EnrichedProcedureAndFunctionDeclaration
enrichProcFunDecl (ProcFunDecl heading block) = ProcFunDecl (enrichProcFunHeading heading) (enrichBlock block)

enrichProcFunHeading :: SimpleProcedureAndFunctionHeading -> EnrichedProcedureAndFunctionHeading
enrichProcFunHeading (ProcHead ident params) =
    let
        ty = BasicType Void
        paramsEnriched = enrichFormalParametersList params
        funInfo = FunInfo ty params
    in ProcHead (enrichIdent ident (Just (BasicType Void)) NoVarInfo funInfo) (enrichFormalParametersList params)
enrichProcFunHeading (FunHead ident params retTy loc) =
    let
        tyEnriched = enrichType retTy
        paramsEnriched = enrichFormalParametersList params
    in FunHead (enrichIdent ident (Just retTy) NoVarInfo (FunInfo retTy params)) paramsEnriched tyEnriched loc

enrichCompoundStatement :: SimpleCompoundStatement -> EnrichedCompoundStatement
enrichCompoundStatement (CompStmt stmts) = CompStmt (map enrichStatement stmts)

enrichStatement :: SimpleStatement -> EnrichedStatement
enrichStatement = undefined

enrichIndex :: SimpleIndexType -> EnrichedIndexType
enrichIndex (IndexRange c1 c2) = IndexRange (enrichConst c1) (enrichConst c2)

enrichConst :: Constant () () () -> EnrichedConstant
enrichConst (ConstId s i)      = ConstId s (enrichIdentUnknown i)
enrichConst (ConstInt s n)     = ConstInt s n
enrichConst (ConstDouble s d)  = ConstDouble s d
enrichConst (ConstStr s)       = ConstStr s
enrichConst (ConstChar c)      = ConstChar c
enrichConst (ConstBool b)      = ConstBool b

enrichIdent :: SimpleIdent -> Maybe SimpleType -> VarInfo -> FunInfo -> EnrichedIdent
enrichIdent (Ident name loc _ _ _) = Ident name loc

enrichIdentUnknown :: SimpleIdent -> EnrichedIdent
enrichIdentUnknown (Ident name loc _ _ _) = Ident name loc Nothing NoVarInfo NoFunInfo

enrichType :: SimpleType -> EnrichedType
enrichType (Array idxs t) = Array (map enrichIndex idxs) (enrichType t)
enrichType (Pointer t)    = Pointer (enrichType t)
enrichType (BasicType b)  = BasicType b

enrichLitExpr :: Literal -> EnrichedRExpr
enrichLitExpr lit = case lit of
    LiteralInteger _ -> Lit lit (Just (BasicType Integer))
    LiteralDouble _  -> Lit lit (Just (BasicType Real))
    LiteralChar _    -> Lit lit (Just (BasicType Character))
    LiteralString _  -> Lit lit (Just (BasicType String))
    Literal_true     -> Lit lit (Just (BasicType Boolean))
    Literal_false    -> Lit lit (Just (BasicType Boolean))

-- Extract type information from an RExpr
getRExprType :: EnrichedRExpr -> Maybe SimpleType
getRExprType (Binary _ _ _ ty)  = ty
getRExprType (Unary _ _ ty)     = ty
getRExprType (Lit _ ty)         = ty
getRExprType (FCall ident _ _)  = getIdentTypeInfo ident
getRExprType (Ref _ _ ty)       = ty
getRExprType (LExpr _ ty)       = ty

-- Extract type information from a LExpr
getLExprType :: EnrichedLExpr -> Maybe SimpleType
getLExprType (LId _ ty)      = ty
getLExprType (LArr _ _ ty _) = ty
getLExprType (LDeref _ ty _) = ty

-- Convert an enriched constant to a simple one
toSimpleConstant :: EnrichedConstant -> Constant () () ()
toSimpleConstant (ConstId s i)      = ConstId s (toSimpleIdent i)
toSimpleConstant (ConstInt s n)     = ConstInt s n
toSimpleConstant (ConstDouble s d)  = ConstDouble s d
toSimpleConstant (ConstStr str)     = ConstStr str

-- Convert an enriched identifier to a simple one
toSimpleIdent :: EnrichedIdent -> SimpleIdent
toSimpleIdent (Ident name loc _ _ _) = Ident name loc () () ()

-- Extract the type information from an enriched identifier
getIdentTypeInfo :: EnrichedIdent -> Maybe SimpleType
getIdentTypeInfo (Ident _ _ typeInfo _ _) = typeInfo

setRExprTypeInfo :: EnrichedRExpr -> Maybe SimpleType -> EnrichedRExpr
setRExprTypeInfo (Binary e1 op e2 _) ty = Binary e1 op e2 ty
setRExprTypeInfo (Unary op e _) ty = Unary op e ty
setRExprTypeInfo (Lit lit _) ty = Lit lit ty
setRExprTypeInfo (FCall ident args loc) ty = FCall (setIdentTypeInfo ident ty) args loc
setRExprTypeInfo (Ref op rexpr _) ty = Ref op rexpr ty
setRExprTypeInfo (LExpr lexpr _) ty = LExpr (setLExprTypeInfo lexpr ty) ty

setIdentTypeInfo :: EnrichedIdent -> Maybe SimpleType -> EnrichedIdent
setIdentTypeInfo (Ident name loc _ v f) ty = Ident name loc ty v f

setLExprTypeInfo :: EnrichedLExpr -> Maybe SimpleType -> EnrichedLExpr
setLExprTypeInfo (LId ident _) ty = LId (setIdentTypeInfo ident ty) ty
setLExprTypeInfo (LArr lexpr indices _ loc) ty = LArr (setLExprTypeInfo lexpr ty) indices ty loc
setLExprTypeInfo (LDeref lexpr _ loc) ty = LDeref (setLExprTypeInfo lexpr ty) ty loc

-- Check if any index in an array declaration is negative
checkNegativeIndex :: Env -> [SimpleIndexType] -> Loc -> (Env, Bool)
checkNegativeIndex env [] _ = (env, False)
checkNegativeIndex env (IndexRange c1 c2 : xs) loc =
    let
        val1 = getConstVal env c1
        val2 = getConstVal env c2
        isCurrentNegative = val1 < 0 || val2 < 0
        env1 = if isCurrentNegative
                        then addError env (NegativeArrIndex, loc) -- Error if there's a negative index
                        else env
        (finalEnv, hasErrorInRest) = checkNegativeIndex env1 xs loc -- Recursive call, check the reset of the list
    in
        (finalEnv, isCurrentNegative || hasErrorInRest)

-----------------------------------------------------------
-------------------- SUPPORT FUNCTIONS --------------------
-----------------------------------------------------------

-- Extract the identifier name
getIdentName :: SimpleIdent -> String
getIdentName (Ident name _ _ _ _) = name

-- Extract the location of an identifier
getIdentLoc :: SimpleIdent -> Loc
getIdentLoc (Ident _ loc _ _ _) = loc

-- Infer the type of a constant
inferConstantType :: Constant () () () -> SimpleType
inferConstantType (ConstInt _ _)    = BasicType Integer
inferConstantType (ConstDouble _ _) = BasicType Real
inferConstantType (ConstStr _)      = BasicType String
inferConstantType (ConstChar _)     = BasicType Character
inferConstantType (ConstBool _)     = BasicType Boolean
inferConstantType _                 = BasicType Void -- Not a constant

-- Search if the name is already in the environment and return it if present
lookupByName :: Env -> String -> Maybe (EnvEntry, Bool)
lookupByName env name =
    let
        -- Filter current scope
        matchingEntries = filter (\(Ident n _ _ _ _, _) -> n == name) (Map.toList (symbols env))
    in
    case matchingEntries of
        (_, entry):_ -> Just (entry, False) -- Found in current scope
        [] ->                               -- Not found, check parent
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

-- Flatten the parameters list in case of declarations like "a, b, c: Integer, d: Real"
paramsToList :: [SimpleFormalParameters] -> [SimpleFormalParameters]
paramsToList = concatMap expandParam
  where
    expandParam (FormParam mod idents ty) = [FormParam mod [ident] ty | ident <- idents]

isLExpr :: EnrichedRExpr -> Bool
isLExpr (LExpr {}) = True
isLExpr _          = False

isPointer :: EnrichedLExpr -> Bool
isPointer lexpr = case getLExprType lexpr of
    Just (Pointer _)    -> True
    _                   -> False

isArray :: EnrichedLExpr -> Bool
isArray lexpr = case getLExprType lexpr of
    Just (Array _ _)    -> True
    _                   -> False

-- Extract the range of an array from its index type
getArrRange :: Env -> [SimpleIndexType] -> (Integer, Integer)
getArrRange env [] = (0, -1) -- Invalid range for an array
getArrRange env (IndexRange c1 c2 : _) = (getConstVal env c1, getConstVal env c2)

-- Extract integer value from a constant, resolving identifiers from the environment
getConstVal :: Env -> Constant () () () -> Integer
getConstVal _ (ConstInt sign n) = case sign of
    OptSignMinus -> negate n
    _            -> n
getConstVal env (ConstId sign ident) =
    let value = case lookupByName env (getIdentName ident) of
            Just (ConstVarEntry _ True (Just (ConstInt _ val)) _, _) -> val
            _ -> 0 -- Not a known integer constant, default to 0
    in case sign of
        OptSignMinus -> negate value
        _            -> value
getConstVal _ _ = 0 -- Other constant types

-- Indices of an array can only be integers
getValFromIndices :: [EnrichedRExpr] -> [Maybe Integer]
getValFromIndices = map evalConstExpr

evalConstExpr :: EnrichedRExpr -> Maybe Integer
evalConstExpr (Lit (LiteralInteger n) _) = Just n
evalConstExpr (Unary (UnaryPlus _) e _) = evalConstExpr e
evalConstExpr (Unary (UnaryMinus _) e _) = fmap negate (evalConstExpr e)
evalConstExpr (Binary e1 op e2 _) =
  case (evalConstExpr e1, evalConstExpr e2) of
    (Just v1, Just v2) -> case op of
        BinaryPlus _    -> Just (v1 + v2)
        BinaryMinus _   -> Just (v1 - v2)
        BinaryMul _     -> Just (v1 * v2)
        BinaryDiv _     -> if v2 /= 0 then Just (v1 `div` v2) else Nothing
        BinaryIntDiv _  -> if v2 /= 0 then Just (v1 `div` v2) else Nothing
        BinaryMod _     -> if v2 /= 0 then Just (v1 `mod` v2) else Nothing
        _               -> Nothing -- non-integer values
    _ -> Nothing
evalConstExpr _ = Nothing -- other values cannot be evaluated at compile-time

-- True if the LExpr has been defined as a constant
isConstant :: EnrichedLExpr -> Bool
isConstant lexpr = case lexpr of
    LId ident _ -> case ident of
        Ident _ _ _ (VarInfo _ isConst) _ -> isConst
        _ -> False
    LArr l _ _ _ -> isConstant l
    LDeref l _ _ -> isConstant l

isBoolean :: EnrichedRExpr -> Bool
isBoolean rexpr = case getRExprType rexpr of
    Just (BasicType Boolean) -> True
    _ -> False

getFunHeadType :: EnrichedProcedureAndFunctionHeading -> Maybe SimpleType
getFunHeadType (ProcHead _ _)      = Just (BasicType Void)
getFunHeadType (FunHead _ _ retTy _) = getFunType retTy

getFunType :: EnrichedType -> Maybe SimpleType
getFunType (Array _ ty)     = getFunType ty
getFunType (Pointer ty)     = getFunType ty
getFunType (BasicType t)    = Just (BasicType t)

-- Helper to extract Loc from a SimpleRExpr (for error reporting in If)
getCondLoc :: SimpleRExpr -> Loc
getCondLoc expr = case expr of
    Binary _ op _ _ -> getBinaryLoc op
    Unary op _ _    -> getUnaryLoc op
    FCall _ _ loc   -> loc
    _               -> Loc 0 0

-- Helper to extract Loc from a Binary operator
getBinaryLoc :: Binary -> Loc
getBinaryLoc op = case op of
    BinaryPlus loc      -> loc
    BinaryMinus loc     -> loc
    BinaryMul loc       -> loc
    BinaryDiv loc       -> loc
    BinaryIntDiv loc    -> loc
    BinaryMod loc       -> loc
    BinaryOr loc        -> loc
    BinaryAnd loc       -> loc
    BinaryEq loc        -> loc
    BinaryNeq loc       -> loc
    BinaryLt loc        -> loc
    BinaryLte loc       -> loc
    BinaryGt loc        -> loc
    BinaryGte loc       -> loc

-- Helper to extract Loc from a Unary operator
getUnaryLoc :: Unary -> Loc
getUnaryLoc op = case op of
    UnaryPlus loc   -> loc
    UnaryMinus loc  -> loc
    UnaryNot loc    -> loc
    UnaryRef loc    -> loc

hasReturnStmt :: Env -> EnrichedProcedureAndFunctionHeading -> EnrichedBlock -> Env
hasReturnStmt env head (Block _ _ _ (CompStmt stmts)) = case head of
    (ProcHead {}) -> env -- return can't be inside a procedure
    (FunHead ident _ retTy loc) ->
        if hasReturnEveryPath stmts
        then env
        else addError env (MissingReturn, loc)

-- Check if each execution path in a function terminate with a return
hasReturnEveryPath :: [EnrichedStatement] -> Bool
hasReturnEveryPath [] = False
hasReturnEveryPath stmts = anyPathReturns stmts
    where
    anyPathReturns [] = False
    anyPathReturns (s:ss) =
        case s of
        ReturnStmt {} -> True
        Stmts (CompStmt inner) -> anyPathReturns (inner ++ ss)
        If _ thenStmt optElse _ ->
            case optElse of
            NoElse -> anyPathReturns ss -- return not guaranteed
            Else elseStmt -> hasReturnEveryPath [thenStmt] && hasReturnEveryPath [elseStmt] || anyPathReturns ss
        Case _ caseItems -> not (null caseItems) && all (\(CaseItem _ stmt) -> hasReturnEveryPath [stmt]) caseItems || anyPathReturns ss
        _ -> anyPathReturns ss -- return not guaranteed



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
showErrors errs = intercalate "\n" (map showError (nub errs)) -- (nub errs) removes duplicated errors caused by the mutual recursion mechanism
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