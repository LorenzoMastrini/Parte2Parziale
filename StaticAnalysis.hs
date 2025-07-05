module StaticAnalysis where

import Data.Map (Map, toList)
import qualified Data.Map as Map
import Data.Maybe (isJust, isNothing)
import Data.List (intercalate, nub, sortBy)
import Data.Ord (comparing)
import AbsLinguaggio
import ErrorStaticAnalysis hiding (showTy)



-- Environment
data Env = Env {
    symbols :: Map.Map SimpleIdent EnvEntry,    -- Symbols in the environment
    parent  :: Maybe Env,                       -- Parent environment for nested blocks of code
    errors  :: [(String, Loc)]                  -- List of errors (error message, location)
  } deriving (Eq, Show)

data EnvEntry
    = ConstVarEntry SimpleType Bool (Maybe SimpleConstant) (Maybe (Int, Int)) Loc -- (type, is constant, value if constant, arrayBound(if array) location)
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
defaultEnv = Env {
    symbols = Map.fromList defaultFuncList,
    parent = Nothing,
    errors = []
    }



---------------------------------------------------------
-------------------- STATIC ANALYSIS --------------------
---------------------------------------------------------
-- The static analysis updates the informations of the AST needed in subsequent step like TAC generation

-- Called in Compiler.hs. In the environment structure there are all the possible errors.
checkProgram :: SimpleProgram -> (Env, EnrichedProgram)
checkProgram (Prog ident params block loc) =
    let
        (env1, paramsEnriched) = checkFormalParametersList defaultEnv params
        (env2, blockEnriched) = checkBlock env1 block False (BasicType Void)
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
        (env1, identsEnriched) = checkIdentsList env idents ty (VarInfo Nothing False Nothing) NoFunInfo
    in (env1, FormParam mod identsEnriched (enrichType ty))

checkBlock :: Env -> SimpleBlock -> Bool -> SimpleType -> (Env, EnrichedBlock)
checkBlock env (Block constDefList varDeclList procFunDeclList compStmt) isInsideFun funType =
    let
        -- Don't create new scope if already in a function scope
        localEnv = case parent env of
            Just _ -> env  -- Already in a function, use the same environment
            Nothing -> env  -- Program uses the default environment

        -- Two-pass: raccoglie tutte le intestazioni di funzioni/procedure nell'ambiente
        -- Second pass: analyze constants, variables, and function bodies
        (env1, constDefListEnriched) = checkConstDefList localEnv constDefList
        (env2, varDeclListEnriched) = checkVarDeclList env1 varDeclList
        (env3, procFunDeclListEnriched) = checkProcFunDeclList env2 procFunDeclList
        (env4, compStmtEnriched) = checkCompoundStatement env3 compStmt False isInsideFun funType Nothing
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
        (env1, identEnriched) = checkIdent env ident ty (VarInfo (Just constVal) True Nothing) NoFunInfo
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
        -- Check for identifiers redeclarations
        (envWithId, idEnr) = case ty of
            Array indexType _ _ ->
                let
                    loc = getIdentLoc id
                    arrRange = getRange indexType
                    enrichedTy = enrichType ty
                    (env1, baseIdEnr) = checkIdent env id ty (VarInfo Nothing False arrRange) NoFunInfo
                in (env1, baseIdEnr)
            _ -> checkIdent env id ty (VarInfo Nothing False Nothing) NoFunInfo -- Not an array, check just the identifier
        -- Recursive call
        (finalEnv, VarDecl idsEnr tyEnr) = checkVarDeclSequence envWithId ids ty
    in (finalEnv, VarDecl (idEnr : idsEnr) tyEnr) -- Final result

-- Iterate the list of procedures and functions declarations
checkProcFunDeclList :: Env -> [SimpleProcedureAndFunctionDeclaration] -> (Env, [EnrichedProcedureAndFunctionDeclaration])
checkProcFunDeclList env decls =
  let envWithFuncs = collectFunctionDeclarations env decls -- To handle mutual ricursion
  in checkProcFunBodies envWithFuncs decls

-- Two-pass analysis: get only the headings of the procesdures/functions in the environment
collectFunctionDeclarations :: Env -> [SimpleProcedureAndFunctionDeclaration] -> Env
collectFunctionDeclarations env [] = env
collectFunctionDeclarations env (ProcFunDecl heading _ : rest) =
  let env1 = addProcFunSignature env heading
  in collectFunctionDeclarations env1 rest

-- Add only the procedure/function signature to the environment
addProcFunSignature :: Env -> SimpleProcedureAndFunctionHeading -> Env
addProcFunSignature env (ProcHead ident params) =
  if any (\(Ident name _ _ _ _, _) -> name == getIdentName ident) (Map.toList (symbols env))
    then addError env (RedeclaredProcFun ident (getIdentLoc ident), getIdentLoc ident)
    else addEntry env ident (ProcFunEntry params (BasicType Void) (getIdentLoc ident))
addProcFunSignature env (FunHead ident params retTy loc) =
  if any (\(Ident name _ _ _ _, _) -> name == getIdentName ident) (Map.toList (symbols env))
    then addError env (RedeclaredProcFun ident (getIdentLoc ident), getIdentLoc ident)
    else addEntry env ident (ProcFunEntry params retTy (getIdentLoc ident))

checkProcFunBodies :: Env -> [SimpleProcedureAndFunctionDeclaration] -> (Env, [EnrichedProcedureAndFunctionDeclaration])
checkProcFunBodies env [] = (env, [])
checkProcFunBodies env (d:ds) =
  let (env1, dEnr) = checkProcFunDecl env d
      (env2, dsEnr) = checkProcFunBodies env1 ds
  in (env2, dEnr:dsEnr)

checkProcFunDecl :: Env -> SimpleProcedureAndFunctionDeclaration -> (Env, EnrichedProcedureAndFunctionDeclaration)
checkProcFunDecl env (ProcFunDecl heading block) =
    let
        localEnv = enterScope env -- Create a scope for the function
        (localEnv1, headingEnriched) = checkProcFunHead localEnv heading
        retTy = getHeadingType headingEnriched
        (finalLocalEnv, blockEnriched) = checkBlock localEnv1 block True retTy
        envHasReturn = hasReturnStmt finalLocalEnv headingEnriched blockEnriched
        Just outerEnv = parent envHasReturn -- Get the updated outer environment
        finalOuterEnv = outerEnv { errors = errors outerEnv ++ errors envHasReturn } -- Propagate the errors to the outer environment
    in (finalOuterEnv, ProcFunDecl headingEnriched blockEnriched)


checkProcFunHead :: Env -> SimpleProcedureAndFunctionHeading -> (Env, EnrichedProcedureAndFunctionHeading)
checkProcFunHead localEnv head = case head of
    (ProcHead ident params) ->
        let
            identEnriched = enrichIdent ident (BasicType Void) NoVarInfo (FunInfo (BasicType Void) params)
            (localEnvWithFunc, _) = checkProcFunIdent localEnv ident (BasicType Void) NoVarInfo (FunInfo (BasicType Void) params) params -- Add signature only to localEnv for recursion
            (localEnv1, paramsEnriched) = checkFormalParametersList localEnvWithFunc params
            updatedLocalEnv = localEnv1 { parent = parent localEnv }
        in (updatedLocalEnv, ProcHead identEnriched paramsEnriched)
    (FunHead ident params retTy loc) ->
        let
            identEnriched = enrichIdent ident (enrichType retTy) NoVarInfo (FunInfo retTy params)
            (localEnvWithFunc, _) = checkProcFunIdent localEnv ident retTy NoVarInfo (FunInfo retTy params) params -- Add signature only to localEnv for recursion
            (localEnv1, paramsEnriched) = checkFormalParametersList localEnvWithFunc params
            updatedLocalEnv = localEnv1 { parent = parent localEnv }
        in (updatedLocalEnv, FunHead identEnriched paramsEnriched retTy loc)

-- Given a list of identifiers, check recursively if each of them is already used inside the environment
checkIdentsList :: Env -> [SimpleIdent] -> SimpleType -> VarInfo -> FunInfo -> (Env, [EnrichedIdent])
checkIdentsList env [] _ _ _ = (env, [])
checkIdentsList env (i:is) ty varInfo funInfo =
    let (env', iEnr) = checkIdent env i ty varInfo funInfo
        (envFinal, isEnr) = checkIdentsList env' is ty varInfo funInfo
    in (envFinal, iEnr : isEnr)

-- Check if the identifier is already used inside the environment. If not, add it to the environment
checkIdent :: Env -> SimpleIdent -> SimpleType -> VarInfo -> FunInfo -> (Env, EnrichedIdent)
checkIdent env ident@(Ident name loc _ _ _) ty varInfo@(VarInfo constVal isConst arrayRange) funInfo =
    let res = lookupByName env name
    in case res of
        Just (ConstVarEntry _ entryIsConst _ _ _, False) ->
            (addError env (RedeclaredConstVar ident entryIsConst, loc), enrichIdentUnknown ident)
        Just (ProcFunEntry _ _ existingFunLoc, False) ->
            (addError env (RedeclaredProcFun ident existingFunLoc, loc), enrichIdentUnknown ident)
        _ ->
            (addEntry env ident (ConstVarEntry ty isConst constVal arrayRange loc), enrichIdent ident (enrichType ty) varInfo funInfo)

-- Update the environment with the declaration of a procedure/function and return its enriched identifier
checkProcFunIdent :: Env -> SimpleIdent -> SimpleType -> VarInfo -> FunInfo -> [SimpleFormalParameters] -> (Env, EnrichedIdent)
checkProcFunIdent env ident ty NoVarInfo (FunInfo retTy paramsEnriched) simpleParams =
    let res = Map.lookup ident (symbols env) -- check only the local scope for procedures/functions
    in case res of
        Just (ConstVarEntry _ isConst _ _ _) ->
            (addError env (RedeclaredConstVar ident isConst, getIdentLoc ident), enrichIdentUnknown ident)
        Just (ProcFunEntry _ _ existingFunLoc) ->
            (addError env (RedeclaredProcFun ident existingFunLoc, getIdentLoc ident), enrichIdentUnknown ident)
        Nothing ->
            if retTy == BasicType Void
                then (addEntry env ident (ProcFunEntry simpleParams (BasicType Void) (getIdentLoc ident)), enrichIdent ident (enrichType ty) NoVarInfo (FunInfo retTy paramsEnriched))
                else (addEntry env ident (ProcFunEntry simpleParams ty (getIdentLoc ident)), enrichIdent ident (enrichType ty) NoVarInfo (FunInfo retTy paramsEnriched))
checkProcFunIdent env ident ty (VarInfo _ isConst arrayRange) funInfo _ =
    let res = lookupByName env (getIdentName ident)
    in case res of
        Just (ConstVarEntry _ entryIsConst _ _ _, _) ->
            (addError env (RedeclaredConstVar ident entryIsConst, getIdentLoc ident), enrichIdentUnknown ident)
        Just (ProcFunEntry _ _ existingFunLoc, _) ->
            (addError env (RedeclaredProcFun ident existingFunLoc, getIdentLoc ident), enrichIdentUnknown ident)
        Nothing -> (addEntry env ident (ConstVarEntry ty isConst Nothing arrayRange (getIdentLoc ident)), enrichIdent ident (enrichType ty) (VarInfo Nothing isConst arrayRange) funInfo)



-----------------------------------------------------
-------------------- EXPRESSIONS --------------------
-----------------------------------------------------

checkRExpr :: Env -> SimpleRExpr -> Maybe String -> (Env, EnrichedRExpr)
checkRExpr env expr mForVar = case expr of
    Binary e1 op e2 _ ->
        let
            (env1, e1Enriched) = checkRExpr env e1 mForVar
            (env2, e2Enriched) = checkRExpr env1 e2 mForVar
            t1 = getRExprType e1Enriched
            t2 = getRExprType e2Enriched
            (env3, ty) = checkBinaryOp env2 op t1 t2
        in (env3, Binary e1Enriched op e2Enriched (enrichType ty))
    Unary op e _ ->
        let
            (env1, eEnriched) = checkRExpr env e mForVar
            t = getRExprType eEnriched
            (env3, ty) = checkUnaryOp env1 op t
        in (env3, Unary op eEnriched (enrichType ty))
    Lit lit _ -> (env, enrichLitExpr lit)
    FCall ident@(Ident name _ _ _ _) args loc -> case lookupByName env name of
        Nothing ->
            (addError env (FunNotDefined ident, loc), FCall (enrichIdentUnknown ident) [] loc) -- Function doesn't exist
        Just (ConstVarEntry {}, _) ->
            (addError env (NotAFunction ident, loc), FCall (enrichIdentUnknown ident) [] loc) -- Not a function
        Just (ProcFunEntry params retTy _, _) ->
            let
                (env1, argsEnriched, hasErrors) = checkArgs env args (paramsToList params) (length args) (length (paramsToList params)) loc mForVar
                identEnriched = if hasErrors
                    then enrichIdent ident Error NoVarInfo (FunInfo retTy params)
                    else enrichIdent ident (enrichType retTy) NoVarInfo (FunInfo retTy params)
            in (env1, FCall identEnriched argsEnriched loc)
    Ref op@(UnaryRef loc) rexpr _ ->
        let
            (env1, rexprEnriched) = checkRExpr env rexpr mForVar
            isLExprResult = isLExpr rexprEnriched
        in if isLExprResult
            then let
                t = getRExprType rexprEnriched
                refType = case t of
                            Error -> Error
                            _     -> Pointer t
                in (env1, Ref op rexprEnriched (enrichType refType))
            else (addError env1 (NotLExpr op, loc), Ref op rexprEnriched Error)
    LExpr lexpr _ ->
        let
            (env1, lexprEnriched) = checkLExpr env lexpr mForVar
            ty = getLExprType lexprEnriched
        in (env1, LExpr lexprEnriched (enrichType ty))
    _ -> error $ "DEBUG: checkStatement pattern non gestito: " ++ show expr

checkArgs :: Env -> [SimpleRExpr] -> [SimpleFormalParameters] -> Int -> Int -> Loc -> Maybe String -> (Env, [EnrichedRExpr], Bool) -- The boolean return True if there are errors
checkArgs env [] [] origArgs origParams _ _ = (env, [], False) -- No parameters (no errors)
-- Check mismatch between arguments and formal parameters
checkArgs env [] params origArgs origParams loc _ =
    let env1 = addError env (FunArgsNumMismatch origArgs origParams, loc)
    in (env1, [], True)
-- Check mismatch between arguments and formal parameters
checkArgs env args [] origArgs origParams loc _ =
    let env1 = addError env (FunArgsNumMismatch origArgs origParams, loc)
    in (env1, [], True)
-- Same number of arguments and parameters, continue with other checks
checkArgs env (a:as) (p:ps) origArgs origParams loc mForVar =
    let
        (env1, argEnriched) = checkRExpr env a mForVar
        (env2, compatible) = checkArgType env1 argEnriched p loc
        (env3, argsEnriched, hasTypeErrorRest) = checkArgs env2 as ps origArgs origParams loc mForVar
        hasTypeError = not compatible || hasTypeErrorRest
    in (env3, argEnriched : argsEnriched, hasTypeError)

checkArgType :: Env -> EnrichedRExpr -> SimpleFormalParameters -> Loc -> (Env, Bool)
checkArgType env argEnriched (FormParam mod _ paramType) loc = case mod of
    ByVal -> let
                argType = getRExprType argEnriched
                (env1, compatible) = areParamsCompatible env argType paramType loc -- Regular compatibility type checks
            in (env1, compatible)
    _ -> -- ByRef modality
        if isLExpr argEnriched then
            let
                argType = getRExprType argEnriched
            in if argType == paramType -- In ByRef modality the types must be exactly the same
                then (env, True)
                else (addError env (IncompatibleRefModParamType argType paramType, loc), False)
        else (addError env (NotLExprInRefMod, loc), False) -- Not L-expresion in ByRef modality

checkLExpr :: Env -> SimpleLExpr -> Maybe String -> (Env, EnrichedLExpr)
checkLExpr env lexpr mForVar = case lexpr of
    LId ident _ ->
        let
            (env1, hasErrors) = case mForVar of
                    -- Control variable of for loop cannot be used inside its body
                    Just forName | getIdentName ident == forName -> (addError env (ForControlVarUsedInBody ident (getIdentLoc ident), getIdentLoc ident), True)
                    _ -> (env, False)
        in if hasErrors
            then
                (env1, LId (enrichIdentUnknown ident) Error)  -- Don't add errors caused by other errors (for control variable used inside for)
            else let
                    (env2, identEnriched) = identExists env1 ident
                    ty = getIdentType identEnriched
                in (env2, LId identEnriched ty)
    LArr lexpr' index _ loc ->
        let
            (env1, lexprEnriched) = checkLExpr env lexpr' mForVar
            (env2, indexEnriched) = checkArrIndex env1 index loc mForVar
            isArrayResult = isArray lexprEnriched
            hasError = getLExprType lexprEnriched == Error || getRExprType indexEnriched == Error
        in if hasError
            then (env2, LArr lexprEnriched indexEnriched Error loc)
            else if isArrayResult && not hasError
                then if isInteger indexEnriched
                    then
                        let Array i ty varInfo = getLExprType lexprEnriched
                        in (env2, LArr lexprEnriched indexEnriched (enrichType ty) loc)
                    else (addError env2 (NotIntegerArrIndex (getRExprType indexEnriched), loc), LArr lexprEnriched indexEnriched Error loc)
                else (addError env2 (NotAnArray, loc), LArr lexprEnriched indexEnriched Error loc)
    LDeref lexpr' _ loc ->
        let
            (env1, lexprEnriched) = checkLExpr env lexpr' mForVar
            isPointerResult = isPointer lexprEnriched
        in if isPointerResult
            then let
                Pointer t = getLExprType lexprEnriched
                in (env1, LDeref lexprEnriched (enrichType t) loc)
            else (addError env1 (NotAPointer, loc), LDeref lexprEnriched Error loc)

checkArrIndex :: Env -> SimpleRExpr -> Loc -> Maybe String -> (Env, EnrichedRExpr)
checkArrIndex env index loc mForVar =
    let
        (env1, indexEnriched) = checkRExpr env index mForVar
        indexType = getRExprType indexEnriched
    in (if (indexType == Error) || (indexType == BasicType Integer)
        then (env1, indexEnriched) -- If there's an error or the type is correct do not add errors to environment
        else (
            let env2 = addError env1 (NotIntegerArrIndex indexType, loc)
            in (env2, indexEnriched)
        ))



----------------------------------------------------
-------------------- STATEMENTS --------------------
----------------------------------------------------

checkCompoundStatement :: Env -> SimpleCompoundStatement -> Bool -> Bool -> SimpleType -> Maybe String -> (Env, EnrichedCompoundStatement)
checkCompoundStatement env (CompStmt stmts) isLoop isInsideFun funType mForVar =
    let
        (env1, stmtsEnriched) = checkStatementList env stmts isLoop isInsideFun funType mForVar
    in (env1, CompStmt stmtsEnriched)

checkStatementList :: Env -> [SimpleStatement] -> Bool -> Bool -> SimpleType -> Maybe String -> (Env, [EnrichedStatement])
checkStatementList env [] isLoop isInsideFun funType mForVar = (env, [])
checkStatementList env (stmt:stmts) isLoop isInsideFun funType mForVar =
    let
        (env1, stmtEnriched) = checkStatement env stmt isLoop isInsideFun funType mForVar
        (env2, stmtsEnriched) = checkStatementList env1 stmts isLoop isInsideFun funType mForVar
    in (env2, stmtEnriched:stmtsEnriched)

checkStatement :: Env -> SimpleStatement -> Bool -> Bool -> SimpleType -> Maybe String -> (Env, EnrichedStatement)
checkStatement env stmt isLoop isInsideFun funType mForVar = case stmt of
    Stmts compStmt ->
        let
            (env1, compStmtEnriched) = checkCompoundStatement env compStmt isLoop isInsideFun funType mForVar
        in (env1, Stmts compStmtEnriched)
    RExprStmt expr _ ->
        let
            (env1, exprEnriched) = checkRExpr env expr mForVar
        in (env1, RExprStmt exprEnriched (enrichType (getRExprType exprEnriched)))
    Assign lexpr rexpr _ loc ->
        let
            (env1, lexprEnriched) = checkLExpr env lexpr mForVar
            (env2, rexprEnriched) = checkRExpr env1 rexpr mForVar
        in if isConstant lexprEnriched
            then (addError env2 (AssignmentToConstant, loc), Assign lexprEnriched rexprEnriched Error loc) -- If the checks didn't pass as defined in the grammar, it must be a constant
            else let
                (env3, assTy) = checkCompatibleAssignmentTypes env2 (getLExprType lexprEnriched) (getRExprType rexprEnriched) loc
                in (env3, Assign lexprEnriched rexprEnriched (enrichType assTy) loc)
    If cond thenStmt optElse loc ->
        let
            (env1, condEnriched) = checkRExpr env cond mForVar
            (env2, thenEnriched) = checkStatement env1 thenStmt isLoop isInsideFun funType mForVar
            (env3, optElseEnriched) = checkOptElse env2 optElse isLoop isInsideFun funType mForVar
            envFinal = if getRExprType condEnriched == BasicType Boolean
                then env3
                else addError env3 (NotBooleanCondition "if" (getRExprType condEnriched), loc)
        in (envFinal, If condEnriched thenEnriched optElseEnriched loc)
    While rexpr stmt loc ->
        let
            (env1, rexprEnriched) = checkRExpr env rexpr mForVar
            (env2, stmtEnriched) = checkStatement env1 stmt True isInsideFun funType mForVar
        in if isBoolean rexprEnriched
            then (env2, While rexprEnriched stmtEnriched loc)
            else case getRExprType rexprEnriched of
                Error -> (env2, While rexprEnriched stmtEnriched loc) -- Don't add an error if the condition itself contains an error
                _ -> (addError env2 (NotBooleanCondition "while" (getRExprType rexprEnriched), loc), While rexprEnriched stmtEnriched loc)
    DoWhile stmt rexpr loc ->
        let
            (env1, rexprEnriched) = checkRExpr env rexpr mForVar
            (env2, stmtEnriched) = checkStatement env1 stmt True isInsideFun funType mForVar
        in if isBoolean rexprEnriched
            then (env2, DoWhile stmtEnriched rexprEnriched loc)
            else case getRExprType rexprEnriched of
                Error -> (env2, DoWhile stmtEnriched rexprEnriched loc) -- Don't add an error if the condition itself contains an error
                _ -> (addError env2 (NotBooleanCondition "repeat-until" (getRExprType rexprEnriched), loc), DoWhile stmtEnriched rexprEnriched loc)
    ForTo lexpr rexpr1 rexpr2 stmt loc ->
        let
            (env1, rexpr1Enriched) = checkRExpr env rexpr1 Nothing
            (env2, rexpr2Enriched) = checkRExpr env1 rexpr2 Nothing
            ty = getRExprType rexpr1Enriched
            localEnv = enterScope env2 -- Create local env for the for loop
            (localEnv1, controlVarEnriched) = checkForControlVar localEnv lexpr ty loc
            forVarName = case lexpr of
                LId (Ident name _ _ _ _) _ -> Just name
                _ -> Nothing
            (localEnv2, stmtEnriched) = checkStatement localEnv1 stmt True isInsideFun funType forVarName
        in if getRExprType rexpr1Enriched == getRExprType rexpr2Enriched -- Bounds of the array must be of the same type
            then (mergeForEnvs env2 localEnv2, ForTo controlVarEnriched rexpr1Enriched rexpr2Enriched stmtEnriched loc)
            else (addError (mergeForEnvs env2 localEnv2) (ForBoundsTypeMismatch, loc), ForTo controlVarEnriched rexpr1Enriched rexpr2Enriched stmtEnriched loc)
    ReturnStmt rexpr _ loc ->
        let
            (env1, rexprEnriched) = checkRExpr env rexpr mForVar
            ty = getRExprType rexprEnriched
        in if not isInsideFun
            then (addError env1 (NotInFunction, loc), ReturnStmt rexprEnriched Error loc) -- Return is not inside a function
            else case ty of
                Error -> (env1, ReturnStmt rexprEnriched Error loc) -- Don't add errors to the environment
                t -> if t == funType
                    then (env1, ReturnStmt rexprEnriched (enrichType funType) loc)
                    else (addError env1 (ReturnTypeMismatch t funType, loc), ReturnStmt rexprEnriched (enrichType funType) loc)
    ReturnNoExpr _ loc -> if not isInsideFun
            then (addError env (NotInFunction, loc), ReturnNoExpr Error loc) -- Return is not inside a function
            else if funType == BasicType Void
                    then (env, ReturnNoExpr (enrichType funType) loc)
                    else (addError env (ReturnTypeMismatch (BasicType Void) funType, loc), ReturnNoExpr (enrichType funType) loc)
    BreakStmt loc -> if isLoop
        then (env, BreakStmt loc)
        else (addError env (NotInLoop "break", loc), BreakStmt loc)
    ContinueStmt loc -> if isLoop
        then (env, ContinueStmt loc)
        else (addError env (NotInLoop "continue", loc), ContinueStmt loc)
    OpAssign lexpr op rexpr ty ->
        let
            (env1, lexprEnriched) = checkLExpr env lexpr mForVar
            (env2, rexprEnriched) = checkRExpr env1 rexpr mForVar
            loc = getOpAssignLoc op
        in if isConstant lexprEnriched
            then (addError env2 (AssignmentToConstant, loc), Assign lexprEnriched rexprEnriched Error loc) -- If the checks didn't pass as defined in the grammar, it must be a constant
            else let
                (env3, ty) = checkOpAssign env2 op (getLExprType lexprEnriched) (getRExprType rexprEnriched)
                (env4, assTy) = checkCompatibleAssignmentTypes env3 (getLExprType lexprEnriched) (getRExprType rexprEnriched) loc
                in (env4, Assign lexprEnriched rexprEnriched (enrichType assTy) loc)

-------------------- Assignments --------------------
-- Check the type compatibility in an assignment
checkCompatibleAssignmentTypes :: Env -> SimpleType -> SimpleType -> Loc -> (Env, SimpleType)
checkCompatibleAssignmentTypes env targetType sourceType loc
    | targetType == Error || sourceType == Error = (env, Error) -- If one of the two types is Error, stop the analysis
    | targetType == sourceType = (env, targetType) -- Same type: compatible
    | otherwise = case (targetType, sourceType) of
        (BasicType Real, BasicType Integer)     -> (env, targetType) -- Implicit cast: Integer -> Real
        (BasicType Integer, BasicType Boolean)  -> (env, targetType) -- Implicit cast: Boolean -> Integer
        (BasicType Real, BasicType Boolean)     -> (env, targetType) -- Implicit cast: Boolean -> Real
        (Array _ elemType _, Pointer ptrType)   -> if elemType == ptrType
            then (env, targetType)
            else (addError env (IncompatibleArrPtrType, loc), Error)
        (Pointer ptrType, Array _ elemType _)   -> if ptrType == elemType
            then (env, targetType)
            else (addError env (IncompatiblePtrArrType, loc), Error)
        _ -> (addError env (IncompatibleAssignment targetType sourceType, loc), Error)

-- Check type compatibility for the operations of ":+=", ":-=" and ":*=" assignments
checkOpAssign :: Env -> OpAssignType -> SimpleType -> SimpleType -> (Env, SimpleType)
checkOpAssign env op t1 t2 = if t1 == Error || t2 == Error
    then (env, Error) -- One of the two types is Error, stop expression analysis
    else case op of
    OpAssignAdd loc -> checkNumericOp env "+" t1 t2 loc
    OpAssignSub loc -> checkNumericOp env "-" t1 t2 loc
    OpAssignMul loc -> checkNumericOp env "*" t1 t2 loc

-------------------- If-Then-Else --------------------
checkOptElse :: Env -> SimpleOptElse -> Bool -> Bool -> SimpleType -> Maybe String -> (Env, EnrichedOptElse)
checkOptElse env optElse isLoop isInsideFun funType mForVar = case optElse of
    NoElse -> (env, NoElse)
    Else stmt ->
        let (env1, stmtEnriched) = checkStatement env stmt isLoop isInsideFun funType mForVar
        in (env1, Else stmtEnriched)

-------------------- For --------------------
checkForControlVar :: Env -> SimpleLExpr -> SimpleType -> Loc -> (Env, EnrichedLExpr)
checkForControlVar env lexpr ty loc = case lexpr of
    LId ident@(Ident _ idLoc _ _ _) _ -> case ty of
        (BasicType Integer) ->
            let
                identEnriched = enrichIdent ident (enrichType ty) (VarInfo Nothing False Nothing) NoFunInfo
                env1 = addEntry env ident (ConstVarEntry ty False Nothing Nothing idLoc) -- Implicit declaration of control variable
            in (env1, LId identEnriched (enrichType ty))
        (BasicType Boolean) ->
            let
                identEnriched = enrichIdent ident (enrichType ty) (VarInfo Nothing False Nothing) NoFunInfo
                env1 = addEntry env ident (ConstVarEntry ty False Nothing Nothing idLoc)
            in (env1, LId identEnriched (enrichType ty))
        (BasicType Real) ->
            let
                identEnriched = enrichIdent ident (enrichType ty) (VarInfo Nothing False Nothing) NoFunInfo
                env1 = addEntry env ident (ConstVarEntry ty False Nothing Nothing idLoc)
            in (env1, LId identEnriched (enrichType ty))
        _ -> 
            let
                identEnriched = enrichIdent ident (enrichType ty) (VarInfo Nothing False Nothing) NoFunInfo
            in (addError env (ForControlVarTypeNotAllowed ty, loc), LId identEnriched Error)
    LArr lexpr' index _ loc -> 
        let
            (env1, lexprEnriched) = checkLExpr env lexpr' Nothing
            (env2, indexEnriched) = checkArrIndex env1 index loc Nothing
            isArrayResult = isArray lexprEnriched
        in (addError env2 (ForControlVarNotLId, loc), LArr lexprEnriched indexEnriched Error loc)
    LDeref lexpr' _ loc -> 
        let
            (env1, lexprEnriched) = checkLExpr env lexpr' Nothing
        in (addError env1 (ForControlVarNotLId, loc), LDeref lexprEnriched Error loc)


-------------------- FCall --------------------
-- Check the type compatibility between formal and actual parameters in function calls
areParamsCompatible :: Env -> SimpleType -> SimpleType -> Loc -> (Env, Bool)
areParamsCompatible env argType formParamType loc
    | argType == Error || formParamType == Error = (env, False) -- If one of the two types is Error, don't add errors
    | argType == formParamType = (env, True) -- Same type: compatible
    | otherwise = case (formParamType, argType) of
        (BasicType Real, BasicType Integer)     -> (env, True) -- Implicit cast: Integer -> Real
        (BasicType Integer, BasicType Boolean)  -> (env, True) -- Implicit cast: Boolean -> Integer
        (BasicType Real, BasicType Boolean)     -> (env, True) -- Implicit cast: Boolean -> Real
        (Array _ elemType _, Pointer ptrType)   -> if elemType == ptrType
            then (env, True)
            else (addError env (IncompatibleArrPtrType, loc), False)
        (Pointer ptrType, Array _ elemType _)   -> if ptrType == elemType
            then (env, True)
            else (addError env (IncompatiblePtrArrType, loc), False)
        _ -> (addError env (FunArgTypeMismatch argType formParamType, loc), False)



------------------------------------------------
-------------------- CHECKS --------------------
------------------------------------------------
identExists :: Env -> SimpleIdent -> (Env, EnrichedIdent)
identExists env ident@(Ident name loc _ varInfo funInfo) = case lookupByName env name of
    Nothing ->
        (addError env (UnknownIdentifier ident, loc), enrichIdentUnknown ident) -- Identifier doesn't exist
    Just (ConstVarEntry ty isConst constVal arrRange entryLoc, _) ->
        (env, enrichIdent ident (enrichType ty) (VarInfo constVal isConst arrRange) NoFunInfo)
    Just (ProcFunEntry params retTy loc, _) ->
        (env, enrichIdent ident (enrichType retTy) NoVarInfo (FunInfo retTy params))

-------------------- Binary --------------------
checkBinaryOp :: Env -> Binary -> SimpleType -> SimpleType -> (Env, SimpleType)
checkBinaryOp env op t1 t2 = if t1 == Error || t2 == Error
    then (env, Error) -- One of the two types is Error, stop expression analysis
    else case op of
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

-- Type checking numeric operations
checkNumericOp :: Env -> String -> SimpleType -> SimpleType -> Loc -> (Env, SimpleType)
checkNumericOp env op t1 t2 loc = case (t1, t2) of
    (BasicType Integer, BasicType Integer)  -> (env, BasicType Integer)
    (BasicType Integer, BasicType Real)     -> (env, BasicType Real)    -- Implicit cast: Integer -> Real
    (BasicType Integer, BasicType Boolean)  -> (env, BasicType Integer) -- Implicit cast: Boolean -> Integer
    (BasicType Real, BasicType Real)        -> (env, BasicType Real)
    (BasicType Real, BasicType Integer)     -> (env, BasicType Real)    -- Implicit cast: Integer -> Real
    (BasicType Real, BasicType Boolean)     -> (env, BasicType Real)    -- Implicit cast: Boolean (-> Integer) -> Real
    (BasicType Boolean, BasicType Integer)  -> (env, BasicType Integer) -- Implicit cast: Boolean -> Integer
    (BasicType Boolean, BasicType Real)     -> (env, BasicType Real)    -- Implicit cast: Boolean (-> Integer) -> Real
    (BasicType Boolean, BasicType Boolean)  -> (env, BasicType Integer) -- Implicit cast: Boolean (-> Integer) -> Real
    _                                       -> (addError env (IncompatibleTypes op t1 t2, loc), Error) -- Incompatible numeric types

-- Type checking integer operations
checkIntegerOp :: Env -> String -> SimpleType -> SimpleType -> Loc -> (Env, SimpleType)
checkIntegerOp env op t1 t2 loc = case (t1, t2) of
    (BasicType Integer, BasicType Integer)  -> (env, BasicType Integer)
    (BasicType Integer, BasicType Boolean)  -> (env, BasicType Integer) -- Implicit cast: Boolean -> Integer
    (BasicType Boolean, BasicType Integer)  -> (env, BasicType Integer) -- Implicit cast: Boolean -> Integer
    (BasicType Boolean, BasicType Boolean)  -> (env, BasicType Integer) -- Implicit cast: Boolean (-> Integer) -> Real (both operands)
    _                                       -> (addError env (IncompatibleTypes op t1 t2, loc), Error) -- Incompatible numeric types

checkBooleanOp :: Env -> String -> SimpleType -> SimpleType -> Loc -> (Env, SimpleType)
checkBooleanOp env op t1 t2 loc =
    let
        (env1, mTy1) = checkBooleanType env op t1 loc
        (env2, mTy2) = checkBooleanType env1 op t2 loc
    in if mTy1 == Error || mTy2 == Error
        then (env2, Error) -- Don't add errors if present (done in checkBooleanType)
        else (env2, BasicType Boolean)

checkComparisonOp :: Env -> String -> SimpleType -> SimpleType -> Loc -> (Env, SimpleType)
checkComparisonOp env "=" t1 t2 loc = case (t1, t2) of
    (BasicType Boolean, BasicType Boolean) -> (env, BasicType Boolean)
    (BasicType Character, BasicType Character) -> (env, BasicType Boolean)
    (BasicType Integer, BasicType Integer) -> (env, BasicType Boolean)
    (BasicType Real, BasicType Real) -> (env, BasicType Boolean)
    _ -> (addError env (IncompatibleTypes "=" t1 t2, loc), Error)
checkComparisonOp env "<>" t1 t2 loc = case (t1, t2) of
    (BasicType Boolean, BasicType Boolean) -> (env, BasicType Boolean)
    (BasicType Character, BasicType Character) -> (env, BasicType Boolean)
    (BasicType Integer, BasicType Integer) -> (env, BasicType Boolean)
    (BasicType Real, BasicType Real) -> (env, BasicType Boolean)
    _ -> (addError env (IncompatibleTypes "<>" t1 t2, loc), Error)
checkComparisonOp env op t1 t2 loc =
    let (env1, _) = checkNumericOp env op t1 t2 loc -- The other comparison operations follow the arithmetic types compatibilty rules
    in (env1, BasicType Boolean)

-------------------- Unary --------------------
checkUnaryOp :: Env -> Unary -> SimpleType -> (Env, SimpleType)
checkUnaryOp env op t = if t == Error
    then (env, Error) -- If the type is Error, stop expression analysis
    else case op of
    UnaryPlus loc   -> checkArithmeticType env "+" t loc
    UnaryMinus loc  -> checkArithmeticType env "-" t loc
    UnaryNot loc    -> checkBooleanType env "not" t loc

checkBooleanType :: Env -> String -> SimpleType -> Loc -> (Env, SimpleType)
checkBooleanType env op t loc = case t of
    BasicType Boolean -> (env, BasicType Boolean) -- It can only be Boolean type
    _ -> (addError env (NotBooleanType op t, loc), Error)

checkArithmeticType :: Env -> String -> SimpleType -> Loc -> (Env, SimpleType)
checkArithmeticType env op t loc = case t of
    BasicType Integer   -> (env, BasicType Integer)
    BasicType Real      -> (env, BasicType Real)
    BasicType Boolean   -> (env, BasicType Integer)
    _                   -> (addError env (NotArithmeticType op t, loc), Error)



----------------------------------------------------------------
-------------------- ENVIRONMENT MANAGEMENT --------------------
----------------------------------------------------------------
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

mergeForEnvs :: Env -> Env -> Env
mergeForEnvs env1 env2 = Env
  { symbols = symbols env1 -- NON unisco le symbol table: le variabili locali (es. di for) restano locali
  , parent  = parent env1
  , errors  = errors env1 ++ errors env2
  }



----------------------------------------------------------------
-------------------- ENRICHED AST FUNCTIONS --------------------
----------------------------------------------------------------
enrichConst :: SimpleConstant -> EnrichedConstant
enrichConst (ConstId s i)      = ConstId s (enrichIdentUnknown i)
enrichConst (ConstInt s n)     = ConstInt s n
enrichConst (ConstDouble s d)  = ConstDouble s d
enrichConst (ConstStr s)       = ConstStr s
enrichConst (ConstChar c)      = ConstChar c
enrichConst (ConstBool b)      = ConstBool b

enrichIdent :: SimpleIdent -> EnrichedType -> VarInfo -> FunInfo -> EnrichedIdent
enrichIdent (Ident name loc _ _ _) = Ident name loc

enrichIdentUnknown :: SimpleIdent -> EnrichedIdent
enrichIdentUnknown (Ident name loc _ _ _) = Ident name loc Error NoVarInfo NoFunInfo

enrichType :: SimpleType -> EnrichedType
enrichType (Array idxs t varInfo) = Array idxs (enrichType t) (VarInfo Nothing False (getRange idxs))
enrichType (Pointer t)            = Pointer (enrichType t)
enrichType (BasicType b)          = BasicType b
enrichType Error                  = Error

enrichLitExpr :: Literal -> EnrichedRExpr
enrichLitExpr lit = case lit of
    LiteralInteger _ -> Lit lit (BasicType Integer)
    LiteralDouble _  -> Lit lit (BasicType Real)
    LiteralChar _    -> Lit lit (BasicType Character)
    LiteralString _  -> Lit lit (BasicType String)
    Literal_true     -> Lit lit (BasicType Boolean)
    Literal_false    -> Lit lit (BasicType Boolean)



-----------------------------------------------------------
-------------------- SUPPORT FUNCTIONS --------------------
-----------------------------------------------------------

-- Extract the identifier name
getIdentName :: SimpleIdent -> String
getIdentName (Ident name _ _ _ _) = name

-- Extract the location of an identifier
getIdentLoc :: SimpleIdent -> Loc
getIdentLoc (Ident _ loc _ _ _) = loc

-- Extract the type information from an enriched identifier
getIdentType :: EnrichedIdent -> EnrichedType
getIdentType (Ident _ _ ty _ _) = ty

-- Infer the type of a constant
inferConstantType :: SimpleConstant -> SimpleType
inferConstantType (ConstInt _ _)    = BasicType Integer
inferConstantType (ConstDouble _ _) = BasicType Real
inferConstantType (ConstStr _)      = BasicType String
inferConstantType (ConstChar _)     = BasicType Character
inferConstantType (ConstBool _)     = BasicType Boolean
inferConstantType _                 = BasicType Void -- Not a constant

-- Extract type information from an RExpr
getRExprType :: EnrichedRExpr -> SimpleType
getRExprType (Binary _ _ _ ty)  = enrichedTypeToSimpleType ty
getRExprType (Unary _ _ ty)     = enrichedTypeToSimpleType ty
getRExprType (Lit _ ty)         = enrichedTypeToSimpleType ty
getRExprType (FCall ident _ _)  = enrichedTypeToSimpleType (getIdentType ident)
getRExprType (Ref _ _ ty)       = enrichedTypeToSimpleType ty
getRExprType (LExpr _ ty)       = enrichedTypeToSimpleType ty

-- Extract type information from a LExpr
getLExprType :: EnrichedLExpr -> SimpleType
getLExprType (LId _ ty)      = enrichedTypeToSimpleType ty
getLExprType (LArr _ _ ty _) = enrichedTypeToSimpleType ty
getLExprType (LDeref _ ty _) = enrichedTypeToSimpleType ty

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
    Pointer _   -> True
    _           -> False

isArray :: EnrichedLExpr -> Bool
isArray lexpr = case getLExprType lexpr of
    Array {}    -> True
    _           -> False

-- Extract the range from array indexType
getRange :: SimpleIndexType -> Maybe (Int, Int)
getRange (IndexRange c1 c2) =
    case (c1, c2) of
        (ConstInt _ n1, ConstInt _ n2) -> Just (fromInteger n1, fromInteger n2)
        _ -> Nothing

-- Extract integer value from a constant, resolving identifiers from the environment <-------------------------- TODO: other constant types
getConstVal :: Env -> SimpleConstant -> Integer
getConstVal _ (ConstInt sign n) = case sign of
    OptSignMinus -> negate n
    _            -> n
getConstVal env (ConstId sign ident) =
    let value = case lookupByName env (getIdentName ident) of
            Just (ConstVarEntry _ _ (Just (ConstInt _ val)) _ _, _) -> val
            _ -> 0 -- Not a known integer constant, default to 0
    in case sign of
        OptSignMinus -> negate value
        _            -> value
getConstVal _ _ = 0 -- Other constant types

-- True if the LExpr has been defined as a constant
isConstant :: EnrichedLExpr -> Bool
isConstant lexpr = case lexpr of
    LId ident _ -> case ident of
        Ident _ _ _ (VarInfo _ isConst _) _ -> isConst
        _ -> False
    LArr l _ _ _ -> isConstant l
    LDeref l _ _ -> isConstant l

isBoolean :: EnrichedRExpr -> Bool
isBoolean rexpr = case getRExprType rexpr of
    BasicType Boolean -> True
    _ -> False

getHeadingType :: EnrichedProcedureAndFunctionHeading -> SimpleType
getHeadingType (ProcHead _ _)        = BasicType Void
getHeadingType (FunHead _ _ retTy _) = retTy

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
        _ -> anyPathReturns ss -- return not guaranteed

-- True if the r-expression has Integer type
isInteger :: EnrichedRExpr -> Bool
isInteger rexpr = case getRExprType rexpr of
    BasicType Integer -> True
    _ -> False

enrichedTypeToSimpleType :: EnrichedType -> SimpleType
enrichedTypeToSimpleType (Array idx t _) = Array idx (enrichedTypeToSimpleType t) NoVarInfo
enrichedTypeToSimpleType (Pointer t) = Pointer (enrichedTypeToSimpleType t)
enrichedTypeToSimpleType (BasicType t) = BasicType t
enrichedTypeToSimpleType Error = Error

getOpAssignLoc :: OpAssignType -> Loc
getOpAssignLoc (OpAssignAdd loc) = loc
getOpAssignLoc (OpAssignSub loc) = loc
getOpAssignLoc (OpAssignMul loc) = loc

-----------------------------------------------------------
-------------------- PRINT ENVIRONMENT --------------------
-----------------------------------------------------------

printEnv :: Env -> String
printEnv env =
    let
        entries = Map.toList (symbols env)
        errs = errors env
    in "Symbol Table:" ++ showNames entries ++ "\n\nErrors:\n" ++ showErrors errs

showNames :: [(SimpleIdent, EnvEntry)] -> String
showNames [] = ""
showNames ((Ident name _ _ _ _, entry):s) = "\n\t" ++ show name ++ ": " ++ showEnvEntry entry ++ showNames s

showEnvEntry :: EnvEntry -> String
showEnvEntry entry = case entry of
    ConstVarEntry ty isConst constVal arrRange loc ->
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
    Array _ arrTy _     -> "Array of " ++ showType arrTy
    Pointer ptrTy       -> "Pointer to " ++ showType ptrTy
    BasicType Integer   -> "Integer"
    BasicType Real      -> "Real"
    BasicType Boolean   -> "Boolean"
    BasicType Character -> "Character"
    BasicType String    -> "String"
    BasicType Void      -> "Void"

showErrors :: [(String, Loc)] -> String
showErrors [] = "No errors"
showErrors errs =
  let uniqueSortedErrors = sortBy (comparing snd) (nub errs) -- (nub errs) removes duplicated errors caused by the mutual recursion mechanism
  in intercalate "\n" (map showError uniqueSortedErrors)
  where
    showError (msg, loc) = showLoc loc ++ ": " ++ msg