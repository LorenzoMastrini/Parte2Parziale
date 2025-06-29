module TACGen where

import TACabssynt
import TACMonad
import AbsLinguaggio
import Control.Monad.State
import Control.Monad (forM)
import Data.Functor.Identity (runIdentity)
import qualified Data.Map as Map



-- Execute the monad TACGen
runTACGen :: TACGen a -> a
runTACGen gen = runIdentity (evalStateT gen initialTACState)

-- TAC Generator for Program
genProgram :: EnrichedProgram -> ([TAC TACLabel], TACState)
genProgram (Prog ident params block loc) = 
    runTACGen (genBlock block)

--TAC Generator for Block
genBlock :: EnrichedBlock -> TACGen ([TAC TACLabel], TACState)
genBlock (Block constDefs varDecls procFunDecls compStmt) = do 
    constCodes <- mapM genConstDef constDefs -- Constants
    varCodes <- mapM genVarDecl varDecls -- Variables
    procFunResults <- mapM genProcedureFunctionDeclaration procFunDecls -- Procedures and functions
    let procFunCodes = concat procFunResults
    compStmtCode <- genCompoundStatement compStmt -- Compound statement
    st <- get
    return (concat constCodes ++ concat varCodes ++ procFunCodes ++ compStmtCode, st)

-- TAC Generator for ConstantDefinition
genConstDef :: EnrichedConstantDefinition -> TACGen [TAC TACLabel]
genConstDef (ConstDef ident@(Ident _ _ ty _ _) constVal) = do
    (code, valueAddr) <- genConstant constVal -- AST should store the value of the constant (not yet modified)
    -- Create the non-mutable variabile associated to the costant
    let var = ProgVar (TacProgVar (extractSimpleIdent ident) True ByVal (case ty of
                                                                            Just (BasicType bt) -> bt
                                                                            _ -> Integer)) (case ty of
                                                                                                Just t -> getTACType (Just t)
                                                                                                _ -> TACInteger)
    let assignCode = [TACAssign var valueAddr] -- Assignment
    return (code ++ assignCode)

-- TAC Generator for VariableDeclaration
genVarDecl :: EnrichedVariableDeclaration -> TACGen [TAC TACLabel]
genVarDecl (VarDecl idents ty) = do
    -- For each variable, generate an assignment to 0 (default)
    fmap concat $ forM idents $ \ident -> do
        let tacType = enrichedTypeToTACType ty
        let var = ProgVar (TacProgVar (extractSimpleIdent ident) True ByVal (enrichedTypeToBasicType ty)) tacType
        -- Choose default value based on type
        let defaultLit = case tacType of
                TACInteger   -> LiteralInteger 0
                TACReal      -> LiteralDouble 0.0
                TACBoolean   -> Literal_false
                TACCharacter -> LiteralChar '\0'
                TACAddr      -> LiteralInteger 0 -- Default value for addresses (null pointer)
                _            -> LiteralInteger 0 -- Error
        tmp <- newtemp tacType
        let code = [TACAssign tmp (TACLit defaultLit tacType), TACAssign var tmp]
        return code

-- TAC Generator for ProcedureAndFunctionDeclaration
genProcedureFunctionDeclaration :: EnrichedProcedureAndFunctionDeclaration -> TACGen [TAC TACLabel]
genProcedureFunctionDeclaration (ProcFunDecl heading block) = do
    let (ident, params, returnType) = case heading of
            ProcHead i p -> (i, p, Nothing)
            FunHead i p t loc -> (i, p, Just t)
    entryLabel <- newlabel
    paramInitCode <- genParamInit params
    (bodyCode, _) <- genBlock block
    let endCode = case returnType of
            Just _ -> []
            Nothing -> [TACReturn]
    return ([TACAddLabel entryLabel] ++ paramInitCode ++ bodyCode ++ endCode)

-- TAC Generator for RExpr
genRExpr :: EnrichedRExpr -> TACGen ([TAC TACLabel], XAddr)
genRExpr (Binary e1 op e2 t) = do
    case op of
        BinaryAnd loc -> genShortCircuitAnd e1 e2 (getTACType t)
        BinaryOr loc -> genShortCircuitOr e1 e2 (getTACType t)
        _ -> do
            (code1, xaddr1) <- genRExpr e1
            (code2, xaddr2) <- genRExpr e2
            let addr1 = case xaddr1 of Addr a -> a; _ -> error "Non-Address operand"
            let addr2 = case xaddr2 of Addr a -> a; _ -> error "Non-Address operand"
            addr <- newtemp (getTACType t)
            let code = code1 ++ code2 ++ [TACBinary addr op (getBasicType t) addr1 addr2]
            return (code, Addr addr)

genRExpr (Unary op e1 t) = do
    (code1, xaddr1) <- genRExpr e1
    let addr1 = case xaddr1 of Addr a -> a; _ -> error "Non-Address operand"
    addr <- newtemp (getTACType t)
    let code = code1 ++ [TACUnary addr op (getBasicType t) addr1]
    return (code, Addr addr)

genRExpr (Lit lit ty) = do
    addr <- newtemp (getTACType ty)
    let code = [TACAssign addr (TACLit lit (getTACType ty))]
    return (code, Addr addr)

genRExpr (FCall ident@(Ident _ _ ty _ (FunInfo retTy params)) args loc) = do
    let paramModes = extractParamModes params
    genRExprFCall ident args paramModes (getTACType ty)

genRExpr (FCall ident args loc) = do
    -- When funInfo is NoFunInfo (error) do nothing
    return ([], Addr (Temporary 0 TACInteger)) -- Temporary not used

genRExpr (Ref unary rexpr ty) = do
    (code, xaddr) <- genRExpr rexpr
    case xaddr of
        Addr addr -> do
            resultAddr <- newtemp (getTACType ty)
            let code' = case addr of
                    ProgVar v _ -> code ++ [TACAddrOf resultAddr v]
                    _ -> code ++ [TACAssign resultAddr addr] -- fallback: directly assign the address
            return (code', Addr resultAddr)
        _ -> do
            resultAddr <- newtemp (getTACType ty) -- fallback: not a simple address, generate a temporary
            return (code, Addr resultAddr)

genRExpr (LExpr le ty) = do
    (code, xaddr) <- genLExpr le
    case xaddr of
        Addr addr -> return (code, Addr addr)
        ArrayAddr{base, offset} -> do
            let t = addrT base
            tmp <- newtemp t
            let code' = code ++ [TACIndexedLoad tmp (case base of ProgVar v _ -> v; _ -> error "base is not ProgVar") offset]
            return (code', Addr tmp)
        RefAddr addr -> do
            let t = addrT addr
            tmp <- newtemp t
            let code' = code ++ [TACDerefRead tmp addr]
            return (code', Addr tmp)

-- TAC Generator for LExpr 
genLExpr :: EnrichedLExpr -> TACGen ([TAC TACLabel], XAddr)
genLExpr (LId ident ty) = do 
    let addr = ProgVar (TacProgVar (extractSimpleIdent ident) True ByVal (getBasicType ty)) (getTACType ty)
    return ([], Addr addr)

genLExpr (LArr e1 [e2] ty loc) = do
    (codeBase, xbase) <- genLExpr e1
    (codeIdx, xidx) <- genRExpr e2
    let idxAddr = case xidx of Addr a -> a; _ -> error "Non-Address index"
    case xbase of
        Addr baseAddr -> do
            let xaddr = ArrayAddr { base = baseAddr, offset = idxAddr }
            return (codeBase ++ codeIdx, xaddr)
        ArrayAddr{base, offset} -> do
            -- Multi-dimentional arrays: calculate total offset
            dimAddr <- newtemp TACInteger
            let dimCode = [TACAssign dimAddr (TACLit (LiteralInteger 2) TACInteger)]
            totalOffsetAddr <- newtemp TACInteger
            let mulCode = [TACBinary totalOffsetAddr (BinaryMul (Loc 0 0)) Integer offset dimAddr]
            finalOffsetAddr <- newtemp TACInteger
            let addCode = [TACBinary finalOffsetAddr (BinaryPlus (Loc 0 0)) Integer totalOffsetAddr idxAddr]
            let xaddr = ArrayAddr { base = base, offset = finalOffsetAddr }
            return (codeBase ++ codeIdx ++ dimCode ++ mulCode ++ addCode, xaddr)
        RefAddr baseAddr -> do
            -- Dereferenced pointers: create an ArrayAddr with the base of the pointer
            let xaddr = ArrayAddr { base = baseAddr, offset = idxAddr }
            return (codeBase ++ codeIdx, xaddr)

genLExpr (LDeref e1 ty loc) = do
    (code1, xaddr) <- genLExpr e1
    case xaddr of
        Addr addr -> return (code1, RefAddr addr)
        RefAddr addr -> do
            -- if dereferencing a RefAddr, generate new temporary and make a TACDerefRead
            tmp <- newtemp (case ty of Just t' -> getTACType (Just t'); Nothing -> TACInteger)
            let code' = code1 ++ [TACDerefRead tmp addr]
            return (code', Addr tmp)
        _ -> do
            -- Error: dereference of a not-address
            tmp <- newtemp (case ty of Just t' -> getTACType (Just t'); Nothing -> TACInteger)
            return (code1, Addr tmp)

-- TAC Generator for LExpr 
genLExprTarget :: EnrichedLExpr -> TACGen ([TAC TACLabel], XAddr)
genLExprTarget (LId ident t) = do 
    let addr = ProgVar (TacProgVar (extractSimpleIdent ident) True ByVal (getBasicType t)) (getTACType t)
    return ([], Addr addr)

genLExprTarget (LArr e1 [e2] ty loc) = do
    (codeBase, xbase) <- genLExpr e1
    (codeIdx, xidx) <- genRExpr e2
    let idxAddr = case xidx of Addr a -> a; _ -> error "Indice non Address"
    case xbase of
        Addr baseAddr -> do
            let xaddr = ArrayAddr { base = baseAddr, offset = idxAddr }
            return (codeBase ++ codeIdx, xaddr)
        ArrayAddr{base, offset} -> do
            -- Multi-dimentional arrays: calculate total offset
            dimAddr <- newtemp TACInteger
            let dimCode = [TACAssign dimAddr (TACLit (LiteralInteger 2) TACInteger)]
            totalOffsetAddr <- newtemp TACInteger
            let mulCode = [TACBinary totalOffsetAddr (BinaryMul (Loc 0 0)) Integer offset dimAddr]
            finalOffsetAddr <- newtemp TACInteger
            let addCode = [TACBinary finalOffsetAddr (BinaryPlus (Loc 0 0)) Integer totalOffsetAddr idxAddr]
            let xaddr = ArrayAddr { base = base, offset = finalOffsetAddr }
            return (codeBase ++ codeIdx ++ dimCode ++ mulCode ++ addCode, xaddr)
        RefAddr baseAddr -> do
            -- Dereferenced pointers: create an ArrayAddr with the base of the pointer
            let xaddr = ArrayAddr { base = baseAddr, offset = idxAddr }
            return (codeBase ++ codeIdx, xaddr)

genLExprTarget (LDeref e1 ty loc) = do
    (code1, xaddr) <- genLExpr e1
    case xaddr of
        Addr addr -> return (code1, RefAddr addr)
        RefAddr addr -> do
            -- if dereferencing a RefAddr, generate new temporary and make a TACDerefRead
            tmp <- newtemp (case ty of Just t' -> getTACType (Just t'); Nothing -> TACInteger)
            let code' = code1 ++ [TACDerefRead tmp addr]
            return (code', Addr tmp)
        _ -> do
            -- Error: dereference of a not-address
            tmp <- newtemp (case ty of Just t' -> getTACType (Just t'); Nothing -> TACInteger)
            return (code1, Addr tmp)

-- TAC Generator for CompoundStatement
genCompoundStatement :: EnrichedCompoundStatement -> TACGen [TAC TACLabel]
genCompoundStatement (CompStmt stmts) = do
    codes <- mapM genStatement stmts
    let result = concat codes
    return result

-- TAC Generator for Statement
genStatement :: EnrichedStatement -> TACGen [TAC TACLabel]
genStatement (Assign lexpr rexpr ty loc) = do
    (code1, xaddr1) <- genLExprTarget lexpr
    (code2, xaddr2) <- genRExpr rexpr
    case (xaddr1, xaddr2) of
        (Addr addr1, Addr addr2) -> return (code1 ++ code2 ++ [TACAssign addr1 addr2])
        (ArrayAddr{base, offset}, Addr addr2) -> return (code1 ++ code2 ++ [TACIndexedStore (case base of ProgVar v _ -> v; _ -> error "Base is not ProgVar") offset addr2])
        (RefAddr addr1, Addr addr2) -> return (code1 ++ code2 ++ [TACDerefWrite addr1 addr2])
        _ -> error "Assignment not supported"

genStatement (RExprStmt re t) = do
    (code, _) <- genRExpr re
    return code

genStatement (ReturnStmt re ty loc) = do
    (code, xaddr) <- genRExpr re
    let addr = extractAddr xaddr
    return (code ++ [TACReturnVal addr])

genStatement (Stmts compStmt) = genCompoundStatement compStmt

-- "if" statement
genStatement (If cond thenStmt optElse t) = do
    (condCode, xcondAddr) <- genRExpr cond
    let condAddr = extractAddr xcondAddr
    thenCode <- genStatement thenStmt
    
    case optElse of
        NoElse -> do
            -- if-then (whithout else)
            endLabel <- newlabel
            return (condCode ++ 
                    [TACIfFalse condAddr endLabel] ++ 
                    thenCode ++ 
                    [TACAddLabel endLabel])
        
        Else elseStmt -> do
            -- if-then-else
            elseCode <- genStatement elseStmt
            endLabel <- newlabel
            elseLabel <- newlabel
            
            return (condCode ++ 
                    [TACIfFalse condAddr elseLabel] ++ 
                    thenCode ++ 
                    [TACGoto endLabel] ++ 
                    [TACAddLabel elseLabel] ++ 
                    elseCode ++ 
                    [TACAddLabel endLabel])

-- "while" statement
genStatement (While cond body t) = do
    loopLabel <- newlabel
    endLabel <- newlabel
    
    -- Push of break/continue labels
    pushBreakLabel endLabel
    pushContinueLabel loopLabel
    
    (condCode, xcondAddr) <- genRExpr cond
    let condAddr = extractAddr xcondAddr
    bodyCode <- genStatement body
    
    -- Pop labels
    _ <- popBreakLabel
    _ <- popContinueLabel
    
    return ([TACAddLabel loopLabel] ++ 
            condCode ++ 
            [TACIfFalse condAddr endLabel] ++ 
            bodyCode ++ 
            [TACGoto loopLabel] ++ 
            [TACAddLabel endLabel])

-- "do-while" statement (repeat-until)
genStatement (DoWhile body cond t) = do
    loopLabel <- newlabel
    endLabel <- newlabel
    
    -- Push of break/continue labels
    pushBreakLabel endLabel
    pushContinueLabel loopLabel
    
    bodyCode <- genStatement body
    (condCode, xcondAddr) <- genRExpr cond
    let condAddr = extractAddr xcondAddr
    
    -- Pop labels
    _ <- popBreakLabel
    _ <- popContinueLabel
    
    return ([TACAddLabel loopLabel] ++ 
            bodyCode ++ 
            condCode ++ 
            [TACIfFalse condAddr endLabel] ++ 
            [TACGoto loopLabel] ++ 
            [TACAddLabel endLabel])

-- "break" statement
genStatement (BreakStmt loc) = do
    mBreakLabel <- tryPopBreakLabel
    case mBreakLabel of
        Just breakLabel -> return [TACGoto breakLabel]
        Nothing -> return [] -- Not inside a loop: don't generate anyhing

-- "continue" statement
genStatement (ContinueStmt loc) = do
    mContinueLabel <- tryPopContinueLabel
    case mContinueLabel of
        Just continueLabel -> return [TACGoto continueLabel]
        Nothing -> return [] -- Not inside a loop: don't generate anyhing

-- Generate costants
genConstant :: EnrichedConstant -> TACGen ([TAC TACLabel], Address)
genConstant (ConstInt sign val) = do
    addr <- newtemp TACInteger
    let intVal = case sign of
                    NoOptSign -> val
                    OptSignPlus -> val
                    OptSignMinus -> -val
    let code = [TACAssign addr (TACLit (LiteralInteger intVal) TACInteger)]
    return (code, addr)
genConstant (ConstDouble sign val) = do
    addr <- newtemp TACReal
    let doubleVal = case sign of
                        NoOptSign -> val
                        OptSignPlus -> val
                        OptSignMinus -> -val
    let code = [TACAssign addr (TACLit (LiteralDouble doubleVal) TACReal)]
    return (code, addr)
genConstant (ConstStr str) = do
    addr <- newtemp TACAddr
    strAddr <- newStringAddress str
    let code = [TACAssign addr (TACLit (LiteralString str) TACAddr)]
    return (code, addr)
genConstant (ConstId sign ident@(Ident _ _ ty _ _)) = do
    -- AST should store the value of the consant (not yet modified)
    let constType = getTACType ty
    addr <- newtemp constType
    let progVar = TacProgVar (extractSimpleIdent ident) True ByVal Integer
    let code = [TACAssign addr (ProgVar progVar constType)]
    return (code, addr)
genConstant (ConstChar c) = do
    addr <- newtemp TACCharacter
    let code = [TACAssign addr (TACLit (LiteralChar c) TACCharacter)]
    return (code, addr)
genConstant (ConstBool b) = do
    addr <- newtemp TACBoolean
    let code = [TACAssign addr (TACLit (if b then Literal_true else Literal_false) TACBoolean)]
    return (code, addr)

-- Short-circuit evaluation for "and"
genShortCircuitAnd :: EnrichedRExpr -> EnrichedRExpr -> TACType -> TACGen ([TAC TACLabel], XAddr)
genShortCircuitAnd e1 e2 t = do
    resultAddr <- newtemp t
    falseLabel <- newlabel
    endLabel <- newlabel
    -- Generate first operand
    (code1, addr1) <- genRExpr e1
    let addr1' = case addr1 of Addr a -> a; _ -> error "Non-Address AND condition"
    -- If first is false, jump to the result (false)
    let shortcutCode = code1 ++ [TACIfFalse addr1' falseLabel]
    -- Generate second operand only if the first is true
    (code2, addr2) <- genRExpr e2
    let addr2' = case addr2 of Addr a -> a; _ -> error "Non-Address AND condition"
    -- Id the second is false, jump to the result (false)
    let secondCheck = code2 ++ [TACIfFalse addr2' falseLabel]
    -- If get here, they are both true
    let trueCode = [TACAssign resultAddr (TACLit Literal_true TACBoolean), TACGoto endLabel]
    -- Code for the "false" result
    let falseCode = [TACAddLabel falseLabel, TACAssign resultAddr (TACLit Literal_false TACBoolean)]
    return (shortcutCode ++ secondCheck ++ trueCode ++ falseCode ++ [TACAddLabel endLabel], Addr resultAddr)

-- Short-circuit evaluation for "or"
genShortCircuitOr :: EnrichedRExpr -> EnrichedRExpr -> TACType -> TACGen ([TAC TACLabel], XAddr)
genShortCircuitOr e1 e2 t = do
    resultAddr <- newtemp t
    trueLabel <- newlabel
    endLabel <- newlabel
    (code1, addr1) <- genRExpr e1
    let addr1' = case addr1 of Addr a -> a; _ -> error "Non-Address OR condition"
    let shortcutCode = code1 ++ [TACIf addr1' trueLabel]
    (code2, addr2) <- genRExpr e2
    let addr2' = case addr2 of Addr a -> a; _ -> error "Non-Address OR condition"
    let secondCheck = code2 ++ [TACIf addr2' trueLabel]
    let falseCode = [TACAssign resultAddr (TACLit Literal_false TACBoolean), TACGoto endLabel]
    let trueCode = [TACAddLabel trueLabel, TACAssign resultAddr (TACLit Literal_true TACBoolean)]
    return (shortcutCode ++ secondCheck ++ falseCode ++ trueCode ++ [TACAddLabel endLabel], Addr resultAddr)

genParamInit :: [EnrichedFormalParameters] -> TACGen [TAC TACLabel]
genParamInit params = fmap (concat . concat) $ forM params $ \(FormParam mode idents typ) ->
    forM idents $ \ident -> do
        let var = ProgVar (TacProgVar (extractSimpleIdent ident) True mode (enrichedTypeToBasicType typ)) (enrichedTypeToTACType typ)
        case mode of
            ByVal -> do
                tmp <- newtemp (enrichedTypeToTACType typ)
                return [TACParam tmp, TACAssign var tmp]
            ByRef -> do
                tmp <- newtemp (enrichedTypeToTACType typ)
                return [TACParam tmp, TACAssign var tmp]
            ByValRes -> do
                tmp <- newtemp (enrichedTypeToTACType typ)
                return [TACParam tmp, TACAssign var tmp]

-- Extract the flatten list of the formal parameters modality
extractParamModes :: [SimpleFormalParameters] -> [OptModality]
extractParamModes params = concatMap (\(FormParam m ids _) -> replicate (length ids) m) params

genRExprFCall :: EnrichedIdent -> [EnrichedRExpr] -> [OptModality] -> TACType -> TACGen ([TAC TACLabel], XAddr)
genRExprFCall ident args paramModes t = do
    paramTACs <- forM (zip args paramModes) $ \(arg, mode) ->
        case mode of
            ByVal -> do
                (code, xaddr) <- genRExpr arg
                let addr = case xaddr of Addr a -> a; _ -> error "Parametro ByVal non Address"
                return (code, TACParam addr)
            ByRef -> case arg of
                LExpr le t -> do
                    (code, xaddr) <- genLExprTarget le
                    let addr = case xaddr of Addr a -> a; _ -> error "Parametro ByRef non Address"
                    return (code, TACParam addr)
                _ -> do
                    -- Errore: ByRef parameter (not LExpr)
                    return ([], TACParam (Temporary 0 TACInteger)) -- Temporary not used
            ByValRes -> case arg of
                LExpr le t -> do
                    (code, xaddr) <- genLExprTarget le
                    let addr = case xaddr of Addr a -> a; _ -> error "Parametro ByValRes non Address"
                    return (code, TACParam addr)
                _ -> do
                    -- Errore: ByRef parameter (not LExpr)
                    return ([], TACParam (Temporary 0 TACInteger)) -- Temporary not used
    let (codes, tacParams) = unzip paramTACs
    addr <- newtemp t
    let codeCall = concat codes ++ tacParams ++ [TACFCall addr 0 (length args)]
    return (codeCall, Addr addr)



-----------------------------------------------------------
-------------------- SUPPORT FUNCTIONS --------------------
-----------------------------------------------------------

-- Extract SimpleIdent form EnrichedIdent
extractSimpleIdent :: EnrichedIdent -> SimpleIdent
extractSimpleIdent (Ident name loc _ _ _) = Ident name loc () () ()

-- Extract Address from XAddr
extractAddr :: XAddr -> Address
extractAddr (Addr addr) = addr
extractAddr _ = error "Expected Address but got different XAddr type"

-- Convert EnrichedType to TACType
enrichedTypeToTACType :: EnrichedType -> TACType
enrichedTypeToTACType (BasicType bt) = basicTypeToTACType bt
enrichedTypeToTACType (Pointer _)    = TACAddr
enrichedTypeToTACType (Array _ _)    = TACAddr

-- Convert BasicType to TACType
basicTypeToTACType :: BasicType -> TACType
basicTypeToTACType Integer    = TACInteger
basicTypeToTACType Real       = TACReal
basicTypeToTACType Character  = TACCharacter
basicTypeToTACType Boolean    = TACBoolean
basicTypeToTACType String     = TACAddr
basicTypeToTACType Void       = TACError

-- Convert EnrichedType to BasicType
enrichedTypeToBasicType :: EnrichedType -> BasicType
enrichedTypeToBasicType (BasicType bt) = bt
enrichedTypeToBasicType _ = Void