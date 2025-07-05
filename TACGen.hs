module TACGen where

import TACabssynt
import TACMonad
import AbsLinguaggio
import Control.Monad.State
import Control.Monad (forM, forM_)
import Data.Functor.Identity (runIdentity)
import qualified Data.Map as Map
import Debug.Trace (traceShow)



-- Execute the monad TACGen
runTACGen :: TACGen a -> a
runTACGen gen = runIdentity (evalStateT gen initialTACState)

-- TAC Generator for Program
genProgram :: EnrichedProgram -> ([TAC TACLabel], TACState)
genProgram (Prog ident params block loc) =
  runTACGen $ do
    (code, st) <- genBlock block
    codeOutOfBound <- genOutOfBound
    return (code ++ codeOutOfBound, st)

--TAC Generator for Block
genBlock :: EnrichedBlock -> TACGen ([TAC TACLabel], TACState)
genBlock (Block constDefs varDecls procFunDecls compStmt) = do 
    constCodes <- mapM genConstDef constDefs -- Constants
    varCodes <- mapM genVarDecl varDecls -- Variables

    -- PRIMO PASSO: inserisci tutte le label delle funzioni nella mappa
    forM_ procFunDecls $ \(ProcFunDecl heading _) -> do
        let ident = case heading of
                        ProcHead i _ -> i
                        FunHead i _ _ _ -> i
        entryLabel <- newlabel
        modify $ \st -> st { functionLabels = Map.insert (getIdentName ident) entryLabel (functionLabels st) }
        
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
                                                                            (BasicType bt) -> bt
                                                                            _ -> Integer)) (getTACType ty)
    let assignCode = [TACAssign var valueAddr] -- Assignment
    return (code ++ assignCode)

-- TAC Generator for VariableDeclaration
genVarDecl :: EnrichedVariableDeclaration -> TACGen [TAC TACLabel]
genVarDecl (VarDecl idents ty) = do
    -- For each variable, generate an assignment to 0 (default)
    fmap concat $ forM idents $ \ident -> do
        let tacType = typeToTACType ty
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
    -- Get the label associated with the identifier
    st <- get
    let entryLabel = (functionLabels st) Map.! (getIdentName ident)
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
        _ -> error "genRExpr (LExpr): unexpected XAddr type"

-- TAC Generator for LExpr (for reading values)
genLExpr :: EnrichedLExpr -> TACGen ([TAC TACLabel], XAddr)
genLExpr (LId ident ty) = do 
    let addr = ProgVar (TacProgVar (extractSimpleIdent ident) True ByVal (getBasicType ty)) (getTACType ty)
    return ([], Addr addr)

genLExpr (LArr e1 e2 ty loc) = do
    (codeBase, xbase) <- genLExpr e1
    (codeIdx, xidx) <- genRExpr e2

    let e1Ty = getLExprType e1
    let lowerBound = getArrayLowerBound e1Ty
    let upperBound = getArraySize e1Ty
    let justLower = case lowerBound of Just a -> a; Nothing -> error "array has no lower bound"
    let justUpper = case upperBound of Just a -> a - 1; Nothing -> error "array has no upper bound"

    codeBound <- arrayBoundCheck xidx justLower justUpper

    let idxAddr = case xidx of Addr a -> a; _ -> error "Non-Address index"
    case xbase of
        Addr baseAddr -> do
            -- Load the value from array
            tmp <- newtemp (getTACType ty)
            let code' = codeBase ++ codeIdx ++ codeBound ++ [TACIndexedLoad tmp (case baseAddr of ProgVar v _ -> v; _ -> error "base is not ProgVar") idxAddr]
            return (code', Addr tmp)
        ArrayAddr{base, offset} -> do
            -- Multi-dimentional arrays: calculate total offset
            dimAddr <- newtemp TACInteger
            let dimCode = [TACAssign dimAddr (TACLit (LiteralInteger 2) TACInteger)]
            totalOffsetAddr <- newtemp TACInteger
            let mulCode = [TACBinary totalOffsetAddr (BinaryMul (Loc 0 0)) Integer offset dimAddr]
            finalOffsetAddr <- newtemp TACInteger
            let addCode = [TACBinary finalOffsetAddr (BinaryPlus (Loc 0 0)) Integer totalOffsetAddr idxAddr]
            -- Load the value from the calculated address
            tmp <- newtemp (getTACType ty)
            let loadCode = [TACIndexedLoad tmp (case base of ProgVar v _ -> v; _ -> error "base is not ProgVar") finalOffsetAddr]
            return (codeBase ++ codeIdx ++ codeBound ++ dimCode ++ mulCode ++ addCode ++ loadCode, Addr tmp)
        RefAddr baseAddr -> do
            -- Load the value from the dereferenced pointer
            tmp <- newtemp (getTACType ty)
            let code' = codeBase ++ codeIdx ++ codeBound ++ [TACIndexedLoad tmp (case baseAddr of ProgVar v _ -> v; _ -> error "base is not ProgVar") idxAddr]
            return (code', Addr tmp)

genLExpr (LDeref e1 ty loc) = do
    (code1, xaddr) <- genLExpr e1
    case xaddr of
        Addr addr -> do
            -- Load the value pointed to
            tmp <- newtemp (case ty of t' -> getTACType t')
            let code' = code1 ++ [TACDerefRead tmp addr]
            return (code', Addr tmp)
        RefAddr addr -> do
            -- if dereferencing a RefAddr, generate new temporary and make a TACDerefRead
            tmp <- newtemp (case ty of t' -> getTACType t')
            let code' = code1 ++ [TACDerefRead tmp addr]
            return (code', Addr tmp)
        _ -> do
            -- Error: dereference of a not-address
            tmp <- newtemp (case ty of t' -> getTACType t')
            return (code1, Addr tmp)

-- TAC Generator for LExpr (for writing targets)
genLExprTarget :: EnrichedLExpr -> TACGen ([TAC TACLabel], XAddr)
genLExprTarget (LId ident t) = do 
    let addr = ProgVar (TacProgVar (extractSimpleIdent ident) True ByVal (getBasicType t)) (getTACType t)
    return ([], Addr addr)

genLExprTarget (LArr e1 e2 ty loc) = do
    (codeBase, xbase) <- genLExpr e1
    (codeIdx, xidx) <- genRExpr e2

    let e1Ty = getLExprType e1
    let lowerBound = getArrayLowerBound e1Ty
    let upperBound = getArraySize e1Ty
    let justLower = case lowerBound of Just a -> a; Nothing -> error "array has no lower bound"
    let justUpper = case upperBound of Just a -> a - 1; Nothing -> error "array has no upper bound"

    codeBound <- arrayBoundCheck xidx justLower justUpper

    let idxAddr = case xidx of Addr a -> a; _ -> error "Non-Address index"
    case xbase of
        Addr baseAddr -> do
            -- Return the target address for writing (don't load the value)
            let xaddr = ArrayAddr { base = baseAddr, offset = idxAddr }
            return (codeBase ++ codeIdx ++ codeBound, xaddr)
        ArrayAddr{base, offset} -> do
            -- Multi-dimentional arrays: calculate total offset
            dimAddr <- newtemp TACInteger
            let dimCode = [TACAssign dimAddr (TACLit (LiteralInteger 2) TACInteger)]
            totalOffsetAddr <- newtemp TACInteger
            let mulCode = [TACBinary totalOffsetAddr (BinaryMul (Loc 0 0)) Integer offset dimAddr]
            finalOffsetAddr <- newtemp TACInteger
            let addCode = [TACBinary finalOffsetAddr (BinaryPlus (Loc 0 0)) Integer totalOffsetAddr idxAddr]
            -- Return the target address for writing (don't load the value)
            let xaddr = ArrayAddr { base = base, offset = finalOffsetAddr }
            return (codeBase ++ codeIdx ++ codeBound ++ dimCode ++ mulCode ++ addCode, xaddr)
        RefAddr baseAddr -> do
            -- Return the target address for writing (don't load the value)
            let xaddr = ArrayAddr { base = baseAddr, offset = idxAddr }
            return (codeBase ++ codeIdx ++ codeBound, xaddr)

genLExprTarget (LDeref e1 ty loc) = do
    (code1, xaddr) <- genLExpr e1
    case xaddr of
        Addr addr -> do
            -- Return the target address for writing (don't load the value)
            return (code1, RefAddr addr)
        RefAddr addr -> do
            -- Return the target address for writing (don't load the value)
            return (code1, RefAddr addr)
        _ -> do
            -- Error: dereference of a not-address
            tmp <- newtemp (case ty of t' -> getTACType t')
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

genStatement (ReturnNoExpr ty loc) = do
    return [TACReturn]

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
        Nothing -> return [] -- Not inside a loop: don't generate anything

-- "continue" statement
genStatement (ContinueStmt loc) = do
    mContinueLabel <- tryPopContinueLabel
    case mContinueLabel of
        Just continueLabel -> return [TACGoto continueLabel]
        Nothing -> return [] -- Not inside a loop: don't generate anything

-- "for" statement (for variable := start to end do statement)
genStatement (ForTo var start end body loc) = do
    -- Generate labels for the loop
    loopLabel <- newlabel
    endLabel <- newlabel
    bodyLabel <- newlabel
    
    -- Push break/continue labels
    pushBreakLabel endLabel
    pushContinueLabel loopLabel
    
    -- Generate code for start and end expressions
    (startCode, xstartAddr) <- genRExpr start
    (endCode, xendAddr) <- genRExpr end
    let startAddr = case xstartAddr of Addr a -> a; _ -> error "ForTo: start not Address"
    let endAddr = case xendAddr of Addr a -> a; _ -> error "ForTo: end not Address"
    
    -- Generate code for the loop variable (LHS of assignment)
    (varCode, xvarAddr) <- genLExprTarget var
    let varAddr = case xvarAddr of Addr a -> a; _ -> error "ForTo: variable not Address"
    
    -- Generate code for the loop body
    bodyCode <- genStatement body
    
    -- Pop labels
    _ <- popBreakLabel
    _ <- popContinueLabel
    
    -- Generate the complete for loop code
    let initCode = startCode ++ endCode ++ varCode ++ [TACAssign varAddr startAddr]
    let conditionCode = [TACIfRel varAddr (BinaryGt loc) endAddr endLabel]
    let incrementCode = [TACBinary varAddr (BinaryPlus loc) (tacTypeToBasicType (addrT varAddr)) varAddr (TACLit (LiteralInteger 1) TACInteger)]
    
    return (initCode ++ 
            [TACAddLabel loopLabel] ++ 
            conditionCode ++ 
            [TACAddLabel bodyLabel] ++ 
            bodyCode ++ 
            incrementCode ++ 
            [TACGoto loopLabel] ++ 
            [TACAddLabel endLabel])

-- Assignment operation statement
genStatement (OpAssign lexpr opAssignType rexpr ty) = do
    (codeL, xaddrL) <- genLExprTarget lexpr
    (codeR, xaddrR) <- genRExpr rexpr
    let rhsAddr = case xaddrR of Addr a -> a; _ -> error "OpAssign: RHS not Address"
    
    case xaddrL of
        Addr lhsAddr -> do
            -- Simple variable assignment: load current value, apply operation, store result
            tmpL <- newtemp (addrT lhsAddr)
            let loadCode = [TACAssign tmpL lhsAddr]
            tmpRes <- newtemp (addrT lhsAddr)
            let binOp = case opAssignType of
                    OpAssignAdd _ -> BinaryPlus (Loc 0 0)
                    OpAssignSub _ -> BinaryMinus (Loc 0 0)
                    OpAssignMul _ -> BinaryMul (Loc 0 0)
            let binCode = [TACBinary tmpRes binOp (tacTypeToBasicType (addrT lhsAddr)) tmpL rhsAddr]
            let storeCode = [TACAssign lhsAddr tmpRes]
            return (codeL ++ codeR ++ loadCode ++ binCode ++ storeCode)
            
        ArrayAddr{base, offset} -> do
            -- Array element assignment: load current value, apply operation, store result
            tmpL <- newtemp (addrT base)
            let loadCode = [TACIndexedLoad tmpL (case base of ProgVar v _ -> v; _ -> error "base is not ProgVar") offset]
            tmpRes <- newtemp (addrT base)
            let binOp = case opAssignType of
                    OpAssignAdd _ -> BinaryPlus (Loc 0 0)
                    OpAssignSub _ -> BinaryMinus (Loc 0 0)
                    OpAssignMul _ -> BinaryMul (Loc 0 0)
            let binCode = [TACBinary tmpRes binOp (tacTypeToBasicType (addrT base)) tmpL rhsAddr]
            let storeCode = [TACIndexedStore (case base of ProgVar v _ -> v; _ -> error "base is not ProgVar") offset tmpRes]
            return (codeL ++ codeR ++ loadCode ++ binCode ++ storeCode)
            
        RefAddr addr -> do
            -- Pointer dereference assignment: load current value, apply operation, store result
            tmpL <- newtemp (addrT addr)
            let loadCode = [TACDerefRead tmpL addr]
            tmpRes <- newtemp (addrT addr)
            let binOp = case opAssignType of
                    OpAssignAdd _ -> BinaryPlus (Loc 0 0)
                    OpAssignSub _ -> BinaryMinus (Loc 0 0)
                    OpAssignMul _ -> BinaryMul (Loc 0 0)
            let binCode = [TACBinary tmpRes binOp (tacTypeToBasicType (addrT addr)) tmpL rhsAddr]
            let storeCode = [TACDerefWrite addr tmpRes]
            return (codeL ++ codeR ++ loadCode ++ binCode ++ storeCode)


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
        let var = ProgVar (TacProgVar (extractSimpleIdent ident) True mode (enrichedTypeToBasicType typ)) (typeToTACType typ)
        case mode of
            ByVal -> do
                tmp <- newtemp (typeToTACType typ)
                return [TACParam tmp, TACAssign var tmp]
            ByRef -> do
                tmp <- newtemp (typeToTACType typ)
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
                let addr = case xaddr of Addr a -> a; _ -> error "Parameter ByVal not Address"
                return (code, TACParam addr)
            ByRef -> case arg of
                LExpr le t -> do
                    (code, xaddr) <- genLExprTarget le
                    let addr = case xaddr of Addr a -> a; _ -> error "Parameter ByRef not Address"
                    return (code, TACParam addr)
                _ -> do
                    -- Errore: ByRef parameter (not LExpr)
                    return ([], TACParam (Temporary 0 TACInteger)) -- Temporary not used
    let (codes, tacParams) = unzip paramTACs
    addr <- newtemp t
    -- Get the label of the procedure/function
    funLabel <- gets (Map.findWithDefault (error "Function label not found") (getIdentName ident) . functionLabels)
    let codeCall = concat codes ++ tacParams ++ [TACFCall addr funLabel (length args)]
    return (codeCall, Addr addr)



-----------------------------------------------------------
-------------------- SUPPORT FUNCTIONS --------------------
-----------------------------------------------------------


arrayBoundCheck :: XAddr -> Int -> Int -> TACGen [TAC TACLabel]
arrayBoundCheck idxOperand lowerBound upperBound = do
  let loc = Loc 0 0 
  let lowerLit = TACLit (LiteralInteger (fromIntegral lowerBound)) TACInteger
  let upperLit = TACLit (LiteralInteger (fromIntegral upperBound)) TACInteger

  checkLower <- newtemp TACBoolean
  checkUpper <- newtemp TACBoolean
  isValid    <- newtemp TACBoolean

  let xlower = [TACBinary checkLower (BinaryGte loc) Integer (extractAddr idxOperand) lowerLit]
  let xupper = [TACBinary checkUpper (BinaryLte loc) Integer (extractAddr idxOperand) upperLit]
  let xvalid = [TACBinary isValid (BinaryAnd loc) Boolean checkLower checkUpper]

  let errLabel = array_index_out_of_bounds
  let onFail = [TACIfFalse isValid errLabel]

  -- Mark that we need the out-of-bounds handler
  modify (\s -> s { outOfBound = True })

  return (xlower ++ xupper ++ xvalid ++ onFail)


-- Get array size for out of bound check
getArraySize :: Type ty varInfo funInfo -> Maybe Int
getArraySize (Array _ _ varInfo) = case varInfo of
    (VarInfo _ _ (Just (low, high))) -> Just (high - low + 1)
    _ -> Nothing
getArraySize _ = Nothing

-- Get array lower bound for out of bound check
getArrayLowerBound :: Type ty varInfo funInfo -> Maybe Int
getArrayLowerBound (Array _ _ varInfo) = case varInfo of
    (VarInfo _ _ (Just (low, high))) -> Just low
getArrayLowerBound _ = Nothing




getLExprType :: LExpr ty varInfo funInfo -> ty
getLExprType (LId _ ty)      = ty
getLExprType (LArr _ _ ty _)  = ty
getLExprType (LDeref _ ty _)    = ty

-- Extract SimpleIdent form EnrichedIdent
extractSimpleIdent :: EnrichedIdent -> SimpleIdent
extractSimpleIdent (Ident name loc _ _ _) = Ident name loc () () ()

-- Extract Address from XAddr
extractAddr :: XAddr -> Address
extractAddr (Addr addr) = addr
extractAddr _ = error "Expected Address but got different XAddr type"

-- Convert EnrichedType to TACType
typeToTACType :: EnrichedType -> TACType
typeToTACType (BasicType bt) = basicTypeToTACType bt
typeToTACType (Pointer _)    = TACAddr
typeToTACType (Array {})     = TACAddr

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

-- Convert TACType to BasicType
tacTypeToBasicType :: TACType -> BasicType
tacTypeToBasicType TACInteger   = Integer
tacTypeToBasicType TACReal      = Real
tacTypeToBasicType TACBoolean   = Boolean
tacTypeToBasicType TACCharacter = Character
tacTypeToBasicType TACAddr      = Integer  -- Default for addresses
tacTypeToBasicType TACError     = Void

getIdentName :: EnrichedIdent -> String
getIdentName (Ident name _ _ _ _) = name