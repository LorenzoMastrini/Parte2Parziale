module TACGen where

import TAC
import AbsLinguaggio

import Control.Monad.State
import Control.Monad.Writer

-- | State used to generate TAC
data TACState = TACState
  { tempCount  :: Int
  , labelCount :: Int
  } deriving (Show)

type TACGen a = StateT TACState (Writer [TAC]) a

-- | Generate fresh temporary
newTemp :: TACGen Operand
newTemp = do
  st <- get
  let t = tempCount st
  put st { tempCount = t + 1 }
  return (Temp t)

-- | Generate fresh label
newLabel :: TACGen String
newLabel = do
  st <- get
  let l = labelCount st
  put st { labelCount = l + 1 }
  return ("L" ++ show l)

emit :: TAC -> TACGen ()
emit tac = tell [tac]

emitArg :: Operand -> TACGen () 
emitArg operand = emit (TacArg operand)

-- basic expression gen
genRExpr :: EnrichedRExpr -> TACGen Operand
genRExpr expr = case expr of 
  Binary rexpr1 bin rexpr2 ty -> genBin bin rexpr1 rexpr2 
  Unary unary rexpr ty -> genUnary unary rexpr
  Lit lit ty -> genLit lit
  FCall ident rexpr loc -> genFCall ident rexpr loc
  Ref unary rexpr ty -> genRef unary rexpr 
  LExpr lexpr ty -> genLExpr lexpr
-- genRExpr (Lit (LiteralInteger i) _) = return $ TAC.ConstInt i
-- genRExpr (Lit Literal_true _)       = return $ TAC.ConstBool True
-- genRExpr (Lit Literal_false _)      = return $ TAC.ConstBool False
  


genLExpr :: EnrichedLExpr -> TACGen Operand
genLExpr (LId (Ident name _ _ _ _) _) = do
  -- this is just a variable like `x`
  return $ Var name
genLExpr (LArr baseExpr indices _ _) = do
  -- like `arr[i][j]`
  baseAddr <- genLExpr baseExpr
  indexAddrs <- mapM genRExpr indices
  -- you might need to compute offset based on types and dimensions
  temp <- newTemp
  emit $ TacArrayAccess temp baseAddr indexAddrs
  return $ temp
genLExpr (LDeref ptrExpr _ _) = do
  addr <- genLExpr ptrExpr
  temp <- newTemp
  emit $ TacDeref temp addr
  return $ temp


genOptElse :: OptElse (Maybe SimpleType) VarInfo FunInfo -> TACGen String
genOptElse NoElse = newLabel  -- generate a dummy label to jump over
genOptElse (Else stmt) = do
  elseLabel <- newLabel
  emit $ TacLabel elseLabel
  genStmt stmt
  return elseLabel



genUnary :: Unary -> EnrichedRExpr -> TACGen Operand
genUnary op operand = do
  result <- newTemp
  rexpr <- genRExpr operand
  case op of
    UnaryPlus _loc ->
      -- Unary plus is usually a no-op
      emit $ TacUnary "+" (TAC.ConstInt 0) rexpr result

    UnaryMinus _loc -> do
      emit $ TacUnary "-" (TAC.ConstInt 0) rexpr result

    UnaryNot _loc -> do
      emit $ TacUnary "not" rexpr (TAC.ConstInt 0) result

    UnaryRef _loc -> do
      -- Reference operator (&) usually requires memory address knowledge,
      -- we'll placeholder this for now
      error "UnaryRef not implemented yet (requires memory model)"

  return result 


genBin :: Binary -> EnrichedRExpr -> EnrichedRExpr -> TACGen Operand
genBin binOp leftOp rightOp = do
  temp <- newTemp  -- generate a fresh temporary to hold the result
  left <- genRExpr leftOp
  right <- genRExpr rightOp
  let op = getBinaryOp binOp
  case binOp of
    BinaryPlus _ -> emit $ TacBinary temp op left right
    BinaryMinus _ -> emit $ TacBinary temp op left right
    BinaryMul _ -> emit $ TacBinary temp op left right
    BinaryDiv _ -> emit $ TacBinary temp op left right
    BinaryIntDiv _ -> emit $ TacBinary temp op left right
    BinaryMod _ -> emit $ TacBinary temp op left right
    BinaryOr _ -> emit $ TacBinary temp op left right
    BinaryAnd _ -> emit $ TacBinary temp op left right
    BinaryEq _ -> emit $ TacBinary temp op left right
    BinaryNeq _ -> emit $ TacBinary temp op left right
    BinaryLt _ -> emit $ TacBinary temp op left right
    BinaryLte _ -> emit $ TacBinary temp op left right
    BinaryGt _ -> emit $ TacBinary temp op left right
    BinaryGte _ -> emit $ TacBinary temp op left right
  return temp


genFCall :: EnrichedIdent -> [EnrichedRExpr] -> Loc -> TACGen Operand
genFCall ident rexprs loc = do 
  argOperands <- mapM genRExpr rexprs

  -- Emit TAC instructions to pass arguments (depends on calling convention)
  mapM_ emitArg argOperands
  
  -- Emit the call instruction, result in a fresh temporary
  result <- newTemp
  emit $ TacCall (identToOperand ident) argOperands result loc
  
  -- Return the temp holding the call result
  return result

genRef :: Unary -> EnrichedRExpr -> TACGen Operand
genRef unaryOp operandExpr = genUnary unaryOp operandExpr 


identToOperand :: EnrichedIdent -> Operand
identToOperand (Ident name _ _ _ _loc) = TAC.Var name  -- or some constructor like `Temp name`, if needed

getBinaryOp :: Binary -> String
getBinaryOp (BinaryPlus _)  = "+"
getBinaryOp (BinaryMinus _) = "-"
getBinaryOp (BinaryMul _)   = "*"
getBinaryOp (BinaryDiv _)   = "/"
getBinaryOp (BinaryAnd _)   = "and"
getBinaryOp (BinaryOr _)    = "or"
getBinaryOp (BinaryEq _)    = "=="
getBinaryOp (BinaryNeq _)   = "!="
getBinaryOp (BinaryLt _)    = "<"
getBinaryOp (BinaryLte _)   = "<="
getBinaryOp (BinaryGt _)    = ">"
getBinaryOp (BinaryGte _)   = ">="
getBinaryOp _               = error "Unhandled binary operator"


genLit :: Literal -> TACGen Operand
genLit lit = case lit of
  LiteralInteger i -> return $ TAC.ConstInt i
  LiteralDouble d  -> return $ TAC.ConstDouble d
  LiteralChar c    -> return $ TAC.ConstChar c
  LiteralString s  -> return $ TAC.Var s
  Literal_true     -> return $ TAC.ConstBool True
  Literal_false    -> return $ TAC.ConstBool False

-- assignment and statements

-- genStmt :: EnrichedStatement -> TACGen ()
-- genStmt (Assign (LId (Ident name _ _ _ _) _) expr _ _) = do
--   rhs <- genRExpr expr
--   emit $ TacAssign (Var name) rhs
--
-- genStmt (RExprStmt expr _) = do
--   _ <- genRExpr expr
--   return ()
--
-- genStmt (Stmts (CompStmt stmts)) = mapM_ genStmt stmts
-- genStmt _ = error "Unhandled statement for now"


genStmt :: EnrichedStatement -> TACGen ()
genStmt stmt = case stmt of

  -- Compound statement block
  Stmts (CompStmt stmts) ->
    mapM_ genStmt stmts

  -- Standalone expression statement
  RExprStmt expr _ty -> do
    _ <- genRExpr expr
    return ()

  -- Assignment
  Assign lexpr rexpr _ty _loc -> do
    src <- genRExpr rexpr
    dst <- genLExpr lexpr
    emit $ TacAssign dst src

  -- While loop
  While cond body _loc -> do
    start <- newLabel
    condFalse <- newLabel
    emit $ TacLabel start
    condOp <- genRExpr cond
    emit $ TacIfZ condOp condFalse
    genStmt body
    emit $ TacGoto start
    emit $ TacLabel condFalse

  -- Do-while loop
  DoWhile body cond _loc -> do
    start <- newLabel
    emit $ TacLabel start
    genStmt body
    condOp <- genRExpr cond
    emit $ TacIfZ condOp newLabel
    emit $ TacGoto start

  -- For loop (increasing)
  ForTo lexpr fromExpr toExpr body -> do
    var <- genLExpr lexpr
    from <- genRExpr fromExpr
    to <- genRExpr toExpr
    emit $ TacAssign var from
    start <- newLabel
    end <- newLabel
    emit $ TacLabel start
    tmp <- newTemp
    emit $ TacBinary tmp var "<=" to
    emit $ TacIfZ tmp end
    genStmt body
    emit $ TacBinary var var "+" "1"
    emit $ TacGoto start
    emit $ TacLabel end

  -- For loop (decreasing)
  ForDownTo lexpr fromExpr toExpr body -> do
    var <- genLExpr lexpr
    from <- genRExpr fromExpr
    to <- genRExpr toExpr
    emit $ TacAssign var from
    start <- newLabel
    end <- newLabel
    emit $ TacLabel start
    tmp <- newTemp
    emit $ TacBinary tmp var ">=" to
    emit $ TacIfZ tmp end
    genStmt body
    emit $ TacBinary var var "-" "1"
    emit $ TacGoto start
    emit $ TacLabel end

  -- If statement
  If cond thenStmt elseStmt _loc -> do
    elseLabel <- newLabel
    endLabel <- newLabel
    condOp <- genRExpr cond
    emit $ TacIfZ condOp elseLabel
    genStmt thenStmt
    emit $ TacGoto endLabel
    emit $ TacLabel elseLabel
    genOptElse elseStmt
    emit $ TacLabel endLabel

  -- Case statement (not implemented yet)
  Case expr items -> do
    emit $ TacComment "Case statements not implemented yet"

  -- Return statement
  ReturnStmt expr _ty _loc -> do
    val <- genRExpr expr
    emit $ TacReturn val

  -- Break
  BreakStmt _loc ->
    emit $ TacComment "Break not implemented (needs loop context)"

  -- Continue
  ContinueStmt _loc ->
    emit $ TacComment "Continue not implemented (needs loop context)"





-- main entry
genProgram :: EnrichedProgram -> [TAC]
genProgram (Prog _ _ (Block _ _ _ (CompStmt stmts)) _) =
  let (_, code) = runWriter (evalStateT (mapM_ genStmt stmts) initState)
  in code
  where
    initState = TACState 0 0
