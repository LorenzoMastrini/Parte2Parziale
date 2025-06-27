module TACGen where

import AbsLinguaggio
import Control.Monad.State
import Control.Monad.Writer
import TACabssynt
import TACMonad



--type TACGen a = StateT TacState (Writer [TAC]) a



emit :: TAC TacLabel -> TACGen ()
emit tac = tell [tac]

emitArg :: Address -> TACGen () 
emitArg operand = emit (TacParam operand)

-- basic expression gen
genRExpr :: EnrichedRExpr -> TACGen Address
genRExpr expr = case expr of 
  Binary rexpr1 bin rexpr2 ty -> genBin bin rexpr1 rexpr2 ty
  Unary unary rexpr ty -> genUnary unary rexpr
  Lit lit ty -> genLit lit
  FCall ident rexpr loc -> genFCall ident rexpr loc
  Ref unary rexpr ty -> genRef unary rexpr 
  LExpr lexpr ty -> genLExpr lexpr
-- genRExpr (Lit (LiteralInteger i) _) = return $ TAC.ConstInt i
-- genRExpr (Lit Literal_true _)       = return $ TAC.ConstBool True
-- genRExpr (Lit Literal_false _)      = return $ TAC.ConstBool False
  


genLExpr :: EnrichedLExpr -> TACGen Address
genLExpr (LId (Ident name _ _ _ _) _) = do
  -- this is just a variable like `x`
  return $ Var name
genLExpr (LArr baseExpr indices ty _) = do
  -- like `arr[i][j]`
  baseAddr <- genLExpr baseExpr
  indexAddrs <- mapM genRExpr indices
  -- you might need to compute offset based on types and dimensions
  temp <- newtemp (getTacType ty)
  emit $ TacArrayAccess temp baseAddr (head indexAddrs)
  return $ temp
genLExpr (LDeref ptrExpr ty _) = do
  addr <- genLExpr ptrExpr
  temp <- newtemp (getTacType ty)
  emit $ TacDeref temp addr
  return $ temp


genOptElse :: OptElse (Maybe SimpleType) VarInfo FunInfo -> TACGen TacLabel
genOptElse NoElse = newlabel  -- generate a dummy label to jump over
genOptElse (Else stmt) = do
  elseLabel <- newlabel
  emit $ TacAddLabel elseLabel
  genStmt stmt
  return elseLabel



genUnary :: Unary -> EnrichedRExpr -> TACGen Address
genUnary op operand@(Unary _ _ ty) = do
  result <- newtemp (getTacType ty)
  rexpr <- genRExpr operand
  case op of
    UnaryPlus _loc ->
      -- Unary plus is usually a no-op
      emit $ TacUnary rexpr (UnaryPlus _loc) (getBasicType ty) result

    UnaryMinus _loc -> do
      emit $ TacUnary rexpr (UnaryMinus _loc) (getBasicType ty) result

    UnaryNot _loc -> do
      emit $ TacUnary rexpr (UnaryNot _loc) (getBasicType ty) result

    UnaryRef _loc -> do
      emit $ TacUnary rexpr (UnaryRef _loc) (getBasicType ty) result 


  return result 


genBin :: Binary -> EnrichedRExpr -> EnrichedRExpr -> Maybe SimpleType -> TACGen Address
genBin binOp leftOp rightOp ty = do
  temp <- newtemp (getTacType ty) -- generate a fresh temporary to hold the result
  left <- genRExpr leftOp
  right <- genRExpr rightOp
  let op = getBinaryOp binOp
  case binOp of
    BinaryPlus loc    -> emit $ TacBinary temp (BinaryPlus loc) (getBasicType ty) left right
    BinaryMinus loc   -> emit $ TacBinary temp (BinaryMinus loc) (getBasicType ty) left right
    BinaryMul loc     -> emit $ TacBinary temp (BinaryMul loc) (getBasicType ty) left right
    BinaryDiv loc     -> emit $ TacBinary temp (BinaryDiv loc) (getBasicType ty) left right
    BinaryIntDiv loc  -> emit $ TacBinary temp (BinaryIntDiv loc) (getBasicType ty) left right
    BinaryMod loc     -> emit $ TacBinary temp (BinaryMod loc) (getBasicType ty) left right
    BinaryOr loc      -> emit $ TacBinary temp (BinaryOr loc) (getBasicType ty) left right
    BinaryAnd loc     -> emit $ TacBinary temp (BinaryAnd loc) (getBasicType ty) left right
    BinaryEq loc      -> emit $ TacBinary temp (BinaryEq loc) (getBasicType ty) left right
    BinaryNeq loc     -> emit $ TacBinary temp (BinaryNeq loc) (getBasicType ty) left right
    BinaryLt loc      -> emit $ TacBinary temp (BinaryLt loc) (getBasicType ty) left right
    BinaryLte loc     -> emit $ TacBinary temp (BinaryLte loc) (getBasicType ty) left right
    BinaryGt loc      -> emit $ TacBinary temp (BinaryGt loc) (getBasicType ty) left right
    BinaryGte loc     -> emit $ TacBinary temp (BinaryGte loc) (getBasicType ty) left right
  return temp


genFCall :: EnrichedIdent -> [EnrichedRExpr] -> Loc -> TACGen Address
genFCall ident@(Ident _ _ ty _ _) rexprs loc = do 
  argOperands <- mapM genRExpr rexprs

  -- Emit TAC instructions to pass arguments (depends on calling convention)
  mapM_ emitArg argOperands
  
  -- Emit the call instruction, result in a fresh temporary
  result <- newtemp (getTacType ty)
  emit $ TacFCall (identToOperand ident) argOperands result
  
  -- Return the temp holding the call result
  return result

genRef :: Unary -> EnrichedRExpr -> TACGen Address
genRef unaryOp operandExpr = genUnary unaryOp operandExpr 


identToOperand :: EnrichedIdent -> Address
identToOperand (Ident name _ _ _ _loc) = TAC.Var name  -- or some constructor like `Temp name`, if needed

getBinaryOp :: Binary -> String
getBinaryOp (BinaryPlus _)  = "+"
getBinaryOp (BinaryMinus _) = "-"
getBinaryOp (BinaryMul _)   = "*"
getBinaryOp (BinaryDiv _)   = "/"
getBinaryOp (BinaryIntDiv _) = "div"
getBinaryOp (BinaryMod _)   = "mod"
getBinaryOp (BinaryAnd _)   = "and"
getBinaryOp (BinaryOr _)    = "or"
getBinaryOp (BinaryEq _)    = "=="
getBinaryOp (BinaryNeq _)   = "!="
getBinaryOp (BinaryLt _)    = "<"
getBinaryOp (BinaryLte _)   = "<="
getBinaryOp (BinaryGt _)    = ">"
getBinaryOp (BinaryGte _)   = ">="


genLit :: Literal -> TACGen Address
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
    start <- newlabel
    condFalse <- newlabel
    pushBreakLabel condFalse
    pushContinueLabel start
    emit $ TacAddLabel start
    condOp <- genRExpr cond
    emit $ TacIfFalse condOp condFalse
    genStmt body
    emit $ TacGoto start
    emit $ TacAddLabel condFalse

  -- Do-while loop
  DoWhile body cond _loc -> do
    start <- newlabel
    end <- newlabel 
    pushBreakLabel end
    pushContinueLabel start
    emit $ TacAddLabel start
    genStmt body
    condOp <- genRExpr cond
    emit $ TacIfFalse condOp end
    emit $ TacGoto start
    emit $ TacAddLabel end

  -- For loop (increasing)
  --ForTo lexpr fromExpr toExpr body -> do
  --  emit $ TacComment "not implemented yet"
    -- var <- genLExpr lexpr
    -- from <- genRExpr fromExpr
    -- to <- genRExpr toExpr
    -- emit $ TacAssign var from
    -- start <- newLabel
    -- end <- newLabel
    -- emit $ TacLabel start
    -- tmp <- newTemp
    -- emit $ TacBinary tmp "<=" var to
    -- emit $ TacIfZ tmp end
    -- genStmt body
    -- emit $ TacBinary var "+" var (TAC.ConstInt 1)
    -- emit $ TacGoto start
    -- emit $ TacLabel end

  -- -- For loop (decreasing)
  -- ForDownTo lexpr fromExpr toExpr body -> do
  --   var <- genLExpr lexpr
  --   from <- genRExpr fromExpr
  --   to <- genRExpr toExpr
  --   emit $ TacAssign var from
  --   start <- newLabel
  --   end <- newLabel
  --   emit $ TacLabel start
  --   tmp <- newTemp
  --   emit $ TacBinary tmp ">=" var to
  --   emit $ TacIfZ tmp end
  --   genStmt body
  --   emit $ TacBinary var "-" var (TAC.ConstInt 1)
  --   emit $ TacGoto start
  --   emit $ TacLabel end

  -- If statement
  If cond thenStmt elseStmt _loc -> do
    elseLabel <- newlabel
    endLabel <- newlabel
    condOp <- genRExpr cond
    emit $ TacIfFalse condOp elseLabel
    genStmt thenStmt
    emit $ TacGoto endLabel
    emit $ TacAddLabel elseLabel
    genOptElse elseStmt
    emit $ TacAddLabel endLabel

  -- Case statement (not implemented yet)
  --Case expr items -> do
  --  emit $ TacComment "Case statements not implemented yet"

  -- Return statement
  ReturnStmt expr _ty _loc -> do
    val <- genRExpr expr
    emit $ TacReturnVal val

  -- Break statement
  BreakStmt _loc -> do
    endLabel <- popBreakLabel
    emit $ TacGoto endLabel

  -- Continue
  ContinueStmt _loc -> do
    contLabel <- popContinueLabel
    emit $ TacGoto contLabel




-- main entry

initState :: TacState
initState = TacState
  { tempCounter = 0
  , labelCounter = 0
  , breakLabels = []
  , continueLabels = []
  }

genProgram :: EnrichedProgram -> [TAC TacLabel]
genProgram (Prog _ _ (Block _ _ _ (CompStmt stmts)) _) =
  let (_, code) = runWriter (evalStateT (mapM_ genStmt stmts) initState)
  in code

-- Converte Maybe SimpleType in TacType (BasicType)
getTacType :: Maybe SimpleType -> TacType
getTacType (Just (BasicType t)) = case t of
  Integer -> TACInteger
  Real -> TACReal
  Character -> TACCharacter
  Boolean -> TACBoolean
  String -> TACString
  _ -> TACError
getTacType (Just (Pointer _)) = TACPointer 
getTacType (Just (Array _ _)) = TACArray
getTacType Nothing = TACError  -- default fallback

-- Estrae il BasicType da un Maybe SimpleType
getBasicType :: Maybe SimpleType -> BasicType
getBasicType (Just (BasicType t)) = t
getBasicType _ = Integer  -- default fallback