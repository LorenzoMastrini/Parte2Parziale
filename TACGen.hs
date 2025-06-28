module TACGen where

import AbsLinguaggio
import Control.Monad.State
import Control.Monad.Writer
import TACabssynt
import TACMonad

emit :: TAC TACLabel -> TACGen ()
emit tac = tell [tac]

emitArg :: Address -> TACGen () 
emitArg operand = emit (TACParam operand)

-- basic expression gen
genRExpr :: EnrichedRExpr -> TACGen Address
genRExpr expr = case expr of 
  Binary rexpr1 bin rexpr2 ty -> genBin bin rexpr1 rexpr2 ty
  Unary unary rexpr ty -> genUnary unary rexpr
  Lit lit ty -> genLit lit
  FCall ident rexpr loc -> genFCall ident rexpr loc
  Ref unary rexpr ty -> genRef unary rexpr 
  LExpr lexpr ty -> genLExpr lexpr

genLExpr :: EnrichedLExpr -> TACGen Address
genLExpr (LId (Ident name loc _ varInfo _) ty) = do
  -- this is just a variable like `x`
  -- We need to create a ProgVar from the identifier
  let varInfo = TacProgVar (Ident name loc () () ()) True ByVal Integer  -- Default values, should be extracted from context
  return $ ProgVar varInfo TACInteger
genLExpr (LArr baseExpr indices ty _) = do
  -- like `arr[i][j]`
  baseAddr <- genLExpr baseExpr
  indexAddrs <- mapM genRExpr indices
  -- you might need to compute offset based on types and dimensions
  temp <- newtemp (getTacType ty)
  -- Use TACIndexedLoad instead of TACArrayAccess
  let varInfo = TacProgVar (Ident "" (Loc 0 0) () () ()) True ByVal Integer  -- This should be extracted from baseExpr
  emit $ TACIndexedLoad temp varInfo (head indexAddrs)
  return $ temp
genLExpr (LDeref ptrExpr ty _) = do
  addr <- genLExpr ptrExpr
  temp <- newtemp (getTacType ty)
  -- Use TACDerefRead instead of TACDeref
  emit $ TACDerefRead temp addr
  return $ temp

genOptElse :: OptElse (Maybe SimpleType) VarInfo FunInfo -> TACGen TACLabel
genOptElse NoElse = newlabel  -- generate a dummy label to jump over
genOptElse (Else stmt) = do
  elseLabel <- newlabel
  emit $ TACAddLabel elseLabel
  genStmt stmt
  return elseLabel

genUnary :: Unary -> EnrichedRExpr -> TACGen Address
genUnary op operand@(Unary _ _ ty) = do
  result <- newtemp (getTacType ty)
  rexpr <- genRExpr operand
  case op of
    UnaryPlus _loc ->
      -- Unary plus is usually a no-op
      emit $ TACUnaryAss rexpr (UnaryPlus _loc) (getBasicType ty) result

    UnaryMinus _loc -> do
      emit $ TACUnaryAss rexpr (UnaryMinus _loc) (getBasicType ty) result

    UnaryNot _loc -> do
      emit $ TACUnaryAss rexpr (UnaryNot _loc) (getBasicType ty) result

    UnaryRef _loc -> do
      emit $ TACUnaryAss rexpr (UnaryRef _loc) (getBasicType ty) result 

  return result 

genBin :: Binary -> EnrichedRExpr -> EnrichedRExpr -> Maybe SimpleType -> TACGen Address
genBin binOp leftOp rightOp ty = do
  temp <- newtemp (getTacType ty) -- generate a fresh temporary to hold the result
  left <- genRExpr leftOp
  right <- genRExpr rightOp
  emit $ TACBinaryAss temp binOp (getBasicType ty) left right -- binOp already distinguish between each case
  {-
  case binOp of
    BinaryPlus loc    -> emit $ TACBinaryAss temp binOp (getBasicType ty) left right
    BinaryMinus loc   -> emit $ TACBinaryAss temp binOp (getBasicType ty) left right
    BinaryMul loc     -> emit $ TACBinaryAss temp binOp (getBasicType ty) left right
    BinaryDiv loc     -> emit $ TACBinaryAss temp binOp (getBasicType ty) left right
    BinaryIntDiv loc  -> emit $ TACBinaryAss temp binOp (getBasicType ty) left right
    BinaryMod loc     -> emit $ TACBinaryAss temp binOp (getBasicType ty) left right
    BinaryOr loc      -> emit $ TACBinaryAss temp binOp (getBasicType ty) left right
    BinaryAnd loc     -> emit $ TACBinaryAss temp binOp (getBasicType ty) left right
    BinaryEq loc      -> emit $ TACBinaryAss temp binOp (getBasicType ty) left right
    BinaryNeq loc     -> emit $ TACBinaryAss temp binOp (getBasicType ty) left right
    BinaryLt loc      -> emit $ TACBinaryAss temp binOp (getBasicType ty) left right
    BinaryLte loc     -> emit $ TACBinaryAss temp binOp (getBasicType ty) left right
    BinaryGt loc      -> emit $ TACBinaryAss temp binOp (getBasicType ty) left right
    BinaryGte loc     -> emit $ TACBinaryAss temp binOp (getBasicType ty) left right
  -}
  return temp

genFCall :: EnrichedIdent -> [EnrichedRExpr] -> Loc -> TACGen Address
genFCall ident@(Ident _ _ ty _ _) rexprs loc = do 
  argOperands <- mapM genRExpr rexprs

  -- Emit TAC instructions to pass arguments (depends on calling convention)
  mapM_ emitArg argOperands
  
  -- Emit the call instruction, result in a fresh temporary
  result <- newtemp (getTacType ty)
  -- Fix TACFCall constructor: it takes Address, TACLabel, Int
  let funcAddr = identToOperand ident
  let numArgs = length argOperands
  emit $ TACFCall funcAddr 0 numArgs  -- Using 0 as default label, numArgs as the count
  
  -- Return the temp holding the call result
  return result

genRef :: Unary -> EnrichedRExpr -> TACGen Address
genRef unaryOp operandExpr = genUnary unaryOp operandExpr 

identToOperand :: EnrichedIdent -> Address
identToOperand (Ident name _ _ _ _loc) = 
  let varInfo = TacProgVar (Ident name (Loc 0 0) () () ()) True ByVal Integer  -- Default values, should be extracted from context
  in ProgVar varInfo TACInteger

genLit :: Literal -> TACGen Address
genLit lit = case lit of
  LiteralInteger i -> return $ TacLit (LiteralInteger i) TACInteger
  LiteralDouble d  -> return $ TacLit (LiteralDouble d) TACReal
  LiteralChar c    -> return $ TacLit (LiteralChar c) TACCharacter
  LiteralString s  -> return $ TacLit (LiteralString s) (TACString [TACCharacter])
  Literal_true     -> return $ TacLit Literal_true TACBoolean
  Literal_false    -> return $ TacLit Literal_false TACBoolean

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
    emit $ TACAssign dst src

  -- While loop
  While cond body _loc -> do
    start <- newlabel
    condFalse <- newlabel
    pushBreakLabel condFalse
    pushContinueLabel start
    emit $ TACAddLabel start
    condOp <- genRExpr cond
    emit $ TACIfFalse condOp condFalse
    genStmt body
    emit $ TACGoto start
    emit $ TACAddLabel condFalse

  -- Do-while loop
  DoWhile body cond _loc -> do
    start <- newlabel
    end <- newlabel 
    pushBreakLabel end
    pushContinueLabel start
    emit $ TACAddLabel start
    genStmt body
    condOp <- genRExpr cond
    emit $ TACIfFalse condOp end
    emit $ TACGoto start
    emit $ TACAddLabel end

  -- For loop (increasing)
  --ForTo lexpr fromExpr toExpr body -> do
  --  emit $ TacComment "not implemented yet"
    -- var <- genLExpr lexpr
    -- from <- genRExpr fromExpr
    -- to <- genRExpr toExpr
    -- emit $ TacAssign var from
    -- start <- newLabel
    -- end <- newLabel
    -- emit $ TACLabel start
    -- tmp <- newTemp
    -- emit $ TacBinary tmp "<=" var to
    -- emit $ TacIfZ tmp end
    -- genStmt body
    -- emit $ TacBinary var "+" var (TAC.ConstInt 1)
    -- emit $ TacGoto start
    -- emit $ TACLabel end

  -- If statement
  If cond thenStmt elseStmt _loc -> do
    elseLabel <- newlabel
    endLabel <- newlabel
    condOp <- genRExpr cond
    emit $ TACIfFalse condOp elseLabel
    genStmt thenStmt
    emit $ TACGoto endLabel
    emit $ TACAddLabel elseLabel
    genOptElse elseStmt
    emit $ TACAddLabel endLabel

  -- Case statement (not implemented yet)
  --Case expr items -> do
  --  emit $ TacComment "Case statements not implemented yet"

  -- Return statement
  ReturnStmt expr _ty _loc -> do
    val <- genRExpr expr
    emit $ TACReturnVal val

  -- Break statement
  BreakStmt _loc -> do
    endLabel <- popBreakLabel
    emit $ TACGoto endLabel

  -- Continue
  ContinueStmt _loc -> do
    contLabel <- popContinueLabel
    emit $ TACGoto contLabel


    
-- main entry

initState :: TACState
initState = TACState
  { tempCounter = 0
  , labelCounter = 0
  , breakLabels = []
  , continueLabels = []
  }

genProgram :: EnrichedProgram -> [TAC TACLabel]
genProgram (Prog _ _ (Block _ _ _ (CompStmt stmts)) _) =
  let (_, code) = runWriter (evalStateT (mapM_ genStmt stmts) initState)
  in code



-----------------------------------------------------------
-------------------- SUPPORT FUNCTIONS --------------------
-----------------------------------------------------------

-- Convert Maybe SimpleType in TACType
getTacType :: Maybe SimpleType -> TACType
getTacType (Just (BasicType t)) = case t of
  Integer -> TACInteger
  Real -> TACReal
  Character -> TACCharacter
  Boolean -> TACBoolean
  String -> TACString [TACCharacter]
  _ -> TACError
getTacType (Just (Pointer _)) = TACPointer 
getTacType (Just (Array _ _)) = TACArray
getTacType Nothing = TACError  -- default fallback

-- Estrae il BasicType da un Maybe SimpleType
getBasicType :: Maybe SimpleType -> BasicType
getBasicType (Just (BasicType t)) = t
getBasicType _ = Void  -- default fallback (error)

{-
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
-}