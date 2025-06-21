module TACGen where

import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as Map
import Data.Map (Map)

import TAC
import AbsLinguaggio


type SymbolTable = Map String Type


getVarType :: Ident -> TACGen pn BasicType
getVarType (Ident name _ _) = do
  symtab <- ask
  case Map.lookup name symtab of
    Just t  -> return t
    Nothing -> error $ "Unknown variable: " ++ name


buildSymTab :: Block ty -> SymbolTable
buildSymTab (Block _ varDecls _ _) = Map.fromList $
  concatMap (\(VarDecl ids (Basic bt)) ->
    [ (n, bt) | Ident n _ _ <- ids ]) varDecls


type TACGen pn a = ReaderT SymbolTable (State (TACState pn)) a

data TACState pn = TACState
    { tempCount  :: Int
    , labelCount :: Int
    , code       :: [TAC pn]
    }



newTemp :: BasicType -> TACGen Address
newTemp t = do
  st <- get
  let n = tempCount st
  put st { tempCount = n + 1 }
  return $ Temporary n t

newLabel :: TACGen String
newLabel = do
  st <- get
  let lbl = "L" ++ show (labelCount st)
  put st { labelCount = labelCount st + 1 }
  return lbl

emit :: TAC -> TACGen ()
emit instr = do
  st <- get
  put st { code = code st ++ [instr] }

genRExpr :: RExpr Type -> TACGen Address
genRExpr (Binary e1 op e2) = do
    v1 <- genRExpr e1
    v2 <- genRExpr e2
    t <- newTemp
    emit $ TacBinOp t v1 (binOpToString op) v2
    return (OpTemp t)

genRExpr (Unary unOp e) = do
    v <- genRExpr e
    t <- newTemp
    case unOp of
        Unary_not -> do
            emit $ TacUnOp t "!" v
            return (OpTemp t)
        Unary1 -> return v  -- Unary plus: +x ≡ x
        Unary2 -> do        -- Unary minus: -x
            emit $ TacUnOp t "-" v
            return (OpTemp t)

genRExpr (Lit lit) =
    return (literalToOperand lit)

genRExpr (FCall (Ident name) args) = do
    argVals <- mapM genRExpr args
    mapM_ (emit . TacParam) (reverse argVals)
    t <- newTemp
    emit $ TacCall name (show $ length argVals) [(OpTemp t)]
    return (OpTemp t)

genRExpr (LExpr lexpr) = genLExpr lexpr

-- no symbolic table
genExpr (Lit (LiteralInteger i) t) = return $ TacLit (LInt i) t
genExpr (Binary e1 BinaryPlus e2) = do
  t1 <- genExpr e1
  t2 <- genExpr e2
  temp <- newTemp t
  emit $ TacBinary res AddOp t a1 a2
  -- emit $ case typ of
  --   TInt -> TacBinOp temp t1 "+" t2
  --   TDouble -> TacBinOp temp t1 "+.f" t2
  return temp
genExpr (LExpr (LId x)) = do
  -- Look up variable info from context
  varInfo <- lookupVar x
  return $ ProgVar varInfo t

showUnary :: Unary -> String
showUnary Unary1 = "-"
showUnary Unary2 = "+"
showUnary Unary_not = "not"

binOpToString :: Binary -> String
binOpToString op = case op of
    BinaryPlus  -> "+"
    BinaryMinus -> "-"
    BinaryMul   -> "*"
    BinaryDiv   -> "/"
    BinaryIntDiv -> "div"
    BinaryMod   -> "mod"
    BinaryOr    -> "or"
    BinaryAnd   -> "and"
    BinaryEq    -> "="
    BinaryNeq   -> "<>"
    BinaryLt    -> "<"
    BinaryLte   -> "<="
    BinaryGt    -> ">"
    BinaryGte   -> ">="

literalToAddress :: Literal -> Address
literalToAddress lit = TacLit lit (inferType lit)

inferType :: Literal -> BasicType
inferType (LiteralInteger _) = Integer
inferType (LiteralDouble _)  = Real
inferType (LiteralChar _)    = Character
inferType (LiteralString _)  = String
inferType Literal_true       = Boolean
inferType Literal_false      = Boolean



-- Assuming IdentOrNumber looks something like this:
data IdentOrNumber = IdentName String | Number Integer
  deriving (Eq, Ord, Show, Read)


identName :: Ident -> String
identName (Ident name) = name

constToAddress :: Constant -> TACGen Address
constToAddress (Const sign idOrNum) = case idOrNum of
  ConstId ident -> do
    symtab <- ask
    -- Look up the variable info by ident name (you'll need to extract String from Ident)
    let name = identName ident  -- You need a function to get String from Ident
    case lookupSymbol name symtab of
      Just varInfo -> return $ ProgVar varInfo (varBasicType varInfo)
      Nothing -> error $ "Undefined variable: " ++ name

  ConstInt n -> do
    let val = case sign of
                OptSign3 -> -n
                _ -> n
    return $ TacLit (LInt val) Integer

  ConstFloat f -> do
    let val = case sign of
                OptSign3 -> -f
                _ -> f
    return $ TacLit (LFloat val) Real

constToAddress (ConstStr s) =
  return $ TacLit (LString s) String

sizeOf :: Type -> Int
sizeOf (Basic BasicType_Integer)   = 4
sizeOf (Basic BasicType_Real)      = 8
sizeOf (Basic BasicType_Character) = 1
sizeOf (Basic BasicType_Boolean)   = 1
sizeOf (Basic BasicType_String)    = 16  -- Pointer + length, maybe?
sizeOf (Pointer _)                 = 8   -- Assuming 64-bit machine
sizeOf (Array _ t)                 = sizeOf t -- size of 1 element (not the whole array)
sizeOf _ = error "Unhandled type in sizeOf"

genLExpr :: LExpr -> TACGen Address
genLExpr (LId (Ident name)) = return (OpVar name)


genLExpr (LArr (LId (Ident name)) [indexExpr]) = do
  symTab <- ask  -- assumes you can access symbol table in your monad
  case Map.lookup name symTab of
    Just (Array _ t) -> do
      let elementSize = sizeOf t
      indexOp <- genRExpr indexExpr
      offsetTemp <- newTemp
      emit $ TacBinOp offsetTemp indexOp "*" (OpInt $ fromIntegral elementSize)
      addrTemp <- newTemp
      emit $ TacBinOp addrTemp (OpVar name) "+" (OpTemp offsetTemp)
      return (OpTemp addrTemp)
    _ -> error $ "Array variable not found or not an array: " ++ name


genLExpr (LArr baseExpr [indexExpr]) = do
  baseOp <- genLExpr baseExpr
  indexOp <- genRExpr indexExpr
  let elementSize = 4 -- TODO: fetch from symbol table/type info
  offsetTemp <- newTemp
  emit $ TacBinOp offsetTemp indexOp "*" (OpInt $ fromIntegral elementSize)
  addrTemp <- newTemp
  emit $ TacBinOp addrTemp baseOp "+" (OpTemp offsetTemp)
  return (OpTemp addrTemp)

genLExpr (LArr _ _) = error "Multidimensional arrays not yet implemented"

genLExpr (LDeref l) = do
  addr <- genLExpr l
  return addr



genStmt :: Statement -> TACGen ()
genStmt NoStmt = return ()

genStmt (Stmts (CompStmt stmts)) = mapM_ genStmt stmts

genStmt (RExprStmt expr) = do
    _ <- genRExpr expr  -- discard result, just for side-effects (e.g., function calls)
    return ()

genStmt (Assign (LId (Ident name)) expr) = do
    val <- genRExpr expr
    emit $ TacAssign name val

genStmt (While cond body) = do
    lCond <- newLabel
    lBody <- newLabel
    lEnd  <- newLabel
    emit $ TacLabel lCond
    condVal <- genRExpr cond
    emit $ TacIf condVal "==" (OpBool False) lEnd
    emit $ TacLabel lBody
    genStmt body
    emit $ TacGoto lCond
    emit $ TacLabel lEnd

genStmt (DoWhile body cond) = do
    lStart <- newLabel
    emit $ TacLabel lStart
    genStmt body
    condVal <- genRExpr cond
    emit $ TacIf condVal "!=" (OpBool False) lStart

genStmt (ForTo (LId (Ident name)) from to body) = do
    fromVal <- genRExpr from
    toVal <- genRExpr to
    emit $ TacAssign name fromVal
    lCond <- newLabel
    lBody <- newLabel
    lEnd  <- newLabel
    emit $ TacLabel lCond
    emit $ TacIf (OpVar name) ">" toVal lEnd
    emit $ TacLabel lBody
    genStmt body
    emit $ TacBinOp name (OpVar name) "+" (OpInt 1)
    emit $ TacGoto lCond
    emit $ TacLabel lEnd

genStmt (ForDownTo (LId (Ident name)) from to body) = do
    fromVal <- genRExpr from
    toVal <- genRExpr to
    emit $ TacAssign name fromVal
    lCond <- newLabel
    lBody <- newLabel
    lEnd  <- newLabel
    emit $ TacLabel lCond
    emit $ TacIf (OpVar name) "<" toVal lEnd
    emit $ TacLabel lBody
    genStmt body
    emit $ TacBinOp name (OpVar name) "-" (OpInt 1)
    emit $ TacGoto lCond
    emit $ TacLabel lEnd

genStmt (If cond thenStmt optElse) = do
    lThen <- newLabel
    lElse <- newLabel
    lEnd  <- newLabel
    condVal <- genRExpr cond
    emit $ TacIf condVal "!=" (OpBool False) lThen
    emit $ TacGoto lElse
    emit $ TacLabel lThen
    genStmt thenStmt
    emit $ TacGoto lEnd
    emit $ TacLabel lElse
    case optElse of
        NoElse   -> return ()
        Else els -> genStmt els
    emit $ TacLabel lEnd

genStmt (Case expr items) = do
    exprVal <- genRExpr expr
    endLbl <- newLabel
    mapM_ (emitCase exprVal endLbl) items
    emit $ TacLabel endLbl
  where
    emitCase exprVal endLbl (CaseItem c s) = do
        cVal <- constToOperand c
        l <- newLabel
        emit $ TacIf exprVal "==" cVal l
        emit $ TacLabel l
        genStmt s
        emit $ TacGoto endLbl


initState :: TACState
initState = TACState { tempCount = 0, labelCount = 0, code = [] }

runTACGen :: SymbolTable -> TACGen a -> a
runTACGen symtab gen = evalState (runReaderT gen symtab) initState 

-- genTAC :: SymbolTable -> Program -> [TAC]
-- genTAC symTab prog = code finalState
--   where
--     finalState = execState (runReaderT (genProgram prog) symTab) initState
--
--
-- genProgram :: Program -> TACGen ()
-- genProgram (Prog _ _ block) = do
--   let symtab = buildSymTab (getDecls block)
--   local (const symtab) $ genBlock block
-- genProgram (ProgNoParams _ block) = do
--   let symtab = buildSymTab (getDecls block)
--   genBlock block
-- genProgram p = error ("genProgram: unhandled pattern " ++ show p)

getDecls :: Block -> DeclarationPart
getDecls (Block decls _) = decls


genBlock :: Block -> TACGen ()
genBlock (Block _ (CompStmt stmts)) = mapM_ genStmt stmts


getBlock :: Program -> Block
getBlock (ProgNoParams _ block) = block
getBlock (Prog _ _ block)      = block


genProgram :: Program ty -> TACGen ()
genProgram (Prog _ _ block) = 
  local (const $ buildSymTab block) $ genBlock block

genTAC :: Program ty -> [TAC]
genTAC prog = code finalState
  where
    (_, finalState) = runState (runReaderT (genProgram prog) sym) initState
    sym = buildSymTab (let (Prog _ _ b) = prog in b)
