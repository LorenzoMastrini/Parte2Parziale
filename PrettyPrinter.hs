{-# LANGUAGE FlexibleInstances#-}

module PrettyPrinter where

import Data.List (stripPrefix)
import AbsLinguaggio
import Prelude



errorMsg :: String -> String
errorMsg x  = colorRed "[Error]: " ++ colorWhite  x

infoMsg :: String -> String
infoMsg x = colorBlue "[Info]: " ++ colorWhite x

warningMsg :: String -> String
warningMsg x = colorYellow "[Warning]: " ++ colorWhite x

colorRed :: String -> String
colorRed x  =  "\x1b[31;1m" ++ x ++ "\x1b[37;1m"

colorWhite :: String -> String
colorWhite x = "\x1b[37m" ++  x ++ "\x1b[37;1m"

colorGrey :: String -> String
colorGrey x = "\x1b[90;0m" ++"\x1b[90m"++  x ++ "\x1b[37;1m"

colorBlue :: String -> String
colorBlue x = "\x001b[34;1m" ++ x ++ "\x1b[37;1m"

colorCyan :: String -> String
colorCyan x = "\x001b[36m" ++ x ++ "\x1b[37;1m"

colorMagenta :: String -> String
colorMagenta x = "\x1b[35m" ++  x ++ "\x1b[37;1m"

colorYellow :: String -> String
colorYellow x = "\x001b[33m" ++ x ++ "\x1b[37;1m"

colorGreen :: String -> String
colorGreen x = "\x1b[32;1m" ++ x ++ "\x1b[37;1m"

underlineMag :: String -> String
underlineMag x = "\x1b[35;4m" ++ x ++ "\x1b[37;0m"++"\x1b[37;1m"

underlineYel :: String -> String
underlineYel x = "\x001b[33;4m" ++ x ++ "\x1b[37;0m"++"\x1b[37;1m"

lineAndColumn :: Int -> Int -> String
lineAndColumn l c = "(line: " ++ show l ++", column: "++ show c ++")"

underlineError :: String -> String
underlineError x = "\x1b[34;1m" ++ x ++ "\x1b[37;1m"

colorDecl = colorGreen
colorExpr = colorBlue
colorStmt = colorRed
colorPos = colorYellow
colorMod = colorCyan
colorRel = colorMagenta
colorLE = colorMagenta

-- Colors for the hierarchy
colorPipe :: String -> String
colorPipe x = colorGrey x

pad :: String -> String -> [String] -> [String]
pad first rest lines = zipWith (++) (first : repeat rest) lines

paddingMultiple :: [String] -> [String]
paddingMultiple = pad (colorPipe "   ") (colorPipe "|  ")

paddingSingle :: [String] -> [String]
paddingSingle = pad (colorPipe "   ") (colorPipe "|  ")

colorType :: String -> String
colorType = colorMagenta

colorId :: String -> String
colorId = colorCyan



---------------------------------------------------------------------
-------------------- PRETTY-PRINTER AST ENRICHED --------------------
---------------------------------------------------------------------

prettyAST :: EnrichedProgram -> String
prettyAST p = "Enriched AST:\n" ++ unlines (buildTree p)

class PrintTree a where
  buildTree :: a -> [String]

instance PrintTree EnrichedProgram where
  buildTree (Prog id formals block _) = (colorDecl "Program " ++ showId id) : (buildTree formals ++ buildTree block)

instance PrintTree EnrichedBlock where
  buildTree (Block consts vars procs stmt) = colorDecl "Block" : (buildTree consts ++ buildTree vars ++ buildTree procs ++ buildTree stmt)

instance PrintTree [EnrichedConstantDefinition] where
  buildTree [] = []
  buildTree [d] = paddingSingle (buildTree d)
  buildTree (d:ds) = paddingMultiple (buildTree d) ++ buildTree ds

instance PrintTree EnrichedConstantDefinition where
  buildTree (ConstDef id const) = (colorDecl "ConstDef " ++ showId id) : paddingSingle (buildTree const)

instance PrintTree [EnrichedVariableDeclaration] where
  buildTree [] = []
  buildTree [d] = paddingSingle (buildTree d)
  buildTree (d:ds) = paddingMultiple (buildTree d) ++ buildTree ds

instance PrintTree EnrichedVariableDeclaration where
  buildTree (VarDecl ids type_) = [colorDecl "VarDecl"] ++ (paddingMultiple (buildTree ids) ++ paddingSingle (buildTree type_))

instance PrintTree [EnrichedIdent] where
  buildTree [] = []
  buildTree [id] = paddingSingle [showId id]
  buildTree (id:ids) = paddingMultiple [showId id] ++ buildTree ids

instance PrintTree [EnrichedProcedureAndFunctionDeclaration] where
  buildTree [] = []
  buildTree [d] = paddingSingle (buildTree d)
  buildTree (d:ds) = paddingMultiple (buildTree d) ++ buildTree ds

instance PrintTree EnrichedProcedureAndFunctionDeclaration where
  buildTree (ProcFunDecl heading block) = colorDecl "ProcFunDecl" : (paddingMultiple (buildTree heading) ++ buildTree block)

instance PrintTree EnrichedProcedureAndFunctionHeading where
  buildTree (ProcHead id params) = [colorDecl "ProcHead " ++ showId id] ++ buildTree params
  buildTree (FunHead id params type_ loc) = [colorDecl "FunHead " ++ showId id] ++ (buildTree params ++ paddingSingle (buildTree type_))

instance PrintTree [EnrichedFormalParameters] where
  buildTree [] = []
  buildTree [d] = paddingSingle (buildTree d)
  buildTree (d:ds) = paddingMultiple (buildTree d) ++ buildTree ds

instance PrintTree EnrichedFormalParameters where
  buildTree (FormParam modality ids type_) = [colorMod (show modality)] ++ (paddingMultiple (buildTree ids) ++ paddingSingle (buildTree type_))

instance PrintTree SimpleType where
  buildTree (Array it ty varInfo) = [colorDecl "Array " ++ colorGrey (show varInfo)] ++ (paddingMultiple (buildTree it) ++ paddingSingle (buildTree ty))
  buildTree (Pointer ty) = [colorDecl "Pointer"] ++ paddingSingle (buildTree ty)
  buildTree (BasicType bt) = [colorDecl ("BasicType " ++ show bt)]

instance PrintTree EnrichedType where
  buildTree (Array it ty varInfo) = [colorDecl "Array " ++ showInfoFields [("VarInfo", colorGrey (stripConstructor "VarInfo" (show varInfo)))]] ++ paddingMultiple (buildTree it) ++ paddingSingle (buildTree ty)
  buildTree (Pointer ty) = [colorDecl "Pointer"] ++ paddingSingle (buildTree ty)
  buildTree (BasicType bt) = [colorDecl ("BasicType " ++ show bt)]
  buildTree Error = [colorRed "<Error>"]

instance PrintTree [SimpleIndexType] where
    buildTree [] = []
    buildTree [it] = paddingSingle (buildTree it)
    buildTree (it:its) = paddingMultiple (buildTree it) ++ buildTree its

instance PrintTree SimpleIndexType where
    buildTree (IndexRange c1 c2) = [colorDecl "IndexRange"] ++ (paddingMultiple (buildTree c1) ++ paddingSingle (buildTree c2))

instance PrintTree EnrichedConstant where
    buildTree (ConstId sign id) = [show sign ++ " " ++ showId id]
    buildTree (ConstInt sign i) = [show sign ++ " " ++ show i]
    buildTree (ConstDouble sign d) = [show sign ++ " " ++ show d]
    buildTree (ConstStr s) = ["\"" ++ s ++ "\""]
    buildTree (ConstChar c) = [" " ++ show c]
    buildTree (ConstBool b) = [" " ++ show b]

instance PrintTree EnrichedCompoundStatement where
  buildTree (CompStmt stmts) = colorStmt "CompoundStmt" : buildTree stmts

instance PrintTree [EnrichedStatement] where
  buildTree [] = []
  buildTree [s] = paddingSingle (buildTree s)
  buildTree (s:ss) = paddingMultiple (buildTree s) ++ buildTree ss

instance PrintTree EnrichedStatement where
  buildTree (Stmts stmt) = [colorStmt "Stmts"] ++ paddingSingle (buildTree stmt)
  buildTree (RExprStmt rexpr ty) = [colorStmt "RExprStmt " ++ showInfoFields [("type", colorType (showEnrichedType ty))]] ++ paddingSingle (buildTree rexpr)
  buildTree (Assign lexpr rexpr ty loc) = [colorStmt "Assign " ++ showInfoFields [("type", colorType (showEnrichedType ty)), ("loc", colorPos (showLoc loc))]] ++ (paddingMultiple (buildTree lexpr) ++ paddingSingle (buildTree rexpr))
  buildTree (While rexpr stmt loc) = [colorStmt "While " ++ showInfoFields [("loc", colorPos (showLoc loc))]] ++ (paddingMultiple (buildTree rexpr) ++ paddingSingle (buildTree stmt))
  buildTree (DoWhile stmt rexpr loc) = [colorStmt "DoWhile " ++ showInfoFields [("loc", colorPos (showLoc loc))]] ++ (paddingMultiple (buildTree stmt) ++ paddingSingle (buildTree rexpr))
  buildTree (ForTo lexpr r1 r2 stmt loc) = [colorStmt "ForTo" ++ showInfoFields [("loc", colorPos (showLoc loc))]] ++ (paddingMultiple (buildTree lexpr) ++ paddingMultiple (buildTree r1) ++ paddingMultiple (buildTree r2) ++ paddingSingle (buildTree stmt))
  buildTree (If rexpr stmt optElse loc) = [colorStmt "If " ++ showInfoFields [("loc", colorPos (showLoc loc))]] ++ (paddingMultiple (buildTree rexpr) ++ paddingMultiple (buildTree stmt) ++ paddingSingle (buildTree optElse))
  buildTree (ReturnStmt rexpr ty loc) = [colorStmt "ReturnStmt " ++ showInfoFields [("type", colorType (showEnrichedType ty)), ("loc", colorPos (showLoc loc))]] ++ paddingSingle (buildTree rexpr)
  buildTree (ReturnNoExpr ty loc) = [colorStmt "ReturnStmt " ++ showInfoFields [("type", colorType (showEnrichedType ty)), ("loc", colorPos (showLoc loc))]]
  buildTree (BreakStmt loc) = [colorStmt "BreakStmt " ++ showInfoFields [("loc", colorPos (showLoc loc))]]
  buildTree (ContinueStmt loc) = [colorStmt "ContinueStmt " ++ showInfoFields [("loc", colorPos (showLoc loc))]]

instance PrintTree EnrichedOptElse where
  buildTree NoElse = [colorStmt "NoElse"]
  buildTree (Else stmt) = colorStmt "Else" : paddingSingle (buildTree stmt)

instance PrintTree EnrichedLExpr where
  buildTree (LId id ty) = [colorLE "LId" ++ " " ++ showInfoFields [("type", colorType (showEnrichedType ty))] ++ " " ++ showId id]
  buildTree (LArr lexpr rexprs ty loc) = [colorLE "LArr" ++ " " ++ showInfoFields [("type", colorType (showEnrichedType ty)), ("loc", colorPos (showLoc loc))]] ++ (paddingMultiple (buildTree lexpr) ++ buildTree rexprs)
  buildTree (LDeref lexpr ty loc) = [colorLE "LDeref" ++ " " ++ showInfoFields [("type", colorType (showEnrichedType ty)), ("loc", colorPos (showLoc loc))]] ++ paddingSingle (buildTree lexpr)

instance PrintTree [EnrichedRExpr] where
    buildTree [] = []
    buildTree [r] = paddingSingle (buildTree r)
    buildTree (r:rs) = paddingMultiple (buildTree r) ++ buildTree rs

instance PrintTree EnrichedRExpr where
  buildTree (Binary r1 op r2 ty) =
    [colorExpr ("Binary " ++ colorRel (showBinaryOp op)) ++ " " ++
      showInfoFields [("type", colorType (showEnrichedType ty)), ("loc", colorPos (showLoc (getBinaryLoc op)))]]
    ++ (paddingMultiple (buildTree r1) ++ paddingSingle (buildTree r2))
  buildTree (Unary op r ty) = [colorExpr "Unary" ++ " " ++ showInfoFields [("type", colorType (showEnrichedType ty))]] ++ paddingMultiple (buildTree op) ++ paddingSingle (buildTree r)
  buildTree (Ref op lexpr ty) = [colorExpr "Ref" ++ " " ++ showInfoFields [("type", colorType (showEnrichedType ty))]] ++ (paddingMultiple (buildTree op) ++ paddingSingle (buildTree lexpr))
  buildTree (Lit lit ty) = [colorExpr "Lit" ++ " " ++ showInfoFields [("type", colorType (showEnrichedType ty))]] ++ paddingSingle (buildTree lit)
  buildTree (FCall id rexprs loc) = [colorExpr ("FCall " ++ showId id) ++ " " ++ showInfoFields [("loc", colorPos (showLoc loc))]] ++ buildTree rexprs
  buildTree (LExpr lexpr ty) = [colorExpr "LExpr" ++ " " ++ showInfoFields [("type", colorType (showEnrichedType ty))]] ++ paddingSingle (buildTree lexpr)

instance PrintTree Literal where
  buildTree (LiteralInteger i) = ["Integer " ++ show i]
  buildTree (LiteralDouble d) = ["Double " ++ show d]
  buildTree (LiteralChar c) = ["Char '" ++ [c] ++ "'"]
  buildTree (LiteralString s) = ["String \"" ++ s ++ "\""]
  buildTree Literal_true = ["Bool true"]
  buildTree Literal_false = ["Bool false"]

-- Instances for the simple types used in VarInfo
instance PrintTree SimpleConstant where
  buildTree (ConstId sign id) = [show sign ++ " " ++ showId (idToEnriched id)]
      where idToEnriched (Ident s l _ _ _) = Ident s l (BasicType Void) NoVarInfo NoFunInfo
  buildTree (ConstInt sign i) = [show sign ++ " " ++ show i]
  buildTree (ConstDouble sign d) = [show sign ++ " " ++ show d]
  buildTree (ConstStr s) = ["\"" ++ s ++ "\""]

instance PrintTree Unary where
  buildTree (UnaryPlus loc)    = [colorExpr ("Unary " ++ colorRel "Plus") ++ " " ++ showInfoFields [("loc", colorPos (showLoc loc))]]
  buildTree (UnaryMinus loc)   = [colorExpr ("Unary " ++ colorRel "Minus") ++ " " ++ showInfoFields [("loc", colorPos (showLoc loc))]]
  buildTree (UnaryNot loc)     = [colorExpr ("Unary " ++ colorRel "Not") ++ " " ++ showInfoFields [("loc", colorPos (showLoc loc))]]
  buildTree (UnaryRef loc)     = [colorExpr ("Unary " ++ colorRel "Ref") ++ " " ++ showInfoFields [("loc", colorPos (showLoc loc))]]


-----------------------------------------------------------
-------------------- SUPPORT FUNCTIONS --------------------
-----------------------------------------------------------

showInfoFields :: [(String, String)] -> String
showInfoFields fields = "{ " ++ (concatMap (\(k,v) -> colorGrey (k ++ ": ") ++ v ++ ", ") (Prelude.init fields) ++ colorGrey (fst (last fields) ++ ": ") ++ snd (last fields)) ++ " }"

showId :: EnrichedIdent -> String
showId (Ident s (Loc l c) ty varInfo funInfo) =
  colorId ("'" ++ s ++ "'") ++ " " ++ showInfoFields fields
  where
    baseFields =
      [ ("loc", colorPos (lineAndColumn l c))
      , ("type", colorType (showEnrichedType ty))
      ]
    varFields = case varInfo of
      NoVarInfo -> []
      _         -> [("varInfo", colorGrey (stripConstructor "VarInfo" (show varInfo)))]
    funFields = case funInfo of
      NoFunInfo -> []
      _         -> [("funInfo", colorGrey (show funInfo))]
    fields = baseFields ++ varFields ++ funFields

stripConstructor :: String -> String -> String
stripConstructor ctor str =
  case stripPrefix (ctor ++ " ") str of
    Just rest -> rest
    Nothing   -> str

showLoc :: Loc -> String
showLoc (Loc line col) = "(line: " ++ show line ++ ", column: " ++ show col ++ ")"



showBinaryOp :: Binary -> String
showBinaryOp op = case op of
  BinaryPlus loc    -> "BinaryPlus (+)"
  BinaryMinus loc   -> "BinaryMinus (-)"
  BinaryMul loc     -> "BinaryMul (*)"
  BinaryDiv loc     -> "BinaryDiv (/)"
  BinaryIntDiv loc  -> "BinaryIntDiv (div)"
  BinaryMod loc     -> "BinaryMod (mod)"
  BinaryOr loc      -> "BinaryOr (or)"
  BinaryAnd loc     -> "BinaryAnd (and)"
  BinaryEq loc      -> "BinaryEq (=)"
  BinaryNeq loc     -> "BinaryNeq (<>)"
  BinaryLt loc      -> "BinaryLt (<)"
  BinaryLte loc     -> "BinaryLte (<=)"
  BinaryGt loc      -> "BinaryGt (>)"
  BinaryGte loc     -> "BinaryGte (>=)"

getBinaryLoc :: Binary -> Loc
getBinaryLoc op = case op of
  BinaryPlus loc    -> loc
  BinaryMinus loc   -> loc
  BinaryMul loc     -> loc
  BinaryDiv loc     -> loc
  BinaryIntDiv loc  -> loc
  BinaryMod loc     -> loc
  BinaryOr loc      -> loc
  BinaryAnd loc     -> loc
  BinaryEq loc      -> loc
  BinaryNeq loc     -> loc
  BinaryLt loc      -> loc
  BinaryLte loc     -> loc
  BinaryGt loc      -> loc
  BinaryGte loc     -> loc



showEnrichedType :: EnrichedType -> String
showEnrichedType (Pointer t)           = "Pointer to " ++ showEnrichedType t
showEnrichedType (Array _ t _)         = "Array of " ++ showEnrichedType t
showEnrichedType (BasicType Integer)   = "Integer"
showEnrichedType (BasicType Real)      = "Real"
showEnrichedType (BasicType Boolean)   = "Boolean"
showEnrichedType (BasicType Character) = "Character"
showEnrichedType (BasicType String)    = "String"
showEnrichedType (BasicType Void)      = "Void"
showEnrichedType Error                 = "<error>"

enrichType :: SimpleType -> EnrichedType
enrichType (Array idxs t varInfo) = Array idxs (enrichType t) (VarInfo Nothing False (getRange idxs))
enrichType (Pointer t)            = Pointer (enrichType t)
enrichType (BasicType b)          = BasicType b
enrichType Error                  = Error

-- Extract the range from array indexType
getRange :: SimpleIndexType -> Maybe (Int, Int)
getRange (IndexRange c1 c2) =
    case (c1, c2) of
        (ConstInt _ n1, ConstInt _ n2) -> Just (fromInteger n1, fromInteger n2)
        _ -> Nothing