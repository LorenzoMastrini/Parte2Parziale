{-# LANGUAGE FlexibleInstances#-}

module PrettyPrinter where

import AbsLinguaggio
--import TACGenerator
--import Utils
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



-----------------------------------------------------------
-------------------- SUPPORT FUNCTIONS --------------------
-----------------------------------------------------------

showInfoFields :: [(String, String)] -> String
showInfoFields fields = "{ " ++ (concatMap (\(k,v) -> colorGrey (k ++ ": ") ++ v ++ ", ") (Prelude.init fields) ++ colorGrey (fst (last fields) ++ ": ") ++ snd (last fields)) ++ " }"

showId :: EnrichedIdent -> String
showId (Ident s (Loc l c) ty varInfo funInfo) = colorId ("'" ++ s ++ "'") ++ " " ++ showInfoFields [
    ("loc", colorPos (lineAndColumn l c)),
    ("type", colorType (showMType ty)),
    ("varInfo", colorGrey (show varInfo)),
    ("funInfo", colorGrey (show funInfo))
  ]

showLoc :: Loc -> String
showLoc (Loc line col) = "(line: " ++ show line ++ ", column: " ++ show col ++ ")"

showMType :: Maybe SimpleType -> String
showMType ty = case ty of
    Just (Array _ arrTy)        -> "Array of " ++ showMType (Just arrTy)
    Just (Pointer ptrTy)        -> "Pointer to " ++ showMType (Just ptrTy)
    Just (BasicType Integer)    -> "Integer"
    Just (BasicType Real)       -> "Real"
    Just (BasicType Boolean)    -> "Boolean"
    Just (BasicType Character)  -> "Character"
    Just (BasicType String)     -> "String"
    Just (BasicType Void)       -> "Void"
    Nothing                     -> "Nothing"

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

{-
showId :: BIdent -> String 
showId (BIdent ((x,y),z)) = 
  "'"++ z ++ "', " ++ 
  colorYellow ( "<line:" ++(show x) 
  ++",col:"++(show y) ++ ">")

showVal :: Val -> String
showVal v =
 case v of
 IntVal (BInteger ((x,y),z)) ->
  "Int  "++ z ++ ", " ++ 
  colorYellow ( "<line:" ++(show x) 
  ++",col:"++(show y) ++ ">")
 RealVal (BReal ((x,y),z)) ->
  "Real "++ z ++ ", " 
  ++ colorYellow ( "<line:" ++(show x) 
  ++",col:"++(show y) ++ ">")
 CharVal (BChar ((x,y),z)) ->
  "Char "++ z ++ ", " 
  ++ colorYellow ( "<line:" ++(show x) 
  ++",col:"++(show y) ++ ">")
 BoolVal (BBool ((x,y),z)) ->
  "Bool "++ z ++ ", " 
  ++ colorYellow ( "<line:" ++(show x) 
  ++",col:"++(show y) ++ ">")
 StringVal (BString ((x,y),z)) ->
  "String "++ z ++ ", " 
  ++ colorYellow ( "<line:" ++(show x) 
  ++",col:"++(show y) ++ ">")

showTACAddr :: TACAddr -> (String,Int)
showTACAddr a = case a of
  Addr name -> colorName name
  PointAddr name -> colorName ('&':name)
  DerefAddr name -> colorName ('*':name)
  AddrAt name i -> (id++"["++ addr ++"]",dim1+dim2+2)
   where (id,dim1) = colorName name
         (addr,dim2) = showTACAddr i 
  ValAddr name -> colorName name
  NoAddr -> ("",0)

colorName :: String -> (String,Int)
colorName name = case elem '@' name of
  True -> fin
   where fin1 = takeWhile (/= '@') name
         dim = length fin1
         fin2 = drop (dim+1) name
         fin = (colorBlue fin1 ++ colorCyan "@" ++ colorBlue fin2,38)
  False -> (name,-2) 


showTACLabel :: TACLabel -> (String,Int)
showTACLabel lab = case lab of
  Label s -> colorLabel s 
  Fall    -> ("FALL",0)
  NoLab   -> ("",0)


colorLabel :: String -> (String,Int)
colorLabel lab = case elem '@' lab of
  True -> fin
   where fin1 = takeWhile (/= '@') lab 
         dim = length fin1
         fin2 = drop (dim+1) lab
         fin = (colorBlue fin1 ++ colorCyan "@" ++ colorBlue fin2,40)
  false -> (lab, -2) 
-}



-----------------------------------------------------
-------------------- PRETTY TREE --------------------
-----------------------------------------------------

-- Colorazione pipe per la gerarchia
colorPipe :: String -> String
colorPipe x = colorGrey x

pad :: String -> String -> [String] -> [String]
pad first rest lines = zipWith (++) (first : repeat rest) lines

paddingMultiple :: [String] -> [String]
-- pipe colorata e spaziatura originale
paddingMultiple = pad (colorPipe "   ") (colorPipe "|  ")

paddingSingle :: [String] -> [String]
-- pipe colorata e spaziatura originale
paddingSingle = pad (colorPipe "   ") (colorPipe "|  ")


prettyTree :: EnrichedProgram -> String
prettyTree p = unlines (buildTree p)

class PrintTree a where
  buildTree :: a -> [String]

colorType :: String -> String
colorType = colorMagenta

colorId :: String -> String
colorId = colorCyan


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
  buildTree (ProcFunDecl heading block) = (colorDecl "ProcFunDecl") : (paddingMultiple (buildTree heading) ++ buildTree block)

instance PrintTree EnrichedProcedureAndFunctionHeading where
  buildTree (ProcHead id params) = [colorDecl "ProcHead " ++ showId id] ++ buildTree params
  buildTree (FunHead id params type_ loc) = [colorDecl "FunHead " ++ showId id] ++ (buildTree params ++ paddingSingle (buildTree type_))

instance PrintTree [EnrichedFormalParameters] where
  buildTree [] = []
  buildTree [d] = paddingSingle (buildTree d)
  buildTree (d:ds) = paddingMultiple (buildTree d) ++ buildTree ds

instance PrintTree EnrichedFormalParameters where
  buildTree (FormParam modality ids type_) = [colorMod (show modality)] ++ (paddingMultiple (buildTree ids) ++ paddingSingle (buildTree type_))

instance PrintTree EnrichedType where
  buildTree (Array it ty) = [colorDecl "Array"] ++ (paddingMultiple (buildTree it) ++ paddingSingle (buildTree ty))
  buildTree (Pointer ty) = [colorDecl "Pointer"] ++ paddingSingle (buildTree ty)
  buildTree (BasicType bt) = [colorDecl ("BasicType " ++ show bt)]

instance PrintTree [EnrichedIndexType] where
    buildTree [] = []
    buildTree [it] = paddingSingle (buildTree it)
    buildTree (it:its) = paddingMultiple (buildTree it) ++ buildTree its

instance PrintTree EnrichedIndexType where
    buildTree (IndexRange c1 c2) = [colorDecl "IndexRange"] ++ (paddingMultiple (buildTree c1) ++ paddingSingle (buildTree c2))

instance PrintTree EnrichedConstant where
    buildTree (ConstId sign id) = [show sign ++ " " ++ showId id]
    buildTree (ConstInt sign i) = [show sign ++ " " ++ show i]
    buildTree (ConstDouble sign d) = [show sign ++ " " ++ show d]
    buildTree (ConstStr s) = ["\"" ++ s ++ "\""]
    buildTree (ConstChar c) = [" " ++ show c]
    buildTree (ConstBool b) = [" " ++ show b]

instance PrintTree EnrichedCompoundStatement where
  buildTree (CompStmt stmts) = (colorStmt "CompoundStmt") : buildTree stmts

instance PrintTree [EnrichedStatement] where
  buildTree [] = []
  buildTree [s] = paddingSingle (buildTree s)
  buildTree (s:ss) = paddingMultiple (buildTree s) ++ buildTree ss

instance PrintTree EnrichedStatement where
  buildTree (Stmts stmt) = [colorStmt "Stmts"] ++ paddingSingle (buildTree stmt)
  buildTree (RExprStmt rexpr ty) = [colorStmt "RExprStmt " ++ showInfoFields [("type", colorType (showMType ty))]] ++ paddingSingle (buildTree rexpr)
  buildTree (Assign lexpr rexpr ty loc) = [colorStmt "Assign " ++ showInfoFields [("type", colorType (showMType ty)), ("loc", colorPos (showLoc loc))]] ++ (paddingMultiple (buildTree lexpr) ++ paddingSingle (buildTree rexpr))
  buildTree (While rexpr stmt loc) = [colorStmt "While " ++ showInfoFields [("loc", colorPos (showLoc loc))]] ++ (paddingMultiple (buildTree rexpr) ++ paddingSingle (buildTree stmt))
  buildTree (DoWhile stmt rexpr loc) = [colorStmt "DoWhile " ++ showInfoFields [("loc", colorPos (showLoc loc))]] ++ (paddingMultiple (buildTree stmt) ++ paddingSingle (buildTree rexpr))
  buildTree (ForTo lexpr r1 r2 stmt) = [colorStmt "ForTo"] ++ (paddingMultiple (buildTree lexpr) ++ paddingMultiple (buildTree r1) ++ paddingMultiple (buildTree r2) ++ paddingSingle (buildTree stmt))
  buildTree (ForDownTo lexpr r1 r2 stmt) = [colorStmt "ForDownTo"] ++ (paddingMultiple (buildTree lexpr) ++ paddingMultiple (buildTree r1) ++ paddingMultiple (buildTree r2) ++ paddingSingle (buildTree stmt))
  buildTree (If rexpr stmt optElse loc) = [colorStmt "If " ++ showInfoFields [("loc", colorPos (showLoc loc))]] ++ (paddingMultiple (buildTree rexpr) ++ paddingMultiple (buildTree stmt) ++ paddingSingle (buildTree optElse))
  buildTree (Case rexpr cases) = [colorStmt "Case"] ++ (paddingMultiple (buildTree rexpr) ++ buildTree cases)
  buildTree (ReturnStmt rexpr ty loc) = [colorStmt "ReturnStmt " ++ showInfoFields [("type", colorType (showMType ty)), ("loc", colorPos (showLoc loc))]] ++ paddingSingle (buildTree rexpr)
  buildTree (BreakStmt loc) = [colorStmt "BreakStmt " ++ showInfoFields [("loc", colorPos (showLoc loc))]]
  buildTree (ContinueStmt loc) = [colorStmt "ContinueStmt " ++ showInfoFields [("loc", colorPos (showLoc loc))]]

instance PrintTree EnrichedOptElse where
  buildTree NoElse = [colorStmt "NoElse"]
  buildTree (Else stmt) = colorStmt "Else" : paddingSingle (buildTree stmt)

instance PrintTree [EnrichedCaseItem] where
  buildTree [] = []
  buildTree [c] = paddingSingle (buildTree c)
  buildTree (c:cs) = paddingMultiple (buildTree c) ++ buildTree cs

instance PrintTree EnrichedCaseItem where
  buildTree (CaseItem const stmt) = colorStmt "CaseItem" : (paddingMultiple (buildTree const) ++ paddingSingle (buildTree stmt))

instance PrintTree EnrichedLExpr where
  buildTree (LId id ty) = [colorLE "LId" ++ " " ++ showInfoFields [("type", colorType (showMType ty))] ++ " " ++ showId id]
  buildTree (LArr lexpr rexprs ty loc) = [colorLE "LArr" ++ " " ++ showInfoFields [("type", colorType (showMType ty)), ("loc", colorPos (showLoc loc))]] ++ (paddingMultiple (buildTree lexpr) ++ buildTree rexprs)
  buildTree (LDeref lexpr ty loc) = [colorLE "LDeref" ++ " " ++ showInfoFields [("type", colorType (showMType ty)), ("loc", colorPos (showLoc loc))]] ++ paddingSingle (buildTree lexpr)

instance PrintTree [EnrichedRExpr] where
    buildTree [] = []
    buildTree [r] = paddingSingle (buildTree r)
    buildTree (r:rs) = paddingMultiple (buildTree r) ++ buildTree rs

instance PrintTree EnrichedRExpr where
  buildTree (Binary r1 op r2 ty) =
    [colorExpr ("Binary " ++ colorRel (showBinaryOp op)) ++ " " ++
      showInfoFields [("type", colorType (showMType ty)), ("loc", colorPos (showLoc (getBinaryLoc op)))]]
    ++ (paddingMultiple (buildTree r1) ++ paddingSingle (buildTree r2))
  buildTree (Unary op r ty) = [colorExpr ("Unary " ++ colorRel (show op)) ++ " " ++ showInfoFields [("type", colorType (showMType ty))]] ++ paddingSingle (buildTree r)
  buildTree (Ref op lexpr ty) = [colorExpr "Ref" ++ " " ++ showInfoFields [("type", colorType (showMType ty))]] ++ (paddingMultiple (buildTree op) ++ paddingSingle (buildTree lexpr))
  buildTree (Lit lit ty) = [colorExpr "Lit" ++ " " ++ showInfoFields [("type", colorType (showMType ty))]] ++ paddingSingle (buildTree lit)
  buildTree (FCall id rexprs loc) = [colorExpr ("FCall " ++ showId id) ++ " " ++ showInfoFields [("loc", colorPos (showLoc loc))]] ++ buildTree rexprs
  buildTree (LExpr lexpr ty) = [colorExpr "LExpr" ++ " " ++ showInfoFields [("type", colorType (showMType ty))]] ++ paddingSingle (buildTree lexpr)

instance PrintTree Literal where
  buildTree (LiteralInteger i) = ["Integer " ++ show i]
  buildTree (LiteralDouble d) = ["Double " ++ show d]
  buildTree (LiteralChar c) = ["Char '" ++ [c] ++ "'"]
  buildTree (LiteralString s) = ["String \"" ++ s ++ "\""]
  buildTree Literal_true = ["Bool true"]
  buildTree Literal_false = ["Bool false"]

-- Istanze per i tipi semplici usati in VarInfo
instance PrintTree (Constant () () ()) where
    buildTree (ConstId sign id) = [show sign ++ " " ++ showId (idToEnriched id)]
        where idToEnriched (Ident s l _ _ _) = Ident s l Nothing NoVarInfo NoFunInfo
    buildTree (ConstInt sign i) = [show sign ++ " " ++ show i]
    buildTree (ConstDouble sign d) = [show sign ++ " " ++ show d]
    buildTree (ConstStr s) = ["\"" ++ s ++ "\""]

instance PrintTree Unary where
  buildTree (UnaryPlus loc) = ["UnaryPlus " ++ showLoc loc]
  buildTree (UnaryMinus loc) = ["UnaryMinus " ++ showLoc loc]
  buildTree (UnaryNot loc) = ["UnaryNot " ++ showLoc loc]
  buildTree (UnaryRef loc) = ["UnaryRef " ++ showLoc loc]



{-
------------------------------------------------------
-- PRETTY TAC:
------------------------------------------------------

padLabel = 4
padVar = 3
tiny = 6

padTAC x n
 | n > 0 = padTAC (" " ++ x ) (n-1)
 | n == 0 = x

frameline = "############"

prettyTAC ::  [TAC] -> String
prettyTAC a =  buildTACS a (setBigSmall a (0,0) )

setBigSmall :: [TAC] -> (Int,Int) -> (Int,Int)
setBigSmall [] (big,small) = (big+padLabel,small+padVar)
setBigSmall (x:xs) (big,small) = case x of
  TACAssign  tacAddr _ _ -> setBigSmall xs (big,newSmall)
    where (s,dim) = showTACAddr tacAddr
          newSmall = max small (length s - dim ) 
  TACUnOp tacAddr _ _ _ -> setBigSmall xs (big,newSmall)
    where (s,dim) = showTACAddr tacAddr
          newSmall = max small (length s - dim )      
  TACBinOp tacAddr _ _ _ _ -> setBigSmall xs (big,newSmall)
    where (s,dim) = showTACAddr tacAddr
          newSmall = max small (length s - dim ) 
  TACException _ -> setBigSmall xs (big,newSmall)
    where newSmall = max small 15                          
  TACAddLabel (Label s) ->
    setBigSmall xs ((max big (length s )),small)
  TACStatic tacAddr _ -> 
    setBigSmall xs ((max big (
      length (fst $ showTACAddr  tacAddr) )),small)
  TACReturn _ -> setBigSmall xs (big,(max small 6))
  otherwise  -> setBigSmall xs (big,small)

buildTACS :: [TAC] -> (Int,Int) -> String 
buildTACS [] _ = ""
buildTACS ((TACAddLabel l1):(TACAddLabel l2):xs) y 
  = " " ++lab ++ ":\n" ++ buildTACS ((TACAddLabel l2):xs) y
   where (lab1,dim) = showTACLabel l1
         lab = case dim <= 0 of
           True -> lab1
           otherwise -> "\n "++lab1
buildTACS ((TACAddLabel tacLabel):t:xs) (big,small)
 = " "++lab ++ ":"
   ++ padTAC (buildTAC t small) (big - length lab +dim)
   ++ buildTACS xs (big,small)
    where (lab1,dim) = showTACLabel tacLabel
          lab = case dim <=0 of
            True -> lab1
            otherwise -> "\n "++lab1
buildTACS (TACCode: xs) y 
 = colorGrey("\n"++frameline ++ "    TAC Code    "++frameline++"\n\n")
  ++ buildTACS xs y
buildTACS (TACStaticData:xs) y 
 = colorGrey ("\n" ++ frameline++"   Static Data  "++frameline++"\n\n")
   ++ buildTACS xs y 
buildTACS ((TACStatic tacAddr string):xs) (big,small) 
 = " " ++ addr ++ ":" 
   ++ padTAC string (big - length addr + dim ) ++ "\n"
   ++ buildTACS xs (big,small)
     where (addr,dim) = showTACAddr tacAddr
buildTACS (x:xs) (big,small)  =
   padTAC (buildTAC x small  ) big  ++ buildTACS xs (big,small)

buildTAC :: TAC -> Int -> String  
buildTAC t small = case t of
  TACAssign tacAddr1 type_ tacAddr2
    -> leftAddr ++ padTAC "=" (small - length leftAddr + dim) ++
        (underlineMag assignType) ++
         padTAC (rightAddr)  (tiny - length assignType) ++ "\n"
        where (leftAddr,dim) = showTACAddr tacAddr1
              assignType = showAssignType type_
              (rightAddr,_) = showTACAddr tacAddr2
  TACUnOp tacAddr1 tacOperator type_ tacAddr2
    -> leftAddr ++ padTAC  "=" (small - length leftAddr + dim ) 
       ++(underlineMag (assignType ))  ++
       padTAC   (colorYellow(tacOperator2 ++ assignType)) (tiny - length assignType)
       ++  " "++ rightAddr  ++ "\n"
        where (leftAddr,dim) = showTACAddr tacAddr1
              (rightAddr,_)  = showTACAddr tacAddr2
              assignType = showAssignType type_
              tacOperator2 = case elem 'c' tacOperator of
                True -> tacOperator
                False -> tacOperator ++"_"
  TACBinOp tacAddr1 tacAddr2 tacOperator type_ tacAddr3
    -> leftAddr++padTAC  "=" (small - length leftAddr + dim)
       ++ (underlineMag assignType )
       ++ padTAC (rightAddr1) (tiny -length assignType)  ++ " "
       ++ colorYellow (tacOperator++  "_" ++assignType) 
       ++" "++ rightAddr2++ "\n"
         where (leftAddr,dim) = showTACAddr tacAddr1
               (rightAddr1,_) = showTACAddr tacAddr2
               (rightAddr2,_) = showTACAddr tacAddr3
               assignType = showAssignType type_
  TACGoto tacLabel
    -> "goto " ++lab++ "\n"
      where (lab,_) = showTACLabel tacLabel 
  TACIf tacAddr tacLabel
    -> "if "++ addr ++" goto "
       ++ lab ++ "\n"
      where (addr,_) = showTACAddr tacAddr
            (lab,_) = showTACLabel tacLabel 
  TACIfFalse tacAddr tacLabel
    -> "ifFalse "++addr ++" goto "
        ++lab ++ "\n"
        where (addr,_) = showTACAddr tacAddr
              (lab,_) = showTACLabel tacLabel 
  TACIfRel tacAddr1 rel ty tacAddr2 tacLabel
    -> "ifRel (" ++addr1 ++" "++
       colorYellow (showTACRel rel ++ "_")
       ++(colorYellow (showAssignType ty))++" " ++ addr2 ++
       ") goto " ++lab ++ "\n"
      where (addr1,_) = showTACAddr tacAddr1
            (addr2,_) = showTACAddr tacAddr2
            (lab,_) = showTACLabel tacLabel
  TACParam tacAddr
    -> (underlineMag "param")++" " ++addr ++ "\n"
      where (addr,_) = showTACAddr tacAddr
  TACCallProc tacLabel
    -> (underlineMag "call")++" "++  lab   ++ "\n"
      where (lab,_) = showTACLabel tacLabel 
  TACCallFunc tacAddr ty tacLabel
    -> leftAddr ++ padTAC "=" (small - length leftAddr+dim)
       ++ (underlineMag assignType )
       ++ padTAC (underlineMag "call") (tiny - length assignType) ++ " "++ lab ++ "\n"
         where (leftAddr,dim) = showTACAddr tacAddr
               (lab,_) = showTACLabel tacLabel
               assignType = showAssignType ty
  TACReturn tacAddr
    -> (underlineMag"return") ++" "++ addr ++ "\n"
      where (addr,_) = showTACAddr tacAddr
  TACComment comment
    -> colorGrey("// "++ comment) ++ "\n"
  TACExit -> "exit \n"
  TACAddLabel tacLabel  -> "\n " ++ label ++"\n"
    where (label,_) = showTACLabel tacLabel
  TACException tacLabel -> colorRed "onexceptiongoto " ++ label ++"\n"
    where (label,_) = showTACLabel tacLabel



printState :: State -> String
printState (State tmp lab str env next (tt,ff) (bb,cc) _ _ _ _ _)
  = "State \n tmp = " ++ show tmp ++
    "\n lab = " ++ show lab ++
    "\n str = " ++ show str ++
    "\n env = " ++ showEnv env ++
    "\n next " ++  next1 ++
    "\n tt ff " ++" ("  ++
       tt1 ++","++  ff1 ++ ")"++
    "\n break continue " ++" ("  ++
       bb1 ++","++  cc1 ++ ")"
      where (next1,_) = showTACLabel next
            (tt1,_) = showTACLabel tt
            (ff1,_) = showTACLabel ff
            (bb1,_) = showTACLabel bb
            (cc1,_) = showTACLabel cc
      
showEnv :: Env -> String
showEnv (Env symbols Nothing) =  (showNameMap symbols)
showEnv (Env symbols (Just env)) = (showNameMap symbols) ++
  "\n\t parent: " ++ (showEnv env)

showNameMap :: Map Id ElemAttrs -> String
showNameMap m
  | null m = ""
  | otherwise = showName (toList m)


showName :: [(Id,ElemAttrs)] -> String
showName [] = ""
showName ((id, attr):ss) = "\n\t" ++ id ++
  "\t== " ++ (showAttr attr) ++ (showName ss)

showAttr :: ElemAttrs -> String
showAttr (ExprAttrs addr ty)
   = "{ LExpr  " ++ addr1 ++
     "  :: " ++ showType ty
   where (addr1,_) = showTACAddr addr
showAttr (FuncAttrs callat params ty)
   = "{  callat = " ++ callat1 ++
     "  retty = " ++ showMType ty ++
     "  params = " ++ show params
  where (callat1,_) = showTACLabel callat
-}