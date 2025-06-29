module PrettyPrinterTAC where

import qualified TACabssynt
import AbsLinguaggio

-- Color helpers
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

-- Indentation helper for TAC format (12 spaces like in TACOutput.txt)
tacIndent :: String
tacIndent = replicate 12 ' '

-- Label formatting helper
formatLabel :: Maybe TACabssynt.TACLabel -> String
formatLabel Nothing = tacIndent
formatLabel (Just label) = "l" ++ show label ++ ":" ++ replicate (max 0 (8 - length (show label))) ' '

-- Pretty print Address
prettyAddress :: TACabssynt.Address -> String
prettyAddress addr = case addr of
  TACabssynt.ProgVar varInfo _ -> 
    let ident = TACabssynt.varName varInfo
        name = case ident of
          AbsLinguaggio.Ident nameStr _ _ _ _ -> nameStr
        loc = case ident of
          AbsLinguaggio.Ident _ loc _ _ _ -> loc
        line = AbsLinguaggio.line loc
        column = AbsLinguaggio.column loc
    in colorBlue name ++ colorGrey ("(" ++ show line ++ "," ++ show column ++ ")")
  TACabssynt.TACLit lit _ -> case lit of
    LiteralInteger i -> colorYellow (show i)
    LiteralDouble d -> colorYellow (show d)
    LiteralChar c -> colorMagenta ("'" ++ [c] ++ "'")
    LiteralString s -> colorMagenta ("\"" ++ s ++ "\"")
    Literal_true -> colorMagenta "true"
    Literal_false -> colorMagenta "false"
  TACabssynt.Temporary n _ -> colorCyan ("t" ++ show n)

-- Pretty print Binary operator
prettyBinary :: Binary -> String
prettyBinary bin = case bin of
  BinaryPlus _ -> "+"
  BinaryMinus _ -> "-"
  BinaryMul _ -> "*"
  BinaryDiv _ -> "/"
  BinaryIntDiv _ -> "div"
  BinaryMod _ -> "mod"
  BinaryOr _ -> "or"
  BinaryAnd _ -> "and"
  BinaryEq _ -> "=="
  BinaryNeq _ -> "!="
  BinaryLt _ -> "<"
  BinaryLte _ -> "<="
  BinaryGt _ -> ">"
  BinaryGte _ -> ">="

-- Pretty print Unary operator
prettyUnary :: Unary -> String
prettyUnary unary = case unary of
  UnaryPlus _ -> "+"
  UnaryMinus _ -> "-"
  UnaryNot _ -> "not"
  UnaryRef _ -> "&"

-- Pretty print TACabssynt.TAC with TACOutput.txt format
prettyTACabssynt :: [TACabssynt.TAC TACabssynt.TACLabel] -> String
prettyTACabssynt = unlines . map prettyTACabssyntInstr

-- Estrae il tipo principale di una istruzione TAC (se presente)
getTACInstrType :: TACabssynt.TAC TACabssynt.TACLabel -> Maybe TACabssynt.TACType
getTACInstrType tac = case tac of
  TACabssynt.TACBinary dst _ _ _ _ -> Just (getAddrType dst)
  TACabssynt.TACUnary dst _ _ _ -> Just (getAddrType dst)
  TACabssynt.TACNullary dst _ _ -> Just (getAddrType dst)
  TACabssynt.TACIndexedLoad dst _ _ -> Just (getAddrType dst)
  TACabssynt.TACAddrOf dst _ -> Just (getAddrType dst)
  TACabssynt.TACAssign dst _ -> Just (getAddrType dst)
  TACabssynt.TACDerefRead dst _ -> Just (getAddrType dst)
  TACabssynt.TACReturnVal addr -> Just (getAddrType addr)
  TACabssynt.TACParam addr -> Just (getAddrType addr)
  TACabssynt.TACFCall addr _ _ -> Just (getAddrType addr)
  _ -> Nothing
  where
    getAddrType :: TACabssynt.Address -> TACabssynt.TACType
    getAddrType a = case a of
      TACabssynt.ProgVar _ t -> t
      TACabssynt.TACLit _ t -> t
      TACabssynt.Temporary _ t -> t

-- Modifica: aggiungi il tipo SOLO a fine riga, mai dopo '='
prettyTACabssyntInstr :: TACabssynt.TAC TACabssynt.TACLabel -> String
prettyTACabssyntInstr tac = 
  let base = case tac of
        TACabssynt.TACBinary dst binOp _ src1 src2 ->
          formatLabel Nothing ++ 
          prettyAddress dst ++ 
          replicate (max 0 (12 - length (prettyAddress dst))) ' ' ++
          " = " ++ prettyAddress src1 ++ " " ++ prettyBinary binOp ++ " " ++ prettyAddress src2
        TACabssynt.TACUnary dst unaryOp _ src ->
          formatLabel Nothing ++ 
          prettyAddress dst ++ 
          replicate (max 0 (12 - length (prettyAddress dst))) ' ' ++
          " = " ++ prettyUnary unaryOp ++ " " ++ prettyAddress src
        TACabssynt.TACNullary dst _ src ->
          formatLabel Nothing ++ 
          prettyAddress dst ++ 
          replicate (max 0 (12 - length (prettyAddress dst))) ' ' ++
          " = " ++ prettyAddress src
        TACabssynt.TACGoto label ->
          formatLabel Nothing ++ 
          colorCyan "goto " ++ colorMagenta (show label)
        TACabssynt.TACIf cond label ->
          formatLabel Nothing ++ 
          colorRed "if " ++ prettyAddress cond ++ colorRed " goto " ++ colorMagenta (show label)
        TACabssynt.TACIfFalse cond label ->
          formatLabel Nothing ++ 
          colorRed "ifz " ++ prettyAddress cond ++ colorRed " goto " ++ colorMagenta (show label)
        TACabssynt.TACIfRel cond binOp addr label ->
          formatLabel Nothing ++ 
          colorRed "if " ++ prettyAddress cond ++ " " ++ prettyBinary binOp ++ " " ++ prettyAddress addr ++ colorRed " goto " ++ colorMagenta (show label)
        TACabssynt.TACIndexedLoad dst varInfo addr ->
          formatLabel Nothing ++ 
          prettyAddress dst ++ 
          replicate (max 0 (12 - length (prettyAddress dst))) ' ' ++
          " = " ++ prettyAddress (TACabssynt.ProgVar varInfo TACabssynt.TACError) ++ "[" ++ prettyAddress addr ++ "]"
        TACabssynt.TACIndexedStore varInfo addr src ->
          formatLabel Nothing ++ 
          prettyAddress (TACabssynt.ProgVar varInfo TACabssynt.TACError) ++ "[" ++ prettyAddress addr ++ "]" ++
          replicate (max 0 (12 - length (prettyAddress (TACabssynt.ProgVar varInfo TACabssynt.TACError) ++ "[" ++ prettyAddress addr ++ "]"))) ' ' ++
          " = " ++ prettyAddress src
        TACabssynt.TACAddrOf dst varInfo ->
          formatLabel Nothing ++ 
          prettyAddress dst ++ 
          replicate (max 0 (12 - length (prettyAddress dst))) ' ' ++
          " = &" ++ prettyAddress (TACabssynt.ProgVar varInfo TACabssynt.TACError)
        TACabssynt.TACAssign dst src ->
          formatLabel Nothing ++ 
          prettyAddress dst ++ 
          replicate (max 0 (12 - length (prettyAddress dst))) ' ' ++
          " = " ++ prettyAddress src
        TACabssynt.TACDerefRead dst addr ->
          formatLabel Nothing ++ 
          prettyAddress dst ++ 
          replicate (max 0 (12 - length (prettyAddress dst))) ' ' ++
          " = *" ++ prettyAddress addr
        TACabssynt.TACDerefWrite addr src ->
          formatLabel Nothing ++ 
          "*" ++ prettyAddress addr ++ 
          replicate (max 0 (12 - length ("*" ++ prettyAddress addr))) ' ' ++
          " = " ++ prettyAddress src
        TACabssynt.TACParam operand ->
          formatLabel Nothing ++ 
          colorCyan "param " ++ prettyAddress operand
        TACabssynt.TACPCall label numArgs ->
          formatLabel Nothing ++ 
          colorCyan "pcall " ++ colorMagenta (show label) ++ " " ++ show numArgs
        TACabssynt.TACFCall funcAddr label numArgs ->
          formatLabel Nothing ++ 
          colorCyan "fcall " ++ prettyAddress funcAddr ++ " " ++ colorMagenta (show label) ++ " " ++ show numArgs
        TACabssynt.TACReturn ->
          formatLabel Nothing ++ 
          colorGreen "return"
        TACabssynt.TACReturnVal operand ->
          formatLabel Nothing ++ 
          colorGreen "return " ++ prettyAddress operand
        TACabssynt.TACAddLabel label ->
          colorMagenta (show label ++ ":")
      tipo = getTACInstrType tac
      tipoStr = maybe "" (\t -> "   [" ++ show t ++ "]") tipo
  in base ++ tipoStr

-- Pretty-printer TAC
prettyTACabssyntEnhanced :: [TACabssynt.TAC TACabssynt.TACLabel] -> String
prettyTACabssyntEnhanced instructions = 
  "TAC Generator:\n" ++
  prettyTACabssynt instructions ++
  "\n"

-- Print on stdout
dumpTAC :: [TACabssynt.TAC TACabssynt.TACLabel] -> IO ()
dumpTAC tacList = putStrLn (prettyTACabssyntEnhanced tacList) 