module PrettyPrinterTAC where

import qualified TACabssynt
import AbsLinguaggio

-- Color helpers (copied from PrettyPrinter.hs)
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

-- Pretty print Loc
defaultShowLoc :: Loc -> String
defaultShowLoc (Loc l c) = "(line: " ++ show l ++ ", column: " ++ show c ++ ")"

-- Pretty print Address
prettyAddress :: TACabssynt.Address -> String
prettyAddress addr = case addr of
  TACabssynt.ProgVar varInfo _ -> colorBlue (show (TACabssynt.varName varInfo))
  TACabssynt.TacLit lit _ -> case lit of
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

-- Pretty print TACabssynt.TAC
prettyTACabssynt :: [TACabssynt.TAC TACabssynt.TACLabel] -> String
prettyTACabssynt = unlines . map prettyTACabssyntInstr

prettyTACabssyntInstr :: TACabssynt.TAC TACabssynt.TACLabel -> String
prettyTACabssyntInstr tac = case tac of
  TACabssynt.TACBinaryAss dst binOp basicType src1 src2 ->
    colorGreen (prettyAddress dst) ++ " = " ++ colorYellow (prettyBinary binOp) ++ " " ++ prettyAddress src1 ++ ", " ++ prettyAddress src2
  TACabssynt.TACUnaryAss dst unaryOp basicType src ->
    colorGreen (prettyAddress dst) ++ " = " ++ colorYellow (prettyUnary unaryOp) ++ " " ++ prettyAddress src
  TACabssynt.TACNullaryAss dst basicType src ->
    colorGreen (prettyAddress dst) ++ " = " ++ prettyAddress src
  TACabssynt.TACGoto label ->
    colorCyan ("goto ") ++ colorMagenta (show label)
  TACabssynt.TACIf cond label ->
    colorRed ("if ") ++ prettyAddress cond ++ colorRed (" goto ") ++ colorMagenta (show label)
  TACabssynt.TACIfFalse cond label ->
    colorRed ("ifz ") ++ prettyAddress cond ++ colorRed (" goto ") ++ colorMagenta (show label)
  TACabssynt.TACIfRel cond binOp addr label ->
    colorRed ("if ") ++ prettyAddress cond ++ " " ++ prettyBinary binOp ++ " " ++ prettyAddress addr ++ colorRed (" goto ") ++ colorMagenta (show label)
  TACabssynt.TACIndexedLoad dst varInfo addr ->
    colorGreen (prettyAddress dst) ++ " = " ++ show (TACabssynt.varName varInfo) ++ "[" ++ prettyAddress addr ++ "]"
  TACabssynt.TACIndexedStore varInfo addr src ->
    show (TACabssynt.varName varInfo) ++ "[" ++ prettyAddress addr ++ "] = " ++ prettyAddress src
  TACabssynt.TACAddrOf dst varInfo ->
    colorGreen (prettyAddress dst) ++ " = &" ++ show (TACabssynt.varName varInfo)
  TACabssynt.TACAssign dst src ->
    colorGreen (prettyAddress dst) ++ " = " ++ prettyAddress src
  TACabssynt.TACDerefRead dst addr ->
    colorGreen (prettyAddress dst) ++ " = *" ++ prettyAddress addr
  TACabssynt.TACDerefWrite addr src ->
    "*" ++ prettyAddress addr ++ " = " ++ prettyAddress src
  TACabssynt.TACParam operand ->
    colorCyan "param " ++ prettyAddress operand
  TACabssynt.TACPCall label numArgs ->
    colorCyan "pcall " ++ colorMagenta (show label) ++ " " ++ show numArgs
  TACabssynt.TACFCall funcAddr label numArgs ->
    colorCyan "fcall " ++ prettyAddress funcAddr ++ " " ++ colorMagenta (show label) ++ " " ++ show numArgs
  TACabssynt.TACReturn ->
    colorGreen "return"
  TACabssynt.TACReturnVal operand ->
    colorGreen "return " ++ prettyAddress operand
  TACabssynt.TACAddLabel label ->
    colorMagenta (show label ++ ":")
