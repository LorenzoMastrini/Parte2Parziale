module PrettyPrinterTAC where

import TAC
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

-- Pretty print Operand
prettyOperand :: Operand -> String
prettyOperand op = case op of
  TAC.Temp n        -> colorCyan ("t" ++ show n)
  TAC.ConstInt i    -> colorYellow (show i)
  TAC.ConstDouble d -> colorYellow (show d)
  TAC.ConstChar c   -> colorMagenta ("'" ++ [c] ++ "'")
  TAC.ConstBool b   -> colorMagenta (show b)
  TAC.Var s         -> colorBlue s

-- Safe init and last for non-empty lists
safeInit :: [a] -> [a]
safeInit [] = []
safeInit xs = take (length xs - 1) xs

safeLast :: [a] -> a
safeLast [] = error "safeLast: empty list"
safeLast xs = xs !! (length xs - 1)

-- Pretty print a list of operands
prettyOperands :: [Operand] -> String
prettyOperands ops = case ops of
  [] -> ""
  [x] -> prettyOperand x
  xs -> concatMap (\o -> prettyOperand o ++ ", ") (safeInit xs) ++ prettyOperand (safeLast xs)

-- Pretty print TAC
prettyTAC :: [TAC] -> String
prettyTAC = unlines . map prettyTACInstr

prettyTACInstr :: TAC -> String
prettyTACInstr tac = case tac of
  TacBinary dst op src1 src2 ->
    colorGreen (prettyOperand dst) ++ " = " ++ colorYellow op ++ " " ++ prettyOperand src1 ++ ", " ++ prettyOperand src2
  TacUnary op src dst ->
    colorGreen (prettyOperand dst) ++ " = " ++ colorYellow op ++ " " ++ prettyOperand src
  TacAssign dst src ->
    colorGreen (prettyOperand dst) ++ " = " ++ prettyOperand src
  TacLabel lbl ->
    colorMagenta (lbl ++ ":")
  TacGoto lbl ->
    colorCyan ("goto ") ++ colorMagenta lbl
  TacIfZ cond lbl ->
    colorRed ("ifz ") ++ prettyOperand cond ++ colorRed (" goto ") ++ colorMagenta lbl
  TacReturn op ->
    colorGreen "return " ++ prettyOperand op
  TacUnimplemented msg ->
    colorRed "[Unimplemented] " ++ colorGrey msg
  TacCall fun args dst loc ->
    colorGreen (prettyOperand dst) ++ " = " ++ colorCyan "call " ++ prettyOperand fun ++ "(" ++ prettyOperands args ++ ") " ++ colorGrey (defaultShowLoc loc)
  TacArrayAccess dst base indices ->
    colorGreen (prettyOperand dst) ++ " = " ++ prettyOperand base ++ "[" ++ prettyOperands indices ++ "]"
  TacDeref dst addr ->
    colorGreen (prettyOperand dst) ++ " = *" ++ prettyOperand addr
  TacComment msg ->
    colorGrey ("// " ++ msg)
  TacArg op ->
    colorCyan "arg " ++ prettyOperand op
