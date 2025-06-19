{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The abstract syntax of language linguaggio.

module AbsLinguaggio where

import Prelude (Char, Double, Integer, String, Int)
import qualified Prelude as C (Eq, Ord, Show, Read)
import qualified Data.String



-- Definition of the position of the tokens
data Loc = Loc {line, column :: Int}
  deriving (C.Eq, C.Ord, C.Show, C.Read)


data Program ty
  = Prog (Ident ty) [FormalParameters ty] (Block ty)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Block ty
  = Block [ConstantDefinition ty] [VariableDeclaration ty] [ProcedureAndFunctionDeclaration ty] (CompoundStatement ty)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data ConstantDefinition ty
  = ConstDef (Ident ty) (Constant ty)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data VariableDeclaration ty
  = VarDecl [Ident ty] (Type ty)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data ProcedureAndFunctionDeclaration ty
    = ProcFunDecl (ProcedureAndFunctionHeading ty) (Block ty)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data ProcedureAndFunctionHeading ty
    = ProcHead (Ident ty) [FormalParameters ty]
    | FunHead (Ident ty) [FormalParameters ty] (Type ty)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data FormalParameters ty
  = FormParam OptModality [Ident ty] (Type ty)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data OptModality = ByVal | ByRef | ByValRes
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Type ty
  = Array [IndexType ty] (Type ty)
  | Pointer (Type ty)
  | BasicType BasicType
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data BasicType
    = Integer
    | Real
    | Boolean
    | Character
    | String
    | Void
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data IndexType ty
  = IndexRange (Constant ty) (Constant ty)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Constant ty
  = ConstId OptSign (Ident ty)
  | ConstInt OptSign Integer
  | ConstDouble OptSign Double
  | ConstStr String
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data OptSign
  = NoOptSign
  | OptSignPlus
  | OptSignMinus
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data CompoundStatement ty
  = CompStmt [Statement ty]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Statement ty
    = Stmts (CompoundStatement ty)
    | RExprStmt (RExpr ty)
    | Assign (LExpr ty) (RExpr ty)
    | While (RExpr ty) (Statement ty)
    | DoWhile (Statement ty) (RExpr ty)
    | ForTo (LExpr ty) (RExpr ty) (RExpr ty) (Statement ty)
    | ForDownTo (LExpr ty) (RExpr ty) (RExpr ty) (Statement ty)
    | If (RExpr ty) (Statement ty) (OptElse ty)
    | Case (RExpr ty) [CaseItem ty]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data OptElse ty
  = NoElse
  | Else (Statement ty)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data CaseItem ty
  = CaseItem (Constant ty) (Statement ty)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data LExpr ty =
  LId (Ident ty)
  | LArr (LExpr ty) [RExpr ty]
  | LDeref (LExpr ty)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data RExpr ty
    = Binary (RExpr ty) Binary (RExpr ty)
    | Unary Unary (RExpr ty)
    | Lit Literal
    | FCall (Ident ty) [RExpr ty]
    | LExpr (LExpr ty)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Literal
    = LiteralInteger Integer
    | LiteralDouble Double
    | LiteralChar Char
    | LiteralString String
    | Literal_true
    | Literal_false
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Binary
    = BinaryPlus
    | BinaryMinus
    | BinaryMul
    | BinaryDiv
    | BinaryIntDiv
    | BinaryMod
    | BinaryOr
    | BinaryAnd
    | BinaryEq
    | BinaryNeq
    | BinaryLt
    | BinaryLte
    | BinaryGt
    | BinaryGte
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Unary
  = UnaryPlus
  | UnaryMinus
  | UnaryNot
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Ident ty
  = Ident String Loc ty
  deriving (C.Eq, C.Ord, C.Show, C.Read)