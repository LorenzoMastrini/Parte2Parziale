{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module AbsLinguaggio where

import Prelude (Char, Double, Integer, String, Int, Bool, Maybe)
import qualified Prelude as C (Eq, Ord, Show, Read)



--------------------------------------------------------------
-------------------- ABSTRACT SYNTAX TREE --------------------
--------------------------------------------------------------

data Program ty varInfo funInfo
  = Prog (Ident ty varInfo funInfo) [FormalParameters ty varInfo funInfo] (Block ty varInfo funInfo) Loc
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Block ty varInfo funInfo
  = Block [ConstantDefinition ty varInfo funInfo] [VariableDeclaration ty varInfo funInfo] [ProcedureAndFunctionDeclaration ty varInfo funInfo] (CompoundStatement ty varInfo funInfo)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data ConstantDefinition ty varInfo funInfo
  = ConstDef (Ident ty varInfo funInfo) (Constant ty varInfo funInfo)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data VariableDeclaration ty varInfo funInfo
  = VarDecl [Ident ty varInfo funInfo] (Type ty varInfo funInfo)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data ProcedureAndFunctionDeclaration ty varInfo funInfo
  = ProcFunDecl (ProcedureAndFunctionHeading ty varInfo funInfo) (Block ty varInfo funInfo)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data ProcedureAndFunctionHeading ty varInfo funInfo
  = ProcHead (Ident ty varInfo funInfo) [FormalParameters ty varInfo funInfo]
  | FunHead (Ident ty varInfo funInfo) [FormalParameters ty varInfo funInfo] (Type ty varInfo funInfo) Loc
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data FormalParameters ty varInfo funInfo
  = FormParam OptModality [Ident ty varInfo funInfo] (Type ty varInfo funInfo)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data OptModality
  = ByVal
  | ByRef
  | ByValRes
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data CompoundStatement ty varInfo funInfo
  = CompStmt [Statement ty varInfo funInfo]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Statement ty varInfo funInfo
  = Stmts (CompoundStatement ty varInfo funInfo)
  | RExprStmt (RExpr ty varInfo funInfo) ty
  | Assign (LExpr ty varInfo funInfo) (RExpr ty varInfo funInfo) ty Loc
  | While (RExpr ty varInfo funInfo) (Statement ty varInfo funInfo) Loc
  | DoWhile (Statement ty varInfo funInfo) (RExpr ty varInfo funInfo) Loc
  | ForTo (LExpr ty varInfo funInfo) (RExpr ty varInfo funInfo) (RExpr ty varInfo funInfo) (Statement ty varInfo funInfo)
  | If (RExpr ty varInfo funInfo) (Statement ty varInfo funInfo) (OptElse ty varInfo funInfo) Loc
  | Case (RExpr ty varInfo funInfo) [CaseItem ty varInfo funInfo]
  | ReturnStmt (RExpr ty varInfo funInfo) ty Loc
  | BreakStmt Loc
  | ContinueStmt Loc
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data OptElse ty varInfo funInfo
  = NoElse
  | Else (Statement ty varInfo funInfo)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data CaseItem ty varInfo funInfo
  = CaseItem (Constant ty varInfo funInfo) (Statement ty varInfo funInfo)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Constant ty varInfo funInfo
  = ConstId OptSign (Ident ty varInfo funInfo)
  | ConstInt OptSign Integer
  | ConstDouble OptSign Double
  | ConstStr String
  | ConstChar Char
  | ConstBool Bool
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data OptSign
  = NoOptSign
  | OptSignPlus
  | OptSignMinus
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data RExpr ty varInfo funInfo
  = Binary (RExpr ty varInfo funInfo) Binary (RExpr ty varInfo funInfo) ty
  | Unary Unary (RExpr ty varInfo funInfo) ty
  | Lit Literal ty
  | FCall (Ident ty varInfo funInfo) [RExpr ty varInfo funInfo] Loc
  | Ref Unary (RExpr ty varInfo funInfo) ty
  | LExpr (LExpr ty varInfo funInfo) ty
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data LExpr ty varInfo funInfo
  = LId (Ident ty varInfo funInfo) ty
  | LArr (LExpr ty varInfo funInfo) [RExpr ty varInfo funInfo] ty Loc
  | LDeref (LExpr ty varInfo funInfo) ty Loc
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Ident ty varInfo funInfo
  = Ident String Loc ty varInfo funInfo
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Binary
  = BinaryPlus Loc
  | BinaryMinus Loc
  | BinaryMul Loc
  | BinaryDiv Loc
  | BinaryIntDiv Loc
  | BinaryMod Loc
  | BinaryOr Loc
  | BinaryAnd Loc
  | BinaryEq Loc
  | BinaryNeq Loc
  | BinaryLt Loc
  | BinaryLte Loc
  | BinaryGt Loc
  | BinaryGte Loc
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Unary
  = UnaryPlus Loc
  | UnaryMinus Loc
  | UnaryNot Loc
  | UnaryRef Loc
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Literal
  = LiteralInteger Integer
  | LiteralDouble Double
  | LiteralChar Char
  | LiteralString String
  | Literal_true
  | Literal_false
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Type ty varInfo funInfo
  = Array [IndexType ty varInfo funInfo] (Type ty varInfo funInfo)
  | Pointer (Type ty varInfo funInfo)
  | BasicType BasicType
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data IndexType ty varInfo funInfo
  = IndexRange (Constant ty varInfo funInfo) (Constant ty varInfo funInfo)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data BasicType
  = Integer
  | Real
  | Boolean
  | Character
  | String
  | Void
  deriving (C.Eq, C.Ord, C.Show, C.Read)



------------------------------------------------------
-------------------- ENRICHED AST --------------------
------------------------------------------------------

-- Versions of each node without the added information
type SimpleProgram                         = Program () () ()
type SimpleFormalParameters                = FormalParameters () () ()
type SimpleBlock                           = Block () () ()
type SimpleConstantDefinition              = ConstantDefinition () () ()
type SimpleVariableDeclaration             = VariableDeclaration () () ()
type SimpleConstant                        = Constant () () ()
type SimpleProcedureAndFunctionDeclaration = ProcedureAndFunctionDeclaration () () ()
type SimpleProcedureAndFunctionHeading     = ProcedureAndFunctionHeading () () ()
type SimpleCompoundStatement               = CompoundStatement () () ()
type SimpleStatement                       = Statement () () ()
type SimpleOptElse                         = OptElse () () ()
type SimpleRExpr                           = RExpr () () ()
type SimpleLExpr                           = LExpr () () ()
type SimpleIdent                           = Ident () () ()
type SimpleType                            = Type () () ()
type SimpleIndexType                       = IndexType () () ()

-- Type aliases for the enriched AST nodes
type EnrichedProgram                          = Program (Maybe SimpleType) VarInfo FunInfo
type EnrichedFormalParameters                 = FormalParameters (Maybe SimpleType) VarInfo FunInfo
type EnrichedBlock                            = Block (Maybe SimpleType) VarInfo FunInfo
type EnrichedConstantDefinition               = ConstantDefinition (Maybe SimpleType) VarInfo FunInfo
type EnrichedVariableDeclaration              = VariableDeclaration (Maybe SimpleType) VarInfo FunInfo
type EnrichedProcedureAndFunctionDeclaration  = ProcedureAndFunctionDeclaration (Maybe SimpleType) VarInfo FunInfo
type EnrichedProcedureAndFunctionHeading      = ProcedureAndFunctionHeading (Maybe SimpleType) VarInfo FunInfo
type EnrichedCompoundStatement                = CompoundStatement (Maybe SimpleType) VarInfo FunInfo
type EnrichedStatement                        = Statement (Maybe SimpleType) VarInfo FunInfo
type EnrichedOptElse                          = OptElse (Maybe SimpleType) VarInfo FunInfo
type EnrichedCaseItem                         = CaseItem (Maybe SimpleType) VarInfo FunInfo
type EnrichedRExpr                            = RExpr (Maybe SimpleType) VarInfo FunInfo
type EnrichedLExpr                            = LExpr (Maybe SimpleType) VarInfo FunInfo
type EnrichedIdent                            = Ident (Maybe SimpleType) VarInfo FunInfo
type EnrichedType                             = Type (Maybe SimpleType) VarInfo FunInfo
type EnrichedIndexType                        = IndexType (Maybe SimpleType) VarInfo FunInfo
type EnrichedConstant                         = Constant (Maybe SimpleType) VarInfo FunInfo

-- Definition of the position of the tokens
data Loc = Loc {line, column :: Int}
  deriving (C.Eq, C.Ord, C.Show, C.Read)

-- Variable informations for the enriched AST
data VarInfo =
  VarInfo {
    init :: Maybe (Constant () () ()),  -- Value of a constant known at compile time (if present)
    const :: Bool                       -- True if it's a constant
  }
  | NoVarInfo
  deriving (C.Eq, C.Ord, C.Show)

-- Function informations for the enriched AST
data FunInfo =
  FunInfo {
    retType :: SimpleType,              -- Function return type
    params :: [SimpleFormalParameters]  -- List of parameters
  }
  | NoFunInfo
  deriving (C.Eq, C.Show)