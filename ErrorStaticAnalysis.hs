module ErrorStaticAnalysis where

import AbsLinguaggio
import Utils
import PrettyPrinter



tabSpace :: String
tabSpace = "    "

data Error ty
    = RedeclaredVar (Ident ())
    | RedeclaredConst (Ident ())
    | RedeclaredProcFun (Ident ())

    | RedeclaredFunction (Ident ty)
    | RedeclaredParameter (Ident ty)
    | TypeMismatch Loc (Type ty) (Type ty)
    | FunctionNameMismatch Loc Id
    | NonOmogeneusArray Loc
    | NonConstantExpr Loc
    | NonIntegerExpr Loc
    | NonIntegerExprInArrayAccess Loc
    | NotDeclared (Ident ty)
    | DereferenceExcess (Ident ty)
    | NotAnArray Loc
    | PointerArithmetic Loc
    | NonNumericValue Loc
    | ArithmeticError (Type ty) (Type ty) Loc String 
    | DifferentLenght Loc
    | EmptyList Loc
    | NegativeSize Loc
    | NegativeSizeIndex Loc
    | DivideByZero Loc
    | NonBasicType Loc (Type ty)
--  | RelError Loc (Type ty) (Type ty) Rel
    | BoolOpError Loc (Type ty) (Type ty) String
    | BoolNotError Loc (Type ty) String
    | FuncArgsMismatch [Type ty] [Type ty] (Ident ty)
    | FuncTypesMismatch [Type ty] [Type ty] (Ident ty) Loc
    | AssignConstToVar Loc
    | ChangingConstVar (Ident ty)
    | NotInLoop Loc String
    | NonBooleanExpr Loc (Type ty)
    | NonConstPass Loc Int (Ident ty)
    | NonCompExpr (Type ty) (Type ty) Loc
    | NonRefLeft Loc Int (Ident ty)
    | NonEqRefTypes Loc Int (Ident ty) (Type ty) (Type ty)
    | PassingRefOfConst Loc
    | CaseExprNotEq Loc (Type ty)
    | ErrorDeclaration (Ident ty) String
    | FunctionWithoutDimension (Ident ty)
    | NotAnArrayForEach Loc
    | NotIntegerType Loc (Type ty)
    | NotCompileTime Loc
    | IncompatibleExprs Loc (Type ty) (Type ty)
    | FunctionAsIdentifier Loc
    | ReturnInForLoop Loc String
    | CustomError String
    -- warnings
    | WarVarNameUsedForFunc (Ident ty)
    | WarEmptyCase Loc
    | UnsusedValue Loc
    | UnitializedVariable (Ident ty)
    | DeathCode Loc
    | NoMain Loc
    | InfiniteLoop Loc

errorType :: Error ty -> String
errorType x =
    case x of 
        RedeclaredVar (Ident name loc _) -> 
            errorMsg "The identifier '" ++ name ++ "' has already been used for a variable." ++ " [DeclError]."

        RedeclaredConst (Ident name loc _) -> 
            errorMsg "The identifier '" ++ name ++ "' has already been used for a constant." ++ " [DeclError]."

        RedeclaredProcFun (Ident name loc _) -> 
            errorMsg "The identifier '" ++ name ++ "' has already been used for a procedure/function." ++ " [DeclError]."
            
        --RedeclaredFunction (Ident ((loc),name)) -> 
        --    errorMsg "Symbol name '"++ name ++ "' for function declaration already used. " 
        --    ++ (lineAndColumn l c) ++  " [DeclError]."

        --RedeclaredParameter (Ident ((loc),name)) -> 
        --    errorMsg "Symbol name '"++ name ++ "' for parameter declaration already used. " 
        --    ++ (lineAndColumn l c) ++  " [DeclError]."
        
        --TypeMismatch (loc)  ty1 ty2 -> 
        --    errorMsg "Incompatible conversion from '"++ (showType ty1)++
        --    "' to '" ++ (showType ty2)++"'."++ (lineAndColumn l c)  ++  " [TypeError]."
        
        FunctionNameMismatch loc nameBegin ->
            errorMsg "Maybe you meant '" ++ nameBegin ++ "'. The ending name should be equal to the function identifier." ++ " [DeclError]."
        
        NonOmogeneusArray loc ->
            errorMsg "Non homogeneous array initialization."++ showLoc loc  ++ " [TypeError]."
    
        NonConstantExpr loc ->
            errorMsg "Array dimension must be constant and known at compile time in array initialization." ++ " [DeclError]."
        
        NonIntegerExpr loc ->
            errorMsg "Array dimension must be an integer value in array initialization." ++ " [TypeError]."
       
        NonIntegerExprInArrayAccess loc->
            errorMsg "Array access requires an integer value." ++ " [TypeError]."

        --NotDeclared (Ident ((loc),name)) ->
        --    errorMsg "Use of undeclared identifier '"
        --    ++ name ++ "'." ++ (lineAndColumn l c) ++  " [DeclError]."
            
        --DereferenceExcess (Ident ((loc),_)) -> 
        --    errorMsg "Dereferencing requires pointer operand."  
        --    ++ (lineAndColumn l c) ++  " [TypeError]."
        
        NotAnArray loc -> 
            errorMsg "Operator [] requires an Array." ++ " [TypeError]."
            
        PointerArithmetic loc -> 
            errorMsg "Pointer arithmetic is not allowed." ++ " [TypeError]."
            
        NonNumericValue loc -> 
            errorMsg "Required a numerical value." ++ " [TypeError]."
            
        DifferentLenght loc -> 
            errorMsg "All initialization list elements must have the same lenght." ++ " [DeclError]."
            
        EmptyList loc -> 
            errorMsg "Initialization lists must have at least one element." ++ " [DeclError]."
            
        NegativeSize loc -> 
            errorMsg "Array size requires a positive integer." ++ " [TypeError]."

        NegativeSizeIndex loc -> 
            errorMsg "Array index must be a positive integer." ++ " [TypeError]."

        DivideByZero loc -> 
            errorMsg "Division by zero." ++ " [ArithmeticError]."
            
        --NonBasicType (loc) typ-> 
        --    errorMsg "Expected a basic type but found '" ++ showType typ ++ "'. \n"++tabSpace ++ "E.g., 'int', 'real', 'string', 'char' and 'bool'."
        --    ++ (lineAndColumn l c) ++  " [DeclError]."
        
        --ArithmeticError ty1 ty2 (loc) op 
        --    | op == "^" || op == "%" -> errorMsg "Undefined operator '"++ op ++" : "++ showType ty1 ++" x "++ showType ty2 ++" -> Int'.\n"++tabSpace ++ "Maybe you want to use '"++op++": int x int -> int'" ++ (lineAndColumn l c) ++  " [TypeError]."
        --    | op == "/" -> errorMsg "Undefined operator '"++ op ++" : "++ showType ty1 ++" x "++ showType ty2 ++" -> τ'.\n"++tabSpace ++ "Maybe you want to use '"++op++": τ_1 x τ_2 -> real'.\n" ++tabSpace ++"Where τ_1, τ_2 ∈ {'int','real'}.\n"++tabSpace++"More details can be found in the documentation." ++ (lineAndColumn l c) ++  " [TypeError]."
        --    | otherwise -> errorMsg "Undefined operator '"++ op ++" : "++ showType ty1 ++" x "++ showType ty2 ++" -> τ'.\n"++tabSpace ++ "Maybe you want to use '"++op++": τ_1 x τ_2 -> τ3'.\n" ++tabSpace ++"Where τ_1, τ_2 ∈ {'int','real'}  and τ_3 is the max between τ_1 and τ_2.\n"++tabSpace++"More details can be found in the documentation." ++ (lineAndColumn l c) ++  " [TypeError]."
        
        --RelError (loc) ty1 ty2 rel -> 
        --    errorMsg "Undefined operator '"++ showRel rel ++" : "++ showType ty1 ++" x "++ showType ty2 ++" -> Bool'."++{-\n++tabSpace ++ "Expected 2 numerical values, e.g., 'int' or 'real'."++ -}(lineAndColumn l c) ++  " [TypeError]."
            
        --BoolOpError (loc) ty1 ty2 op -> errorMsg "Undefined operator '"++ op ++" : "++ showType ty1 ++" x "++ showType ty2 ++" -> Bool'.\n"++tabSpace ++ "Expected 2 boolean values."++ (lineAndColumn l c) ++  " [TypeError]."
        
        --BoolNotError (loc) ty1 op -> errorMsg "Undefined operator '"++ op ++" : "++ showType ty1 ++" -> Bool'.\n"++tabSpace ++ "Expected a boolean value."++ (lineAndColumn l c) ++  " [TypeError]."
        
        --FuncArgsMismatch exprs params (Ident ((loc),_)) ->
        --    errorMsg "Mismatch in number of arguments. "++ (lineAndColumn l c) ++  " [TypeError].\n"++ tabSpace ++"The call function has "++ show( length exprs) ++" arguments while " ++ show (length params)++ " are required." 
            
        --FuncTypesMismatch exprs params (Ident ((loc),name)) (ld, cd)->
        --    errorMsg "Mismatch in argument types."++ (lineAndColumn l c) ++  " [TypeError].\n" ++ tabSpace ++ "In function call: '" ++ (showFunction ( name ++ "(" ) exprs)++ "'.\n"++tabSpace ++
        --        "Function definition found at <l:" ++ (show ld) ++ ",c:" ++ (show cd) ++ "> with signature: '" ++ ( showFunction (name++"(") params) ++
        --        "'."
        AssignConstToVar loc ->
            errorMsg "You cannot assign the address of a constant variable to a variable." ++ " [TypeError]."

        PassingRefOfConst loc ->
            errorMsg "Passing the address of a const qualified variable." ++ " [TypeError]."
        
        --ChangingConstVar (Ident ((loc),name)) ->
        --    errorMsg "Cannot change the value of the const variable '"++ name ++ "'."
        --    ++ (lineAndColumn l c) ++  " [TypeError]."
            
        NotInLoop loc stmt ->
            errorMsg  "'" ++ stmt ++ "' statement not in a loop statement." ++ " [ContextError]."
        
        --NonBooleanExpr (loc)  ty-> 
        --    errorMsg "Expected a boolean expression but found '" ++ showType ty  ++ "'."
        --    ++ (lineAndColumn l c) ++  " [TypeError]."
        
        --NonConstPass (loc) parNum (Ident (_,name)) -> errorMsg "Cannot pass a const variable if the formal parameter is  not 'const' qualified.\n"++ tabSpace ++ "The "++(show parNum)  ++ "° parameter of the function '"++name++ "' is not const qualified." ++ (lineAndColumn l c) ++  " [TypeError]."
        
        --NonRefLeft (loc) parNum (Ident (_,name)) -> errorMsg "The "++(show parNum)  ++ "° parameter of the function '"++name++ "' requires a Left Expression." ++ (lineAndColumn l c) ++  " [TypeError]."
        
        --NonEqRefTypes (loc) parNum (Ident (_,name)) typReq typFound -> errorMsg "The "++(show parNum)  ++ "° parameter of the function '"++name++ "' requires exactly the type '" ++ showType typReq++ "' but found '" ++ showType typFound ++"'." ++ (lineAndColumn l c) ++  " [TypeError]."

        --NonCompExpr baseType exprType (loc) -> errorMsg "Non compatible types in 'case' statement. Expected '" ++ showType baseType  ++ "' but found '" ++ showType exprType ++ "'." ++ (lineAndColumn l c) ++  " [TypeError]."
        
        --CaseExprNotEq (loc) ty->errorMsg "Illegal expression of type '" ++ showType ty ++ "' inside the statement 'case'.\n"++
        --    tabSpace ++ "The operator '= :"++showType ty++ " x "++ showType ty ++ " -> bool' is not defined." 
        --    ++ (lineAndColumn l c) ++  " [TypeError]."

        --ErrorDeclaration (Ident ((loc),_)) kind -> errorMsg kind++" declaration must be also definition."
        --    ++ (lineAndColumn l c) ++  " [DeclError]."

        --FunctionWithoutDimension (Ident ((loc),name)) -> errorMsg "The array dimension for the parameter '"++ name ++ "' must be specified."
        --    ++ (lineAndColumn l c) ++  " [TypeError]."

        NotAnArrayForEach loc ->
            errorMsg "The 'foreach' statement requires an array type." ++ " [TypeError]."

        --NotIntegerType (loc) ty -> errorMsg "Expected an integer value but found '" ++ showType ty ++"'." ++ (lineAndColumn l c) ++  " [TypeError]."

        NotCompileTime loc ->
            errorMsg "The expression must be evaluable at compile time. " ++ " [TypeError]."

        --IncompatibleExprs (loc) ty1 ty2 -> errorMsg "The two types '"++ showType ty1 ++ "' and '"++ showType ty2 ++ "''  in '("++ showType ty1 ++ ") if (·) else ("++  showType ty2 ++")' must coincide."++ (lineAndColumn l c) ++  " [TypeError]."

        FunctionAsIdentifier loc ->
            errorMsg "Cannot use a function as a variable." ++ " [TypeError]."

        ReturnInForLoop loc stmt ->
            errorMsg "Cannot use the statement '" ++ stmt ++ "' inside a deterministic 'for' loop. " ++ " [ControlFlowAnalysis]."

        CustomError msg -> errorMsg msg

-------------------------------------------------
------------------ WARNINGS ---------------------
-------------------------------------------------

        WarVarNameUsedForFunc (Ident name loc _) ->
            warningMsg "The identifier '" ++ name ++ "' has already been used for a function." ++ " [DeclError]."


        WarEmptyCase loc ->
            warningMsg "Empty 'case' statement." ++ " [ControlFlowAnalysis]."

        UnsusedValue loc ->
            warningMsg "Expression result unused." ++ " [ControlFlowAnalysis]."  

        --UnitializedVariable (Ident ((loc),name)) -> warningMsg "The return variable '"++ name ++"' may be never initialized."++ (lineAndColumn l c) ++  " [ControlFlowAnalysis]." 

        DeathCode loc ->
            warningMsg "Dead code after 'return' statement." ++ " [ControlFlowAnalysis]."
        
        NoMain loc ->
            warningMsg "Declaration of 'main' not found."

        InfiniteLoop loc ->
            warningMsg "Infinite loop." ++ " [ControlFlowAnalysis]."