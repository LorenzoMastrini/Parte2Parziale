module ErrorStaticAnalysis where

import AbsLinguaggio
import PrettyPrinter


data Error ty varInfo funInfo
    = RedeclaredVar SimpleIdent
    | RedeclaredConst SimpleIdent
    | NotConstantValue String
    | RedeclaredProcFun SimpleIdent
    | FunNotDefined SimpleIdent
    | NotAFunction SimpleIdent
    | FunArgsNumMismatch Int Int 
    | UnknownIdentifier SimpleIdent
    | IncompatibleArrPtrType
    | IncompatiblePtrArrType
    | IncompatibleTypes String SimpleType SimpleType
    | ComparisonTypeMismatch String SimpleType SimpleType
    | NotIntegerType String SimpleType
    | NotBooleanType String SimpleType
    | NotNumericType String SimpleType
    | IncompatibleAssignment SimpleType SimpleType
    | NotBooleanOperation String SimpleType SimpleType
    | FunArgTypeMismatch SimpleType SimpleType
    | NotLExpr Unary
    | NotAPointer
    | NotAnArray
    | OutOfBound [Integer] Loc
    | NegativeArrIndex
    | AssignmentToConstant
    | NotBooleanCondition (Maybe SimpleType)
    -- Warnings
    | WarVarNameUsedForFunc (Ident ty varInfo funInfo)
{-
    | RedeclaredFunction (Ident ty varInfo funInfo)
    | RedeclaredParameter (Ident ty varInfo funInfo)
    | TypeMismatch Loc (Type ty varInfo funInfo) (Type ty varInfo funInfo)
    | FunctionNameMismatch Loc Id
    | NonOmogeneusArray Loc
    | NonConstantExpr Loc
    | NonIntegerExpr Loc
    | NonIntegerExprInArrayAccess Loc
    | NotDeclared (Ident ty varInfo funInfo)
    | DereferenceExcess (Ident ty varInfo funInfo)
    | PointerArithmetic Loc
    | NonNumericValue Loc
    | ArithmeticError (Type ty varInfo funInfo) (Type ty varInfo funInfo) Loc String 
    | DifferentLenght Loc
    | EmptyList Loc
    | NegativeSize Loc
    | NegativeSizeIndex Loc
    | DivideByZero Loc
    | NonBasicType Loc (Type ty varInfo funInfo)
--  | RelError Loc (Type ty) (Type ty) Rel
    | BoolOpError Loc (Type ty varInfo funInfo) (Type ty varInfo funInfo) String
    | BoolNotError Loc (Type ty varInfo funInfo) String
    | FuncTypesMismatch [Type ty varInfo funInfo] [Type ty varInfo funInfo] (Ident ty varInfo funInfo) Loc
    | AssignConstToVar Loc
    | ChangingConstVar (Ident ty varInfo funInfo)
    | NotInLoop Loc String
    | NonBooleanExpr Loc (Type ty varInfo funInfo)
    | NonConstPass Loc Int (Ident ty varInfo funInfo)
    | NonCompExpr (Type ty varInfo funInfo) (Type ty varInfo funInfo) Loc
    | NonRefLeft Loc Int (Ident ty varInfo funInfo)
    | NonEqRefTypes Loc Int (Ident ty varInfo funInfo) (Type ty varInfo funInfo) (Type ty varInfo funInfo)
    | PassingRefOfConst Loc
    | CaseExprNotEq Loc (Type ty varInfo funInfo)
    | ErrorDeclaration (Ident ty varInfo funInfo) String
    | FunctionWithoutDimension (Ident ty varInfo funInfo)
    | NotAnArrayForEach Loc
    | NotCompileTime Loc
    | IncompatibleExprs Loc (Type ty varInfo funInfo) (Type ty varInfo funInfo)
    | FunctionAsIdentifier Loc
    | ReturnInForLoop Loc String
    | CustomError String
    | WarEmptyCase Loc
    | UnsusedValue Loc
    | UnitializedVariable (Ident ty varInfo funInfo)
    | DeathCode Loc
    | NoMain Loc
    | InfiniteLoop Loc
-}

errorType :: Error ty varInfo funInfo -> String
errorType x =
    case x of
        -------------------- [Declaration Error] --------------------
        RedeclaredVar (Ident name loc _ _ _) -> 
            errorMsg "The identifier '" ++ name ++ "' has already been used for a variable." ++ " [Declaration Error]."

        RedeclaredConst (Ident name loc _ _ _) -> 
            errorMsg "The identifier '" ++ name ++ "' has already been used for a constant." ++ " [Declaration Error]."
        
        NotConstantValue idName ->
            errorMsg "The value '" ++ idName ++ "' must be a constant." ++ " [Declaration Error]."

        RedeclaredProcFun (Ident name loc _ _ _) -> 
            errorMsg "The identifier '" ++ name ++ "' has already been used for a procedure/function." ++ " [Declaration Error]."

        FunNotDefined (Ident name loc _ _ _) -> 
            errorMsg "The function '" ++ name ++ "' does not exists." ++ " [Declaration Error]."
        
        NotAFunction (Ident name loc _ _ _) -> 
            errorMsg "'" ++ name ++ "' is a variable, not a function." ++ " [Declaration Error]."
        
        FunArgsNumMismatch nArgs nFormParams -> 
            errorMsg "Mismatch in arguments number: expected "++ show nFormParams ++ " parameters, got " ++ show nArgs  ++ "." ++ " [Declaration Error]."
        
        UnknownIdentifier (Ident name loc _ _ _) -> 
            errorMsg "Unknown identifier '" ++ name ++ "'." ++ " [Declaration Error]."

        AssignmentToConstant ->
            errorMsg "Cannot assign a value to a constant." ++ " [Declaration Error]"

        -------------------- [Type Error] --------------------
        IncompatibleArrPtrType -> 
            errorMsg "Array element type must match pointer type."

        IncompatiblePtrArrType -> 
            errorMsg "Pointer type must match array element type."
        
        IncompatibleTypes op (BasicType t1) (BasicType t2) ->
            errorMsg "Cannot perform " ++ show op ++ " operation: type \"" ++ show t1 ++ "\" is not compatible with type \"" ++ show t2 ++ "\"." ++ " [Type Error]"

        ComparisonTypeMismatch op (BasicType t1) (BasicType t2) ->
            errorMsg "Type mismatch in comparison operation " ++ show op ++ ". [Type Error]"

        NotBooleanOperation op (BasicType t1) (BasicType t2) ->
            errorMsg "Cannot perform " ++ show op ++ " operation between boolean types. [Type Error]"
        
        NotIntegerType op (BasicType t) ->
            errorMsg "Integer type expected in " ++ show op ++ " operation. Got " ++ show t ++ " type instead." ++ " [Type Error]"

        NotBooleanType op (BasicType t) ->
            errorMsg "Boolean type expected in " ++ show op ++ " operation. Got " ++ show t ++ " type instead." ++ " [Type Error]"
        
        NotNumericType op (BasicType t) ->
            errorMsg "Numeric type expected in " ++ show op ++ "." ++ " [Type Error]"
        
        IncompatibleAssignment targetType sourceType ->
            errorMsg "Cannot assign \"" ++ showTy sourceType ++ "\" type to \"" ++ showTy targetType ++ "\"." ++ " [Type Error]"
        
        FunArgTypeMismatch (BasicType argType) (BasicType paramType) ->
            errorMsg "Argument type mismatch in function call: expected \"" ++ show paramType ++ "\" type, got \"" ++ show argType ++ "\" instead." ++ " [Type Error]"
        
        NotBooleanCondition conditionType ->
            errorMsg "The condition must be of type \"Boolean\", got \"" ++ showMType conditionType ++ "\" instead." ++ " [Type Error]"
        
        -------------------- [Array Error] --------------------
        NotAnArray ->
            errorMsg "Cannot index a non-array type. " ++ " [Array Error]"
        
        OutOfBound outOfBoundIndices loc -> case length outOfBoundIndices of
            1 -> errorMsg "Index " ++ show (head outOfBoundIndices) ++ " out of bound." ++ " [Array Error]"
            _ -> errorMsg "Indices " ++ show outOfBoundIndices ++ " out of bound." ++ " [Array Error]"
        
        NegativeArrIndex ->
            errorMsg "Array index cannot be negative." ++ " [Array Error]"

        -------------------- [Pointer Error] --------------------
        NotLExpr op@(UnaryRef loc) ->
            errorMsg "Cannot use reference operator " ++ show op ++ " on a non-L-expression (e.g., a literal or an r-value)." ++ " [Pointer Error]"

        NotAPointer ->
            errorMsg "Cannot dereference a non-pointer type. " ++ " [Pointer Error]"

{-
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

        NotDeclared (Ident ((loc),name)) ->
            errorMsg "Use of undeclared identifier '"
            ++ name ++ "'." ++ (lineAndColumn l c) ++  " [DeclError]."
            
        DereferenceExcess (Ident ((loc),_)) -> 
            errorMsg "Dereferencing requires pointer operand."  
            ++ (lineAndColumn l c) ++  " [TypeError]."
        
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
            
        NonBasicType (loc) typ-> 
            errorMsg "Expected a basic type but found '" ++ showType typ ++ "'. \n"++tabSpace ++ "E.g., 'int', 'real', 'string', 'char' and 'bool'."
            ++ (lineAndColumn l c) ++  " [DeclError]."
        
        ArithmeticError ty1 ty2 (loc) op 
            | op == "^" || op == "%" -> errorMsg "Undefined operator '"++ op ++" : "++ showType ty1 ++" x "++ showType ty2 ++" -> Int'.\n"++tabSpace ++ "Maybe you want to use '"++op++": int x int -> int'" ++ (lineAndColumn l c) ++  " [TypeError]."
            | op == "/" -> errorMsg "Undefined operator '"++ op ++" : "++ showType ty1 ++" x "++ showType ty2 ++" -> τ'.\n"++tabSpace ++ "Maybe you want to use '"++op++": τ_1 x τ_2 -> real'.\n" ++tabSpace ++"Where τ_1, τ_2 ∈ {'int','real'}.\n"++tabSpace++"More details can be found in the documentation." ++ (lineAndColumn l c) ++  " [TypeError]."
            | otherwise -> errorMsg "Undefined operator '"++ op ++" : "++ showType ty1 ++" x "++ showType ty2 ++" -> τ'.\n"++tabSpace ++ "Maybe you want to use '"++op++": τ_1 x τ_2 -> τ3'.\n" ++tabSpace ++"Where τ_1, τ_2 ∈ {'int','real'}  and τ_3 is the max between τ_1 and τ_2.\n"++tabSpace++"More details can be found in the documentation." ++ (lineAndColumn l c) ++  " [TypeError]."
        
        RelError (loc) ty1 ty2 rel -> 
            errorMsg "Undefined operator '"++ showRel rel ++" : "++ showType ty1 ++" x "++ showType ty2 ++" -> Bool'."++{-\n++tabSpace ++ "Expected 2 numerical values, e.g., 'int' or 'real'."++ -}(lineAndColumn l c) ++  " [TypeError]."
            
        BoolOpError (loc) ty1 ty2 op -> errorMsg "Undefined operator '"++ op ++" : "++ showType ty1 ++" x "++ showType ty2 ++" -> Bool'.\n"++tabSpace ++ "Expected 2 boolean values."++ (lineAndColumn l c) ++  " [TypeError]."
        
        BoolNotError (loc) ty1 op -> errorMsg "Undefined operator '"++ op ++" : "++ showType ty1 ++" -> Bool'.\n"++tabSpace ++ "Expected a boolean value."++ (lineAndColumn l c) ++  " [TypeError]."
                    
        FuncTypesMismatch exprs params (Ident ((loc),name)) (ld, cd)->
            errorMsg "Mismatch in argument types."++ (lineAndColumn l c) ++  " [TypeError].\n" ++ tabSpace ++ "In function call: '" ++ (showFunction ( name ++ "(" ) exprs)++ "'.\n"++tabSpace ++
                "Function definition found at <l:" ++ (show ld) ++ ",c:" ++ (show cd) ++ "> with signature: '" ++ ( showFunction (name++"(") params) ++
                "'."
        AssignConstToVar loc ->
            errorMsg "You cannot assign the address of a constant variable to a variable." ++ " [TypeError]."

        PassingRefOfConst loc ->
            errorMsg "Passing the address of a const qualified variable." ++ " [TypeError]."
        
        ChangingConstVar (Ident ((loc),name)) ->
            errorMsg "Cannot change the value of the const variable '"++ name ++ "'."
            ++ (lineAndColumn l c) ++  " [TypeError]."
            
        NotInLoop loc stmt ->
            errorMsg  "'" ++ stmt ++ "' statement not in a loop statement." ++ " [ContextError]."
        
        NonBooleanExpr (loc)  ty-> 
            errorMsg "Expected a boolean expression but found '" ++ showType ty  ++ "'."
            ++ (lineAndColumn l c) ++  " [TypeError]."
        
        NonConstPass (loc) parNum (Ident (_,name)) -> errorMsg "Cannot pass a const variable if the formal parameter is  not 'const' qualified.\n"++ tabSpace ++ "The "++(show parNum)  ++ "° parameter of the function '"++name++ "' is not const qualified." ++ (lineAndColumn l c) ++  " [TypeError]."
        
        NonRefLeft (loc) parNum (Ident (_,name)) -> errorMsg "The "++(show parNum)  ++ "° parameter of the function '"++name++ "' requires a Left Expression." ++ (lineAndColumn l c) ++  " [TypeError]."
        
        NonEqRefTypes (loc) parNum (Ident (_,name)) typReq typFound -> errorMsg "The "++(show parNum)  ++ "° parameter of the function '"++name++ "' requires exactly the type '" ++ showType typReq++ "' but found '" ++ showType typFound ++"'." ++ (lineAndColumn l c) ++  " [TypeError]."

        NonCompExpr baseType exprType (loc) -> errorMsg "Non compatible types in 'case' statement. Expected '" ++ showType baseType  ++ "' but found '" ++ showType exprType ++ "'." ++ (lineAndColumn l c) ++  " [TypeError]."
        
        CaseExprNotEq (loc) ty->errorMsg "Illegal expression of type '" ++ showType ty ++ "' inside the statement 'case'.\n"++
            tabSpace ++ "The operator '= :"++showType ty++ " x "++ showType ty ++ " -> bool' is not defined." 
            ++ (lineAndColumn l c) ++  " [TypeError]."

        ErrorDeclaration (Ident ((loc),_)) kind ->
            errorMsg kind++" declaration must be also definition." ++ (lineAndColumn l c) ++  " [DeclError]."

        FunctionWithoutDimension (Ident ((loc),name)) ->
            errorMsg "The array dimension for the parameter '"++ name ++ "' must be specified." ++ (lineAndColumn l c) ++  " [TypeError]."

        NotAnArrayForEach loc ->
            errorMsg "The 'foreach' statement requires an array type." ++ " [TypeError]."

        NotIntegerType (loc) ty ->
            errorMsg "Expected an integer value but found '" ++ showType ty ++"'." ++ (lineAndColumn l c) ++  " [TypeError]."

        NotCompileTime loc ->
            errorMsg "The expression must be evaluable at compile time. " ++ " [TypeError]."

        IncompatibleExprs (loc) ty1 ty2 ->
            errorMsg "The two types '"++ showType ty1 ++ "' and '"++ showType ty2 ++ "''  in '("++ showType ty1 ++ ") if (·) else ("++  showType ty2 ++")' must coincide."++ (lineAndColumn l c) ++  " [TypeError]."

        FunctionAsIdentifier loc ->
            errorMsg "Cannot use a function as a variable." ++ " [TypeError]."

        ReturnInForLoop loc stmt ->
            errorMsg "Cannot use the statement '" ++ stmt ++ "' inside a deterministic 'for' loop. " ++ " [ControlFlowAnalysis]."

        CustomError msg -> errorMsg msg
-}
-------------------------------------------------
------------------ WARNINGS ---------------------
-------------------------------------------------

        WarVarNameUsedForFunc (Ident name _ _ _ _) ->
            warningMsg "The identifier '" ++ name ++ "' has already been used for a function." ++ " [DeclError]."

{-
        WarEmptyCase loc ->
            warningMsg "Empty 'case' statement." ++ " [ControlFlowAnalysis]."

        UnsusedValue loc ->
            warningMsg "Expression result unused." ++ " [ControlFlowAnalysis]."  

        UnitializedVariable (Ident ((loc),name)) ->
            warningMsg "The return variable '"++ name ++"' may be never initialized."++ (lineAndColumn l c) ++  " [ControlFlowAnalysis]." 

        DeathCode loc ->
            warningMsg "Dead code after 'return' statement." ++ " [ControlFlowAnalysis]."
        
        NoMain loc ->
            warningMsg "Declaration of 'main' not found."

        InfiniteLoop loc ->
            warningMsg "Infinite loop." ++ " [ControlFlowAnalysis]."
-}

showTy :: SimpleType -> String
showTy ty = case ty of
    Array _ arrTy       -> "Array of " ++ showTy arrTy
    Pointer ptrTy       -> "Pointer to " ++ showTy ptrTy
    BasicType Integer   -> "Integer"
    BasicType Real      -> "Real"
    BasicType Boolean   -> "Boolean"
    BasicType Character -> "Character"
    BasicType String    -> "String"
    BasicType Void      -> "Void"