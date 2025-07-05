module ErrorStaticAnalysis where

import AbsLinguaggio
import PrettyPrinter
import Data.Data (toConstr, showConstr)



data Error ty varInfo funInfo
    = RedeclaredConstVar SimpleIdent Bool
    | NotConstantValue String
    | RedeclaredProcFun SimpleIdent Loc
    | FunNotDefined SimpleIdent
    | NotAFunction SimpleIdent
    | FunArgsNumMismatch Int Int 
    | UnknownIdentifier SimpleIdent
    | IncompatibleArrPtrType
    | IncompatiblePtrArrType
    | IncompatibleTypes String SimpleType SimpleType
    | ComparisonTypeMismatch String SimpleType SimpleType
    | NotBooleanType String SimpleType
    | NotArithmeticType String SimpleType
    | IncompatibleAssignment SimpleType SimpleType
    | NotBooleanOperation String SimpleType SimpleType
    | FunArgTypeMismatch SimpleType SimpleType
    | IncompatibleRefModParamType SimpleType SimpleType
    | NotLExprInRefMod
    | ForBoundsTypeMismatch
    | ForControlVarTypeNotAllowed SimpleType
    | NotLExpr Unary
    | NotAPointer
    | NotAnArray
    | AssignmentToConstant
    | NotBooleanCondition String SimpleType
    | NotInLoop String
    | NotInFunction
    | MissingReturn
    | ReturnTypeMismatch SimpleType SimpleType
    | NotIntegerArrIndex SimpleType
    | ForControlVarUsedInBody SimpleIdent Loc
    | ForControlVarNotLId

errorType :: Error ty varInfo funInfo -> String
errorType x =
    case x of
        -------------------- [Declaration Error] --------------------
        RedeclaredConstVar (Ident name loc _ _ _) isConst -> if isConst
            then errorMsg "The identifier \"" ++ name ++ "\" has already been used for a constant. [Declaration Error]"
            else errorMsg "The identifier \"" ++ name ++ "\" has already been used for a variable or function parameter. [Declaration Error]"
        
        NotConstantValue idName ->
            errorMsg "The value \"" ++ idName ++ "\" must be a constant. [Declaration Error]"

        RedeclaredProcFun (Ident name loc _ _ _) existingFunLoc ->
            if existingFunLoc == Loc 0 0 -- This means that the function is one of the default
            then errorMsg "The identifier \"" ++ name ++ "\" is used for a default procedure/function. [Declaration Error]"
            else errorMsg "The identifier \"" ++ name ++ "\" has already been used for a procedure/function. [Declaration Error]"

        FunNotDefined (Ident name loc _ _ _) -> 
            errorMsg "The function \"" ++ name ++ "\" doesn't exist. [Declaration Error]"
        
        NotAFunction (Ident name loc _ _ _) -> 
            errorMsg "\"" ++ name ++ "\" is a variable, not a function. [Declaration Error]"
        
        FunArgsNumMismatch nArgs nFormParams -> 
            errorMsg "Mismatch in arguments number: expected " ++ show nFormParams ++ " parameters, got " ++ show nArgs ++ ". [Declaration Error]"
        
        UnknownIdentifier (Ident name loc _ _ _) -> 
            errorMsg "Unknown identifier \"" ++ name ++ "\". [Declaration Error]"

        AssignmentToConstant ->
            errorMsg "Cannot assign a value to a constant. [Declaration Error]"

        ForControlVarUsedInBody (Ident name loc _ _ _) _ ->
            errorMsg "Cannot use the control variable \"" ++ name ++ "\" of the for loop inside its body. [Declaration Error]"

        -------------------- [Type Error] --------------------
        IncompatibleArrPtrType -> 
            errorMsg "Array element type must match pointer type. [Type Error]"

        IncompatiblePtrArrType -> 
            errorMsg "Pointer type must match array element type. [Type Error]"
        
        IncompatibleTypes op (BasicType t1) (BasicType t2) ->
            errorMsg "Cannot perform " ++ show op ++ " operation: type \"" ++ show t1 ++ "\" is not compatible with type \"" ++ show t2 ++ "\". [Type Error]"

        ComparisonTypeMismatch op (BasicType t1) (BasicType t2) ->
            errorMsg "Type mismatch in comparison operation " ++ show op ++ ". [Type Error]"

        NotBooleanOperation op (BasicType t1) (BasicType t2) ->
            errorMsg "Cannot perform " ++ show op ++ " operation between boolean types. [Type Error]"

        NotBooleanType op (BasicType t) ->
            errorMsg "Boolean type expected in " ++ show op ++ " operation. Got " ++ show t ++ " type instead. [Type Error]"
        
        NotArithmeticType op (BasicType t) ->
            errorMsg "Numeric type expected in " ++ show op ++ ". [Type Error]"
        
        IncompatibleAssignment targetType sourceType ->
            errorMsg "Cannot assign \"" ++ showTy sourceType ++ "\" type to \"" ++ showTy targetType ++ "\". [Type Error]"
        
        FunArgTypeMismatch argType paramType ->
            errorMsg "Argument type mismatch in function call: expected \"" ++ showTy paramType ++ "\" type, got \"" ++ showTy argType ++ "\" instead. [Type Error]"
        
        IncompatibleRefModParamType argType paramType ->
            errorMsg "The argument type for the \"By reference\" modality must exactly match the type of the formal parameter: expected \"" ++ showTy paramType ++ "\", got \"" ++ showTy argType ++ "\" instead. [Type Error]"

        NotBooleanCondition stmt conditionType ->
            errorMsg "The condition of the " ++ show stmt ++ " statement must be of type \"Boolean\", got \"" ++ showTy conditionType ++ "\" instead. [Type Error]"

        ReturnTypeMismatch returnType functionType ->
            errorMsg "The function must return a value of type \"" ++ showTy functionType ++ "\", got \"" ++ showTy returnType ++ "\" instead. [Type Error]"

        NotIntegerArrIndex ty ->
            errorMsg "Array index must be of type \"Integer\", got \"" ++ showTy ty ++ "\" instead. [Type Error]"

        NotLExprInRefMod ->
            errorMsg "The argument for the \"By reference\" modality must be an L-expression. [Type Error]"

        ForBoundsTypeMismatch ->
            errorMsg "The bounds of the \"for\" loop must be of the same type. [Type Error]"
        
        ForControlVarTypeNotAllowed ty ->
            errorMsg "The control variable of the \"for\" loop can only be of type Integer, Real or Boolean, got \"" ++ showTy ty ++ "\" instead. [Type Error]"

        ForControlVarNotLId ->
            errorMsg "The control variable of the \"for\" loop can't be an array element nor a dereferenced pointer. [Type Error]"
            
        -------------------- [Array Error] --------------------
        NotAnArray ->
            errorMsg "Cannot index a non-array type. [Array Error]"

        -------------------- [Pointer Error] --------------------
        NotLExpr op@(UnaryRef loc) ->
            errorMsg "Cannot use reference operator \"^\" on a non-L-expression (e.g. a literal or an r-value). [Pointer Error]"

        NotAPointer ->
            errorMsg "Cannot dereference a non-pointer type. [Pointer Error]"

        -------------------- [Control Flow Error] --------------------
        NotInLoop stmt ->
            errorMsg "Statement " ++ show stmt ++ " used outside a loop. [Control Flow Error]"

        NotInFunction ->
            errorMsg "Cannot use \"return\" statement outside a function. [Control Flow Error]"
        
        MissingReturn ->
            errorMsg "A \"return\" statement must be present in each possible execution flow of the function. [Control Flow Error]"

        -- DEBUG
        _ -> "[UNHANDLED ERROR]"



showTy :: SimpleType -> String
showTy ty = case ty of
    Array _ arrTy varInfo   -> "Array of " ++ showTy arrTy
    Pointer ptrTy           -> "Pointer to " ++ showTy ptrTy
    BasicType Integer       -> "Integer"
    BasicType Real          -> "Real"
    BasicType Boolean       -> "Boolean"
    BasicType Character     -> "Character"
    BasicType String        -> "String"
    BasicType Void          -> "Void"
    Error                   -> "<error>"