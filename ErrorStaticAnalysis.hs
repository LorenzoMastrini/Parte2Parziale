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
    | NotIntegerType String SimpleType
    | NotBooleanType String SimpleType
    | NotNumericType String SimpleType
    | IncompatibleAssignment SimpleType SimpleType
    | NotBooleanOperation String SimpleType SimpleType
    | FunArgTypeMismatch SimpleType SimpleType
    | IncompatibleModParamType OptModality (Maybe SimpleType) SimpleType
    | NotLExprInMod OptModality
    | NotLExpr Unary
    | NotAPointer
    | NotAnArray
    | OutOfBound [Integer] Loc
    | NegativeArrIndex
    | AssignmentToConstant
    | NotBooleanCondition String (Maybe SimpleType)
    | NotInLoop String
    | NotInFunction
    | ReturnInsideProcedure
    | MissingReturn
    | ReturnTypeMismatch (Maybe SimpleType) (Maybe SimpleType)
    | NotIntegerArrIndex (Maybe SimpleType)
{-
    | DifferentLenght Loc
    | DivideByZero Loc
-}

errorType :: Error ty varInfo funInfo -> String
errorType x =
    case x of
        -------------------- [Declaration Error] --------------------
        RedeclaredConstVar (Ident name loc _ _ _) isConst -> if isConst
            then errorMsg "The identifier \"" ++ name ++ "\" has already been used for a constant." ++ " [Declaration Error]."
            else errorMsg "The identifier \"" ++ name ++ "\" has already been used for a variable or function parameter." ++ " [Declaration Error]."
        
        NotConstantValue idName ->
            errorMsg "The value \"" ++ idName ++ "\" must be a constant." ++ " [Declaration Error]."

        RedeclaredProcFun (Ident name loc _ _ _) existingFunLoc ->
            if existingFunLoc == Loc 0 0 -- This means that the function is one of the default
            then errorMsg "The identifier \"" ++ name ++ "\" is used for a default procedure/function." ++ " [Declaration Error]."
            else errorMsg "The identifier \"" ++ name ++ "\" has already been used for a procedure/function." ++ " [Declaration Error]."

        FunNotDefined (Ident name loc _ _ _) -> 
            errorMsg "The function \"" ++ name ++ "\" doesn't exist." ++ " [Declaration Error]."
        
        NotAFunction (Ident name loc _ _ _) -> 
            errorMsg "\"" ++ name ++ "\" is a variable, not a function." ++ " [Declaration Error]."
        
        FunArgsNumMismatch nArgs nFormParams -> 
            errorMsg "Mismatch in arguments number: expected " ++ show nFormParams ++ " parameters, got " ++ show nArgs ++ "." ++ " [Declaration Error]."
        
        UnknownIdentifier (Ident name loc _ _ _) -> 
            errorMsg "Unknown identifier \"" ++ name ++ "\"." ++ " [Declaration Error]."

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
        
        FunArgTypeMismatch argType paramType ->
            errorMsg "Argument type mismatch in function call: expected \"" ++ showTy paramType ++ "\" type, got \"" ++ showTy argType ++ "\" instead." ++ " [Type Error]"
        
        IncompatibleModParamType mod argType paramType -> case mod of
            ByRef       -> errorMsg "The argument type for the \"By reference\" modality must exactly match the type of the formal parameter: expected \"" ++ showTy paramType ++ "\", got \"" ++ showMType argType ++ "\"." ++ " [Type Error]"
            ByValRes    -> errorMsg "The argument type for the \"By value-result\" modality must exactly match the type of the formal parameter: expected \"" ++ showTy paramType ++ "\", got \"" ++ showMType argType ++ "\"." ++ " [Type Error]"

        NotBooleanCondition stmt conditionType ->
            errorMsg "The condition of the " ++ show stmt ++ " statement must be of type \"Boolean\", got \"" ++ showMType conditionType ++ "\" instead." ++ " [Type Error]"

        ReturnTypeMismatch returnType functionType ->
            errorMsg "The return statement returns a value of type \"" ++ showMType returnType ++ "\", but the function is declared to return \"" ++ showMType functionType ++ "\"." ++ " [Type Error]"
        
        ReturnInsideProcedure ->
            errorMsg "The \"return\" statement cannot be used inside a procedure." ++ " [Type Error]"

        NotIntegerArrIndex ty ->
            errorMsg "Array index must be of type \"Integer\", got \"" ++ showMType ty ++ "\" instead." ++ " [Type Error]"
        
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

        -------------------- [Control Flow Error] --------------------
        NotInLoop stmt ->
            errorMsg "Statement " ++ show stmt ++ " used outside a loop." ++ " [Control Flow Error]"

        NotInFunction ->
            errorMsg "Cannot use \"return\" statement outside a function." ++ " [Control Flow Error]"
        
        MissingReturn ->
            errorMsg "A \"return\" statement must be present in each possible execution flow of the function." ++ " [Control Flow Error]"



        NotLExprInMod mod -> case mod of
            ByRef       -> errorMsg "The argument for the \"By reference\" modality must be an L-expression." ++ " [Type Error]"
            ByValRes    -> errorMsg "The argument for the \"By value-result\" modality must be an L-expression." ++ " [Type Error]"

        -- DEBUG
        _ -> "[UNHANDLED ERROR]"



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