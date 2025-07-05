module TACMonad where

import qualified Data.Set as Set
import qualified Data.Map as Map
import AbsLinguaggio
import TACabssynt
import Control.Monad.State
import Data.Functor.Identity (Identity)



type TACGen a = StateT TACState Identity a

data TACState = TACState {
    tempCounter :: Int,
    labelCounter :: Int,
    breakLabels :: [TACLabel],
    continueLabels :: [TACLabel],
    stringCounter :: Int,                   -- counter for string addresses
    stringTable :: Map.Map String String,    -- table for constant strings
    outOfBound :: Bool,
    functionLabels :: Map.Map String TACLabel -- Map between proc/func identifiers and their labels
}

initialTACState :: TACState
initialTACState = TACState 0 0 [] [] 0 Map.empty False Map.empty

newtemp :: Maybe TACType -> TACGen Address
newtemp tacTy = do
    state <- get
    let newCounter = tempCounter state + 1
    put (state { tempCounter = newCounter })
    return $ Temporary (newCounter - 1) tacTy

newlabel :: TACGen TACLabel
newlabel = do
    state <- get
    let newCounter = labelCounter state + 1
    put (state { labelCounter = newCounter })
    return (newCounter - 1)

-- Generate new unic address for a string
newStringAddress :: String -> TACGen String
newStringAddress str = do
    state <- get
    let cs = stringCounter state
        strAddr = "str" ++ show cs
        newTable = Map.insert str strAddr (stringTable state)
    put state { stringCounter = cs + 1, stringTable = newTable }
    return strAddr

pushBreakLabel :: TACLabel -> TACGen ()
pushBreakLabel label = do
    state <- get
    put (state { breakLabels = label : breakLabels state })

popBreakLabel :: TACGen TACLabel
popBreakLabel = do
    state <- get
    case breakLabels state of
        (label:rest) -> do
            put (state { breakLabels = rest })
            return label
        [] -> error "No break label available"

pushContinueLabel :: TACLabel -> TACGen ()
pushContinueLabel label = do
    state <- get
    put (state { continueLabels = label : continueLabels state })

popContinueLabel :: TACGen TACLabel
popContinueLabel = do
    state <- get
    case continueLabels state of
        (label:rest) -> do
            put (state { continueLabels = rest })
            return label
        [] -> error "No continue label available"

tryPopBreakLabel :: TACGen (Maybe TACLabel)
tryPopBreakLabel = do
    state <- get
    case breakLabels state of
        (label:rest) -> do
            put (state { breakLabels = rest })
            return (Just label)
        [] -> return Nothing

tryPopContinueLabel :: TACGen (Maybe TACLabel)
tryPopContinueLabel = do
    state <- get
    case continueLabels state of
        (label:rest) -> do
            put (state { continueLabels = rest })
            return (Just label)
        [] -> return Nothing

-- error handling
array_index_out_of_bounds :: TACLabel
array_index_out_of_bounds = 999

outOfBoundHandler :: [TAC TACLabel]
outOfBoundHandler  = [
  TACAddLabel array_index_out_of_bounds,
  TACComment "Runtime Error: Array index out of bounds",
  TACGoto array_index_out_of_bounds  -- infinite loop to simulate halt
  ]

genOutOfBound :: TACGen [TAC TACLabel] 
genOutOfBound = do 
  happened <- gets outOfBound 
  if happened 
    then return outOfBoundHandler
    else return []

-----------------------------------------------------------
-------------------- SUPPORT FUNCTIONS --------------------
-----------------------------------------------------------

-- Convert Maybe SimpleType in TACType
getTACType :: EnrichedType -> Maybe TACType
getTACType (BasicType t) = case t of
  Integer   -> Just TACInteger
  Real      -> Just TACReal
  Character -> Just TACCharacter
  Boolean   -> Just TACBoolean
  String    -> Just TACAddr
  Void      -> Nothing
getTACType (Pointer _) = Just TACAddr
getTACType (Array {}) = Just TACAddr
getTACType Error = Nothing  -- default fallback

-- Extract BasicType from a (Maybe SimpleType)
getBasicType :: EnrichedType -> BasicType
getBasicType (BasicType t) = t
getBasicType _ = Void  -- default fallback (error)