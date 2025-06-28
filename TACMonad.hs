module TACMonad where

import qualified Data.Set as Set
import qualified Data.Map as Map
import AbsLinguaggio
import TACabssynt
import Control.Monad.State
import Control.Monad.Writer

data TACState = TACState {
    tempCounter :: Int,
    labelCounter :: Int,
    breakLabels :: [TACLabel],      
    continueLabels :: [TACLabel]    
}

type TACGen a = StateT TACState (Writer [TAC TACLabel]) a

initialTacState :: TACState
initialTacState = TACState 0 0 [] []


newtemp :: TACType -> TACGen Address
newtemp t = do
    state <- get
    let newCounter = tempCounter state + 1
    put (state { tempCounter = newCounter })
    return $ int2AddrTempName (newCounter - 1) t

newlabel :: TACGen TACLabel
newlabel = do
    state <- get
    let newCounter = labelCounter state + 1
    put (state { labelCounter = newCounter })
    return (newCounter - 1)


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

int2Label :: Int -> TACLabel
int2Label = id