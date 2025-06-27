module TACMonad (
    module TACMonad
) where

import qualified Data.Set as Set
import qualified Data.Map as Map
import AbsLinguaggio
import TACabssynt
import Control.Monad.State


data TacState = TacState {
    tempCounter :: Int,
    labelCounter :: Int,
    breakLabels :: [TacLabel],      
    continueLabels :: [TacLabel]    
}

type TACGen a = State TacState a

initialTacState :: TacState
initialTacState = TacState 0 0 [] []


newtemp :: TacType -> TACGen Address
newtemp t = do
    state <- get
    let newCounter = tempCounter state + 1
    put (state { tempCounter = newCounter })
    return $ int2AddrTempName (newCounter - 1) t

newlabel :: TACGen TacLabel
newlabel = do
    state <- get
    let newCounter = labelCounter state + 1
    put (state { labelCounter = newCounter })
    return (newCounter - 1)


pushBreakLabel :: TacLabel -> TACGen ()
pushBreakLabel label = do
    state <- get
    put (state { breakLabels = label : breakLabels state })

popBreakLabel :: TACGen TacLabel
popBreakLabel = do
    state <- get
    case breakLabels state of
        (label:rest) -> do
            put (state { breakLabels = rest })
            return label
        [] -> error "No break label available"

pushContinueLabel :: TacLabel -> TACGen ()
pushContinueLabel label = do
    state <- get
    put (state { continueLabels = label : continueLabels state })

popContinueLabel :: TACGen TacLabel
popContinueLabel = do
    state <- get
    case continueLabels state of
        (label:rest) -> do
            put (state { continueLabels = rest })
            return label
        [] -> error "No continue label available"

int2Label :: Int -> TacLabel
int2Label = id