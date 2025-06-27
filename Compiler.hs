module Main where

import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )
import Control.Monad (when)
import LexLinguaggio
import ParLinguaggio
import SkelLinguaggio
import AbsLinguaggio
import PrettyPrinter
import PrintLinguaggio
import PrettyPrinterTAC hiding (colorRed, colorGreen, colorYellow) 
import StaticAnalysis
import TACGen
import ErrM

type Pos = (Int, Int)
type ParseFun a = [Token] -> ErrM.Err a

myLLexer = myLexer

type Verbosity = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s

compile :: Verbosity -> ParseFun (Program () () ())-> FilePath -> IO ()
compile v p f = putStrLn f >> readFile f >>= run v p

run ::  Verbosity -> ParseFun (Program () () ())-> String -> IO ()
run v p s = let ts = myLLexer s in case p ts of
           Bad s    -> do putStrLn (colorRed "[PARSING]: FAIL" )
                          putStrLn $ errorMsg s
                          exitFailure
           Ok tree  -> do putStrV v $ "\n" ++ colorGreen "[PARSING]: PASS\n" ++ show tree
                          let enriched = checkProgram tree
                          putStrV v (StaticAnalysis.printEnv (fst enriched)) -- Print static analysis environment
                          putStrLn (prettyTree (snd enriched)) -- Print enriched AST
                          putStrLn (prettyTAC (genProgram (snd enriched))) -- Print TAC

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help                Display this help message."
    , "  (files)               Parse content of files verbosely."
    , "  --s (files)           Silent mode. Parse content of files silently."
    , "  --checkTAC (files)    Check if the generated TAC is equal to the expected TAC."
    ]
  exitFailure

emptyInputError :: IO ()
emptyInputError = do
    putStrLn  (errorMsg "You have to provide an input file.")
    exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"]      -> usage
    []              -> emptyInputError
    "-s":fs         -> mapM_ (compile 4 pProgram) fs
    fs              -> mapM_ (compile 2 pProgram) fs