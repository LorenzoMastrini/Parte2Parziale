module Main where

import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )
import Control.Monad (when)
import LexLinguaggio
import ParLinguaggio
import SkelLinguaggio
import AbsLinguaggio
import PrettyPrinter (colorRed, colorGreen, errorMsg, prettyAST)
import PrintLinguaggio
import StaticAnalysis
import TACGen
import PrettyPrinterTAC (dumpTAC) 
import ErrM



type ParseFun a = [Token] -> ErrM.Err a

myLLexer = myLexer

type Verbosity = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s

compile :: Verbosity -> ParseFun SimpleProgram-> FilePath -> IO ()
compile v p f = putStrLn f >> readFile f >>= run v p

run ::  Verbosity -> ParseFun SimpleProgram -> String -> IO ()
run v p s = let ts = myLLexer s in case p ts of
           Bad s    -> do putStrLn (colorRed "[PARSING]: FAIL" )
                          putStrLn $ errorMsg s
                          exitFailure
           Ok tree  -> do putStrV v $ "\n" ++ colorGreen "[PARSING]: PASS\n"
                          putStrLn "------------------------------ SOURCE CODE ------------------------------"
                          putStrLn (prettyPrintSource tree) -- Print source code

                          -- Static semantic analysis
                          putStrLn "------------------------------ STATIC SEMANTIC ANALYSIS ------------------------------"
                          let (env, enrichedAST) = checkProgram tree
                          putStrLn (StaticAnalysis.printEnv env)
                          putStrLn ""
                          -- Print enriched AST
                          putStrLn (prettyAST enrichedAST)
                          
                          putStrLn "------------------------------ TAC GENERATION ------------------------------"
                          if hasErrors env
                            then putStrLn $ colorRed "Cannot generate TAC: the file contains semantic errors."
                            else do
                              -- TAC generation
                              let (tacList, _) = genProgram enrichedAST
                              dumpTAC tacList

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

hasErrors :: Env -> Bool
hasErrors env = not (null (errors env))