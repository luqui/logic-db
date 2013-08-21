module Main where

import LogicDB.Parser
import System.Environment

main = do
    [file] <- getArgs
    print . parseProgram . lineTokenize =<< readFile file
