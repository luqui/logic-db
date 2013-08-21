module Main where

import LogicDB.Parser
import System.Environment
import qualified Data.List.Split as Split
import Data.List (isPrefixOf)

main = do
    [file] <- getArgs
    text <- readFile file
    putStrLn "Line tokenize:\n----------"
    print . lineTokenize $ text

    putStrLn "Split:\n----------"
    print . Split.splitWhen ("==" `isPrefixOf`) . lineTokenize $ text

    putStrLn "Program:\n----------"
    print . parseProgram $ text
