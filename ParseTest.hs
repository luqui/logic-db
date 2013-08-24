{-# LANGUAGE LambdaCase #-}

module Main where

import Prelude ()
import PreludePlus
import LogicDB.Parser
import System.Environment
import qualified Data.List.Split as Split
import Data.List (isPrefixOf)
import qualified LogicDB.Database as DB
import qualified LogicDB.JavascriptF as JS
import qualified Control.Monad.WeightedSearch as WS
import qualified Data.Map as Map

runWS :: WS.T Integer a -> [a]
runWS = toList

main = do
    [file, spec] <- getArgs
    rules <- parseProgram <$> readFile file >>= \case
                    Left err -> fail err
                    Right rules -> return rules
    Right rules <- parseProgram <$> readFile file
    Right spec' <- return $ parseSpec spec
    
    putStrLn "Rules:"
    print rules
    putStrLn ""

    putStrLn "Spec:"
    print spec'
    putStrLn ""

    let db = foldr DB.addRule DB.emptyDB rules
    let vars = toList =<< spec'
    let results = DB.solve db vars spec' "null"

    putStrLn "Results:"
    forM_ (runWS results) $ \r -> do
        forM_ (Map.assocs r) $ \(k,v) -> do
            case JS.toJS <$> purifyJS v of
                Just js -> putStrLn $ "var " ++ k ++ " = " ++ js ++ ";"
                Nothing -> return ()
        putStrLn "--"
