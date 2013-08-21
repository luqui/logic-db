{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, GeneralizedNewtypeDeriving #-}

module LogicDB.Parser where

import Prelude ()
import PreludePlus
import qualified Text.Parsec as P
import qualified Text.Parsec.Token as P
import qualified Data.Map as Map
import qualified Data.DList as DList

import LogicDB.FZip

newtype Struct k a = Struct (Map.Map k a)
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable, FZip)
    

data IndentM = IndentM (DList.DList String) (DList.DList Char)

emptyIndentM :: IndentM
emptyIndentM = IndentM DList.empty DList.empty

appendIndentM :: IndentM -> String -> IndentM
appendIndentM i "" = i
appendIndentM (IndentM lines buf) s@(c:_)
    | c `elem` " \t}" = IndentM lines (buf <> DList.fromList s)
    | otherwise       = IndentM (lines <> DList.singleton (DList.toList buf)) (DList.fromList s)

indentMToTokens :: IndentM -> [String]
indentMToTokens (IndentM lines buf) = DList.toList (lines <> DList.singleton (DList.toList buf))

lineTokenize :: String -> [String]
lineTokenize = indentMToTokens . foldl appendIndentM emptyIndentM . map (++ "\n") . lines




{-
Baz[ x, y, w ]
---------------
Foo[ x, y, z: zk ]
Bar[ y, x ]

w = function() { 
    ...
    ...
}

===============

-}
