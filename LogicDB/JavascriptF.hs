{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module LogicDB.JavascriptF 
    ( JavascriptF
    , fromJS
    , toJS
    , literal
    )
where

import Prelude ()
import PreludePlus
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.Free (Free(..))
import Data.List (intercalate)
import Data.Void (Void, absurd)

import qualified LogicDB.JavascriptFreeVars as JSFV
import LogicDB.FZip

data JavascriptF a
    = JavascriptF String (Map.Map String a)
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

fromJS :: String -> Either String (JavascriptF String)
fromJS jss = JavascriptF jss . diag . JSFV.freeVars <$> JSFV.vars jss
    where
    diag s = Map.fromList [ (x,x) | x <- Set.toList s ]

toJS :: Free JavascriptF String -> String
toJS (Pure x) = x
toJS (Free (JavascriptF code vars)) = concat 
    [ "(function(__params) {"
    , concat [ "var " ++ n ++ " = __params." ++ n ++ "; " | n <- Map.keys vars ]
    , "return (" ++ code ++ ");})({ "
    , intercalate "," [ n ++ ": " ++ toJS v | (n,v) <- Map.assocs vars ]
    , "})"
    ]

literal :: String -> JavascriptF a
literal js = JavascriptF js Map.empty

instance FZip JavascriptF where
    fzip (JavascriptF code vars) (JavascriptF code' vars')
        | code == code' = JavascriptF code <$> fzip vars vars'
        | otherwise     = empty
