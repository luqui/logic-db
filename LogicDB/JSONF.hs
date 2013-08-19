{-# LANGUAGE DeriveFunctor, PackageImports #-}

module LogicDB.JSONF where

import qualified Data.HashMap.Strict as HashMap
import Data.Hashable
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import Data.Attoparsec.Number (Number)
import qualified Data.Aeson as Aeson
import Control.Applicative
import qualified LogicDB.UnificationSolver as Solver
import LogicDB.UnificationSolver (FZip(..))
import Data.Traversable
import Control.Monad (join)
import qualified Data.Set as Set
import Control.Monad.Free (Free(..))

data ValueF a
    = Object (HashMap.HashMap Text.Text a)
    | Array  (Vector.Vector a)
    | String Text.Text
    | Number Number
    | Bool   Bool
    | Null
    deriving (Functor)

fromAeson :: Aeson.Value -> Free ValueF a
fromAeson (Aeson.Object obj)   = Free . Object $ fromAeson <$> obj
fromAeson (Aeson.Array array)  = Free . Array  $ fromAeson <$> array
fromAeson (Aeson.String t)     = Free . String $ t
fromAeson (Aeson.Number n)     = Free . Number $ n
fromAeson (Aeson.Bool b)       = Free . Bool $ b
fromAeson Aeson.Null           = Free Null

-- CodeShare Snippet http://www.codeshare.co/442/1/
equalAsSets :: (Ord a) => [a] -> [a] -> Bool
equalAsSets xs ys = Set.fromList xs == Set.fromList ys
-- End CodeShare Snippet

instance (Ord k, Hashable k) => FZip (HashMap.HashMap k) where
    fzip hm1 hm2
        | not (HashMap.keys hm1 `equalAsSets` HashMap.keys hm2) = empty
        | otherwise = pure $ HashMap.intersectionWith (,) hm1 hm2

instance FZip Vector.Vector where
    fzip v1 v2
        | Vector.length v1 /= Vector.length v2 = empty
        | otherwise = pure $ Vector.zip v1 v2

instance FZip ValueF where
    fzip (Object a) (Object b) = Object <$> fzip a b
    fzip (Array a) (Array b)   = Array  <$> fzip a b
    fzip (String a) (String b) = String <$> fzipConst a b
    fzip (Number a) (Number b) = Number <$> fzipConst a b
    fzip (Bool a) (Bool b)     = Bool   <$> fzipConst a b
    fzip Null Null             = pure Null
    fzip _    _                = empty

fzipConst :: (Eq a, Alternative m) => a -> a -> m a
fzipConst a b 
    | a == b = pure a
    | otherwise = empty
