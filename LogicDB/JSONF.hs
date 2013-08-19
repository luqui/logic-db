{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, PackageImports #-}

module LogicDB.JSONF 
    ( ValueF(..)
    , fromAeson, toAeson
    )
where

import qualified Data.HashMap.Strict as HashMap
import Data.Hashable
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import Data.Attoparsec.Number (Number)
import qualified Data.Aeson as Aeson
import Control.Applicative
import qualified LogicDB.UnificationSolver as Solver
import LogicDB.FZip
import Data.Foldable
import Data.Traversable
import Control.Monad.Free (Free(..))
import Data.Void (Void, absurd)

data ValueF a
    = Object (HashMap.HashMap Text.Text a)
    | Array  (Vector.Vector a)
    | String Text.Text
    | Number Number
    | Bool   Bool
    | Null
    deriving (Functor, Foldable, Traversable)

fromAeson :: Aeson.Value -> Free ValueF a
fromAeson (Aeson.Object obj)   = Free . Object $ fromAeson <$> obj
fromAeson (Aeson.Array array)  = Free . Array  $ fromAeson <$> array
fromAeson (Aeson.String t)     = Free . String $ t
fromAeson (Aeson.Number n)     = Free . Number $ n
fromAeson (Aeson.Bool b)       = Free . Bool $ b
fromAeson Aeson.Null           = Free Null

toAeson :: Free ValueF Void -> Aeson.Value
toAeson (Free (Object obj))  = Aeson.Object (toAeson <$> obj)
toAeson (Free (Array array)) = Aeson.Array (toAeson <$> array)
toAeson (Free (String t))    = Aeson.String t
toAeson (Free (Number n))    = Aeson.Number n
toAeson (Free (Bool b))      = Aeson.Bool b
toAeson (Free Null)          = Aeson.Null
-- toAeson (Pure ())         -- impossible


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
