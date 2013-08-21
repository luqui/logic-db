{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, PackageImports, PatternGuards #-}

module LogicDB.JSONF 
    ( ValueF(..)
    , fromAeson, toAeson, abstract, inject
    )
where

import Prelude ()
import PreludePlus
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import Data.Attoparsec.Number (Number)
import qualified Data.Aeson as Aeson
import Control.Monad.Free (Free(..))
import Data.Void (Void, absurd)

import LogicDB.FZip

data ValueF a
    = Object (HashMap.HashMap Text.Text a)
    | Array  (Vector.Vector a)
    | String Text.Text
    | Number Number
    | Bool   Bool
    | Null
    deriving (Functor, Foldable, Traversable)

fromAeson :: Aeson.Value -> Free ValueF a
fromAeson = abstract (const Nothing)

toAeson :: Free ValueF Void -> Aeson.Value
toAeson = inject absurd

abstract :: (Aeson.Value -> Maybe a) -> Aeson.Value -> Free ValueF a
abstract f v | Just x <- f v    = Pure x
abstract f (Aeson.Object obj)   = Free . Object $ abstract f <$> obj
abstract f (Aeson.Array array)  = Free . Array  $ abstract f <$> array
abstract _ (Aeson.String t)     = Free . String $ t
abstract _ (Aeson.Number n)     = Free . Number $ n
abstract _ (Aeson.Bool b)       = Free . Bool $ b
abstract _ Aeson.Null           = Free Null

inject :: (a -> Aeson.Value) -> Free ValueF a -> Aeson.Value
inject f (Pure x)             = f x
inject f (Free (Object obj))  = Aeson.Object (inject f <$> obj)
inject f (Free (Array array)) = Aeson.Array (inject f <$> array)
inject f (Free (String t))    = Aeson.String t
inject f (Free (Number n))    = Aeson.Number n
inject f (Free (Bool b))      = Aeson.Bool b
inject f (Free Null)          = Aeson.Null


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
