{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, GeneralizedNewtypeDeriving, StandaloneDeriving, FlexibleContexts #-}

module LogicDB.Parser where

import Prelude ()
import PreludePlus
import qualified Text.Parsec as P
import qualified Text.Parsec.Token as P
import qualified Data.Map as Map
import qualified Data.DList as DList
import qualified Data.Functor.Coproduct as F
import qualified Data.Char as Char
import Control.Monad.Free (Free(..))
import qualified Data.List.Split as Split
import Data.List (isPrefixOf)

import LogicDB.FZip
import qualified LogicDB.Database as DB
import qualified LogicDB.JavascriptF as JS

type Parser = P.Parsec String ()

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
lineTokenize = filter (not . blank) . indentMToTokens 
               . foldl appendIndentM emptyIndentM . map (++ "\n") . lines
    where
    blank = all Char.isSpace


deriving instance (Show (f a), Show (g a)) => Show (F.Coproduct f g a)

tok :: Parser a -> Parser a
tok p = p <* P.spaces

symbol :: String -> Parser ()
symbol = tok . P.try . fmap (const ()) . P.string

identifier :: Parser String
identifier = tok $ (:) <$> P.letter <*> P.many P.alphaNum

struct :: (String -> a) -> Parser a -> Parser (Struct String a)
struct inj p = Struct . Map.fromList <$> 
        P.between (symbol "[") (symbol "]") (binding `P.sepBy` symbol ",")
    where
    binding = proc <$> identifier <*> (P.optionMaybe (symbol ":" *> p))
    proc i Nothing = (i, inj i)
    proc i (Just a) = (i, a)

type Object = F.Coproduct (Struct String) JS.JavascriptF
type Prop = DB.Prop Pred Object String
type Pred = String

prop :: Parser Prop
prop = DB.Prop <$> identifier <*> (Free . F.left <$> struct Pure structOrVar)


structOrVar :: Parser (Free Object String)
structOrVar = nestedStruct <|> Pure <$> identifier
    where
    nestedStruct = Free . F.left <$> struct Pure structOrVar

object :: Parser (Object String)
object = do
    code <- P.many P.anyChar
    case JS.fromJS code of 
        Left err -> fail err
        Right jsf -> pure . F.right $ jsf

defn :: Parser (String, Object String)
defn = (,) <$> identifier <* symbol "=" <*> object


data Clause
    = Hypothesis Prop
    | Conclusion Prop
    | Definition String (Object String)
    deriving Show

parseClauses :: [String] -> Either String [Clause]
parseClauses = early
    where
    early [] = Right []
    early (l:ls)
        | "--" `isPrefixOf` l = late ls
        | otherwise = case P.parse prop "" l of
                          Left err -> Left (show err)
                          Right p  -> (Hypothesis p :) <$> early ls

    late [] = Right []
    late (l:ls) = case P.parse lateClause "" l of
                      Left err -> Left (show err)
                      Right p  -> (p :) <$> late ls
    
    lateClause = Conclusion <$> P.try prop <|> uncurry Definition <$> defn

parseProgram :: [String] -> Either String [[Clause]]
parseProgram = sequenceA . map parseClauses . Split.splitWhen ("==" `isPrefixOf`)

