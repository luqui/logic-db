{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, GeneralizedNewtypeDeriving, StandaloneDeriving, FlexibleContexts, PatternGuards #-}

module LogicDB.Parser where

import Prelude ()
import PreludePlus
import qualified Text.Parsec as P
import qualified Text.Parsec.Token as P
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Set
import qualified Data.DList as DList
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
    blank = (||) <$> all Char.isSpace <*> ("//" `isPrefixOf`)


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


data Object a
    = ObjStruct (Struct String a)
    | ObjJavascript (JS.JavascriptF a)
    | ObjSymbol String
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance FZip Object where
    fzip (ObjStruct s) (ObjStruct s') = ObjStruct <$> fzip s s'
    fzip (ObjJavascript j) (ObjJavascript j') = ObjJavascript <$> fzip j j'
    fzip (ObjSymbol s) (ObjSymbol s') 
        | s == s' = pure $ ObjSymbol s
        | otherwise = empty

type Prop = DB.Prop Pred Object String
type Rule = DB.Rule Pred Object String
type Pred = String
type Error = Either String

prop :: Parser Prop
prop = DB.Prop <$> identifier <*> (Free . ObjStruct <$> struct Pure inlineObject)

purifyJS :: Free Object a -> Maybe (Free JS.JavascriptF a)
purifyJS (Pure x) = Just (Pure x)
purifyJS (Free (ObjJavascript js)) = fmap Free . sequenceA . fmap purifyJS $ js
purifyJS (Free _) = Nothing

closed :: (Traversable f) => Free f a -> Maybe (Free f b)
closed = traverse (const Nothing)

inlineObject :: Parser (Free Object String)
inlineObject = P.choice [ nestedStruct
                        , Pure <$> identifier
                        , Free <$> sym
                        ]
    where
    nestedStruct = Free . ObjStruct <$> struct Pure inlineObject

sym :: Parser (Object a)
sym = ObjSymbol <$> (symbol "'" *> identifier)

object :: Parser (Object String)
object = do
    code <- P.many P.anyChar
    case JS.fromJS code of 
        Left err -> fail err
        Right jsf -> pure . ObjJavascript $ jsf

defn :: Parser (String, Object String)
defn = do
    name <- identifier
    obj <- P.choice [ symbol "=" *> object
                    , ObjJavascript (JS.literal name) <$ symbol "literal"
                    ]
    return (name, obj)


data Clause
    = Hypothesis Prop
    | Conclusion Prop
    | Definition String (Object String)
    deriving Show

parseClauses :: [String] -> Error [Clause]
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

parseRules :: [String] -> Error [[Clause]]
parseRules = sequenceA . map parseClauses . Split.splitWhen ("==" `isPrefixOf`)

clausesToRule :: [Clause] -> Error Rule
clausesToRule clauses = do
    [con] <- return [ p | Conclusion p <- clauses ]
    let hyps = [ p | Hypothesis p <- clauses ]
    let defns = Map.fromList [ (k, substFree defns . Free . fmap Pure $ v) 
                             | Definition k v <- clauses ]
    let con' = DB.inProp (substFree defns) con
    let hyps' = map (DB.inProp (substFree defns)) hyps
    let vars = sortNub $ toList con' ++ (toList =<< hyps') ++ (toList =<< Map.elems defns)
    return $ DB.Rule vars con' hyps'

parseProgram :: String -> Error [Rule]
parseProgram = sequenceA . map clausesToRule <=< parseRules . lineTokenize 
    
    
-- CodeShare Snippet http://www.codeshare.co/450/3/
sortNub :: (Ord a) => [a] -> [a]
sortNub = Data.Set.toList . Data.Set.fromList
-- End CodeShare Snippet


substFree :: (Ord k, Functor f) => Map.Map k (Free f k) -> Free f k -> Free f k
substFree mp (Pure x)
    | Just y <- Map.lookup x mp = y
    | otherwise = Pure x
substFree mp (Free f) = Free (substFree mp <$> f)

complete :: Parser a -> Parser a
complete p = p <* P.eof

parseSpec :: String -> Error [Prop]
parseSpec = left show . P.parse (complete (P.many prop)) ""
