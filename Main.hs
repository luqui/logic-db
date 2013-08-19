{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, OverloadedStrings #-}

import Prelude hiding (mapM_, elem)
import qualified LogicDB.Database as DB
import qualified LogicDB.JSONF as JSONF
import Data.Foldable
import Data.Traversable
import Control.Applicative
import Control.Monad.Free
import qualified Data.Map as Map
import Data.Monoid (Monoid(..))
import qualified Data.Text.Lazy as Text
import Data.Text (Text)
import qualified Data.Text.Lazy.Encoding as Text
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromJust)
import Control.Monad.IO.Class

import qualified Web.Scotty as Scotty
import Data.IORef

import qualified Data.Aeson as Aeson
import Data.Aeson ((.:), (.:?), (.!=), parseJSON)

data Prop = Prop 
    { propHead :: Text
    , propValue :: Aeson.Value
    }

instance Aeson.FromJSON Prop where
    parseJSON (Aeson.Object v) = 
        Prop <$> v .: "head"
             <*> v .: "value"

data Rule = Rule
    { ruleVars       :: [Text]
    , ruleConclusion :: Prop
    , ruleHypotheses :: [Prop]
    }

instance Aeson.FromJSON Rule where
    parseJSON (Aeson.Object v) =
        Rule <$> v .: "variables"
             <*> v .: "conclusion"
             <*> v .: "hypotheses"

data Query = Query
    { queryVars        :: [Text]
    , queryConstraints :: [Prop]
    }

instance Aeson.FromJSON Query where
    parseJSON (Aeson.Object v) =
        Query <$> v .: "variables"
              <*> v .: "constraints"

makeProp :: [Text] -> Prop -> DB.Prop Text JSONF.ValueF Text
makeProp vars prop = DB.Prop (propHead prop) (JSONF.abstract p (propValue prop))
    where
    p (Aeson.String x) | x `elem` vars = Just x
                       | otherwise     = Nothing

makeRule :: Rule -> DB.Rule Text JSONF.ValueF Text
makeRule rule = DB.Rule vars (makeProp vars (ruleConclusion rule))
                             (map (makeProp vars) (ruleHypotheses rule))
    where
    vars = ruleVars rule

modifyIORefA :: IORef a -> (a -> a) -> IO ()
modifyIORefA ref f = atomicModifyIORef ref (\x -> (f x, ()))

asList :: [a] -> [a]
asList = id

main = do
    dbRef <- newIORef $ DB.Database { DB.dbRules = Map.empty }
    
    Scotty.scotty 3000 $ do
        Scotty.post "/insert" $ do
            jbody <- Scotty.jsonData
            Aeson.Success q <- return $ Aeson.fromJSON jbody
            liftIO $ modifyIORefA dbRef (DB.addRule (makeRule q))

        Scotty.post "/query" $ do
            jbody <- Scotty.jsonData
            Aeson.Success q <- return $ Aeson.fromJSON jbody
            db <- liftIO $ readIORef dbRef
            let vars = queryVars q
            let results = DB.solve db vars (map (makeProp vars) (queryConstraints q)) Aeson.Null
            Scotty.json . asList $ (fmap.fmap) (JSONF.inject id) results
