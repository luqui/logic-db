{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, OverloadedStrings #-}

import Prelude hiding (mapM_)
import qualified LogicDB.Database as DB
import Data.Foldable
import Data.Traversable
import Control.Applicative
import Control.Monad.Free
import qualified Data.Map as Map
import Data.Monoid (Monoid(..))
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.Encoding as Text
import qualified Data.ByteString as BS

import qualified Web.Scotty as Scotty
import Data.IORef

import qualified Data.Aeson as Aeson

main = do
    dbRef <- newIORef $ DB.Database { DB.dbRules = Map.empty }
    
    Scotty.scotty 3000 $ do
        Scotty.post "/insert" $ do
            jbody <- Scotty.jsonData
            Scotty.json (jbody :: Aeson.Value)
