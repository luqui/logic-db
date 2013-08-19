{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, TupleSections #-}

module LogicDB.Database where

import Prelude hiding (mapM, mapM_)
import qualified Data.Map as Map
import Control.Monad ((<=<), MonadPlus(..))
import Control.Monad.Free
import qualified LogicDB.UnificationSolver as Solver
import Data.Foldable
import Data.Traversable
import LogicDB.FZip
import Control.Applicative

data Prop k obj v = Prop k (Free obj v)
    deriving (Functor, Foldable, Traversable)

data Rule k obj v = Rule [v] (Prop k obj v) [Prop k obj v]

data Database k obj v = Database {
    dbRules :: Map.Map k [Rule k obj v]
    }

addRule :: (Ord k) => Rule k obj v -> Database k obj v -> Database k obj v
addRule rule@(Rule _ (Prop conk _) _) db =
    db { dbRules = Map.insertWith (++) conk [rule] (dbRules db) }

makeMapping :: (Ord k) => [(k,v)] -> k -> v
makeMapping = (Map.!) . Map.fromList

makeAlloc :: (Ord v, Functor m, Monad m) => [v] -> Solver.Solver obj lv m (v -> lv)
makeAlloc = fmap makeMapping . mapM (\var -> (var,) <$> Solver.alloc)

solve1 :: (FZip obj, Traversable obj, Functor m, MonadPlus m, Ord k, Ord v, Ord lv)
       => Database k obj v -> Prop k obj lv -> Solver.Solver obj lv m ()
solve1 db (Prop pred arg) = do
    let rules = maybe [] id $ Map.lookup pred (dbRules db)
    forM_ rules $ \(Rule vars (Prop _ conObj) hyps) -> do
        vmap <- makeAlloc vars
        Solver.unify arg (vmap <$> conObj)
        mapM_ (solve1 db . fmap vmap) hyps

solve :: (MonadPlus m, Functor m, FZip obj, Traversable obj, Ord k, Ord v) 
      => Database k obj v -> [v] -> [Prop k obj v] -> rv -> m (Map.Map v (Free obj rv))
solve db vars props rv = Solver.runSolver $ do
    vmap <- makeAlloc vars
    mapM_ (solve1 db . fmap vmap) props
    Map.fromList <$> mapM (\var -> 
        (var,) . fmap (const rv) <$> Solver.normalize (Pure (vmap var))) vars


