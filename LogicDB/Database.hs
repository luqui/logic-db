{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, TupleSections, FlexibleInstances, StandaloneDeriving, FlexibleContexts #-}

module LogicDB.Database where

import Prelude ()
import PreludePlus
import Control.Monad.Free
import qualified Control.Monad.WeightedSearch as WS
import qualified Data.Map as Map

import qualified LogicDB.UnificationSolver as Solver
import LogicDB.FZip

data Prop k obj v = Prop k (Free obj v)
    deriving (Functor, Foldable, Traversable)

inProp :: (Free obj v -> Free obj' v') -> Prop k obj v -> Prop k obj' v'
inProp f (Prop k m) = Prop k (f m)

deriving instance (Show k, Show (Free obj v)) => Show (Prop k obj v)

data Rule k obj v = Rule [v] (Prop k obj v) [Prop k obj v]

deriving instance (Show k, Show v, Show (Free obj v)) => Show (Rule k obj v)

data Database k obj v = Database {
    dbRules :: Map.Map k [Rule k obj v]
    }

class (MonadPlus m) => MonadDelay m where
    delay :: m a -> m a

instance MonadDelay (WS.T Integer) where
    delay = WS.weight 1

instance (MonadDelay m) => MonadDelay (Solver.Solver obj lv m) where
    delay = Solver.mapSolver delay


addRule :: (Ord k) => Rule k obj v -> Database k obj v -> Database k obj v
addRule rule@(Rule _ (Prop conk _) _) db =
    db { dbRules = Map.insertWith (++) conk [rule] (dbRules db) }

makeMapping :: (Ord k) => [(k,v)] -> k -> v
makeMapping = (Map.!) . Map.fromList

makeAlloc :: (Ord v, Functor m, Monad m) => [v] -> Solver.Solver obj lv m (v -> lv)
makeAlloc = fmap makeMapping . mapM (\var -> (var,) <$> Solver.alloc)

tryEach :: (MonadPlus m) => [a] -> (a -> m b) -> m b
tryEach xs f = msum . map f $ xs

solve1 :: (FZip obj, Traversable obj, Functor m, MonadDelay m, Ord k, Ord v, Ord lv)
       => Database k obj v -> Prop k obj lv -> Solver.Solver obj lv m ()
solve1 db (Prop pred arg) = do
    let rules = maybe [] id $ Map.lookup pred (dbRules db)
    delay . tryEach rules $ \(Rule vars (Prop _ conObj) hyps) -> do
        vmap <- makeAlloc vars
        Solver.unify arg (vmap <$> conObj)
        mapM_ (solve1 db . fmap vmap) hyps

solve :: (MonadDelay m, Functor m, FZip obj, Traversable obj, Ord k, Ord v) 
      => Database k obj v -> [v] -> [Prop k obj v] -> rv -> m (Map.Map v (Free obj rv))
solve db vars props rv = Solver.runSolver $ do
    vmap <- makeAlloc vars
    mapM_ (solve1 db . fmap vmap) props
    Map.fromList <$> mapM (\var -> 
        (var,) . fmap (const rv) <$> Solver.normalize (Pure (vmap var))) vars

