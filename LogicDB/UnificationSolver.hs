{-# LANGUAGE ExistentialQuantification, GeneralizedNewtypeDeriving, RankNTypes, PatternGuards #-}

module LogicDB.UnificationSolver 
    ( FZip(..)
    , Free(..)
    , Solver
    , alloc
    , normalize
    , unify
    , instantiate
    , runSolver
    )
where

import Prelude hiding (mapM_, elem)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Applicative
import Data.Foldable
import Data.Traversable
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Arrow
import Control.Monad.Free (Free(..))
import Control.Monad (MonadPlus(..))
import LogicDB.FZip

data Supply a = forall s. Supply s (s -> s) (s -> a)

instance Functor Supply where
    fmap f (Supply s t v) = Supply s t (f . v)

supply :: Supply a -> (a, Supply a)
supply (Supply s t v) = s `seq` (v s, Supply (t s) t v)


type Substitution obj v = Map.Map v (Free obj v)

data SolverState obj v = SolverState {
    ssFresh :: Supply v,
    ssSubst :: Substitution obj v
}

type Weight = Integer

newtype Solver obj v m a = Solver { unSolver :: StateT (SolverState obj v) m a }
    deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadTrans)

alloc :: (Monad m) => Solver obj v m v
alloc = Solver $ do
    s <- get
    let sup = ssFresh s
    let (x, sup') = supply sup
    put $ s { ssFresh = sup' }
    return x

subst :: (Ord v, Functor obj) => Substitution obj v -> Free obj v -> Free obj v
subst sub = (doSub =<<)
    where
    doSub x
        | Just y <- Map.lookup x sub = y
        | otherwise = return x
    
normalize :: (Ord v, Functor obj, Monad m) => Free obj v -> Solver obj v m (Free obj v)
normalize obj = Solver $ do
    sub <- gets ssSubst
    return $ subst sub obj

unify :: (FZip obj, Foldable obj, Ord v, Functor m, MonadPlus m) 
      => Free obj v -> Free obj v -> Solver obj v m ()
unify x y = do
    x' <- normalize x
    y' <- normalize y
    case (x', y') of
        (Pure v, Pure v') | v == v' -> return ()
        (Pure v, obj)               -> assign v obj
        (obj, Pure v)               -> assign v obj
        (Free obj, Free obj')       -> mapM_ (uncurry unify) =<< fzip obj obj'

assign :: (Functor obj, Foldable obj, Ord v, Monad m) => v -> Free obj v -> Solver obj v m ()
assign v obj
    | v `elem` obj = fail "Infinite object"
    | otherwise = Solver . modify $ \s -> 
        s { ssSubst = Map.insert v obj (subst (Map.singleton v obj) <$> ssSubst s) }

instantiate :: (Traversable f, Functor m, Monad m) 
            => (a -> Maybe v) -> f a -> Solver obj v m (f v)
instantiate pick = traverse f
    where
    f x | Just v <- pick x = return v
        | otherwise        = alloc

runSolver :: (Monad m) => (forall v. Solver obj v m a) -> m a
runSolver (Solver solver) = flip evalStateT s0 $ solver
    where
    s0 = SolverState { ssFresh = Supply 0 succ id, ssSubst = Map.empty }

