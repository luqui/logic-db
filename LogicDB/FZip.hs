module LogicDB.FZip 
    ( FZip(..) )
where

import Prelude ()
import PreludePlus
import Data.Hashable
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector as Vector
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Functor.Coproduct as F

class (Functor f) => FZip f where
    fzip :: (Alternative m) => f a -> f b -> m (f (a,b))

instance FZip Maybe where
    fzip Nothing Nothing = pure Nothing
    fzip (Just x) (Just y) = pure (Just (x,y))
    fzip _ _ = empty

instance FZip [] where
    fzip [] [] = pure []
    fzip (x:xs) (y:ys) = ((x,y):) <$> fzip xs ys
    fzip _ _ = empty

instance (Ord k, Hashable k) => FZip (HashMap.HashMap k) where
    fzip hm1 hm2
        | not (HashMap.keys hm1 `equalAsSets` HashMap.keys hm2) = empty
        | otherwise = pure $ HashMap.intersectionWith (,) hm1 hm2

instance FZip Vector.Vector where
    fzip v1 v2
        | Vector.length v1 /= Vector.length v2 = empty
        | otherwise = pure $ Vector.zip v1 v2

instance (Ord k) => FZip (Map.Map k) where
    fzip m1 m2
        | Map.keysSet m1 /= Map.keysSet m2 = empty
        | otherwise = pure $ Map.intersectionWith (,) m1 m2

instance (FZip f, FZip g) => FZip (F.Coproduct f g) where
    fzip (F.Coproduct (Left x)) (F.Coproduct (Left y)) = F.left <$> fzip x y
    fzip (F.Coproduct (Right x)) (F.Coproduct (Right y)) = F.right <$> fzip x y
    fzip _ _ = empty 

-- CodeShare Snippet http://www.codeshare.co/442/1/
equalAsSets :: (Ord a) => [a] -> [a] -> Bool
equalAsSets xs ys = Set.fromList xs == Set.fromList ys
-- End CodeShare Snippet
