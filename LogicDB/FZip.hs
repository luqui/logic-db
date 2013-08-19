module LogicDB.FZip 
    ( FZip(..) )
where

import qualified Data.HashMap.Strict as HashMap
import Data.Hashable
import qualified Data.Vector as Vector
import Control.Applicative
import qualified Data.Set as Set

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


-- CodeShare Snippet http://www.codeshare.co/442/1/
equalAsSets :: (Ord a) => [a] -> [a] -> Bool
equalAsSets xs ys = Set.fromList xs == Set.fromList ys
-- End CodeShare Snippet
