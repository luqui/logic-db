{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

import Prelude hiding (mapM_)
import LogicDB.Database
import LogicDB.FZip
import Data.Foldable
import Data.Traversable
import Control.Applicative
import Control.Monad.Free
import qualified Data.Map as Map

data TestObj a = TestObj String [a]
    deriving (Show, Functor, Foldable, Traversable)

instance FZip TestObj where
    fzip (TestObj name xs) (TestObj name' xs')
        | name /= name' = empty
        | otherwise = TestObj name <$> fzip xs xs'

tobj :: String -> [Free TestObj a] -> Free TestObj a
tobj name vars = Free (TestObj name vars)

testDB :: Database String TestObj String
testDB = Database {
    dbRules = Map.fromList [
        "parent" --> [
            Rule [] (Prop "parent" (tobj "tuple" [tobj "luke" [], tobj "sue" []])) [],
            Rule [] (Prop "parent" (tobj "tuple" [tobj "luke" [], tobj "rob" []])) []
        ],
        "child" --> [
            Rule ["X","Y"] (Prop "child" (tobj "tuple" [Pure "X", Pure "Y"]))
                [ Prop "parent" (tobj "tuple" [Pure "Y", Pure "X"]) ]
        ]
    ]
}
    where
    (-->) = (,)

asList :: [a] -> [a]
asList = id

main = do
    mapM_ print . asList $ solve testDB ["X","Y"] [Prop "child" (tobj "tuple" [Pure "X", Pure "Y"])] "???"
