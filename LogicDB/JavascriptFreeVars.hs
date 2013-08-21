{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable, PatternGuards #-}

module LogicDB.JavascriptFreeVars (Vars(..), vars) where

import Prelude ()
import PreludePlus
import qualified Data.Set as Set
import Data.Generics
import Data.Typeable
import Language.JavaScript.Parser
import Language.JavaScript.Parser.AST


data Vars = Vars {
    freeVars :: Set.Set String,
    definedVars :: Set.Set String
}
    deriving Show

instance Monoid Vars where
    mempty = Vars Set.empty Set.empty
    mappend (Vars f d) (Vars f' d') = Vars (f `Set.union` f') (d `Set.union` d')

vars :: String -> Either String Vars
vars i = right gvars $ parse i "<input>"

gvars :: (Data a) => a -> Vars
gvars x = case cast x of
    Just n | Just vs <- query n -> vs
    y                           -> fold (gmapQ gvars x)

getIdentifier :: JSNode -> [String]
getIdentifier (NN (JSIdentifier s)) = [s]
getIdentifier (NT (JSIdentifier s) _ _) = [s]
getIdentifier _ = []

query :: Node -> Maybe Vars
query (JSFunction _ nameS _ paramsS _ body) = Just $ Vars {
        freeVars = freeVars vbody 
                `Set.difference` Set.fromList (concatMap getIdentifier paramsS)
                `Set.difference` definedVars vbody,
        definedVars = Set.fromList (getIdentifier nameS)
    }
    where vbody = gvars body
query (JSFunctionExpression _ namesS _ paramsS _ body) = Just $ Vars {
        freeVars = freeVars vbody 
                `Set.difference` Set.fromList (concatMap getIdentifier paramsS)
                `Set.difference` definedVars vbody,
        definedVars = Set.fromList (concatMap getIdentifier namesS)
    }
    where vbody = gvars body
query (JSVarDecl name init) = Just $ Vars {
        freeVars = freeVars (gvars init),
        definedVars = Set.fromList (getIdentifier name)
    }
query (JSWith _ _ e _ block) = Just $ Vars {  -- ignore free variables in body of with
        freeVars = freeVars ve,
        definedVars = definedVars ve `Set.union` definedVars (gvars block)
    }
    where ve = gvars e 
query (JSIdentifier ident) = Just $ Vars {
        freeVars = Set.singleton ident,
        definedVars = Set.empty
    }
query (JSMemberDot pre _ _) = Just $ gvars pre    -- ignore identifiers after a dot  
query (JSCallExpression "." _ _ _) = Just mempty  -- same
query (JSPropertyNameandValue _ _ value) = Just $ gvars value
query x = Nothing
