module Data.Graph.Cofree (cofreeGraph, cofreeFoldableGraph, formatted) where

import Control.Comonad.Cofree
import Control.Monad.State
import Data.Foldable
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree
import Data.Graph.Internal

cofreeFoldableGraph :: Foldable f => Cofree f a -> Gr (Cofree f a) ()
cofreeFoldableGraph = cofreeGraph toList

cofreeGraph :: (f (Cofree f a) -> [Cofree f a]) -> Cofree f a -> Gr (Cofree f a) ()
cofreeGraph = runGraphState cofreeGraphState

cofreeGraphState :: Node -> (f (Cofree f a) -> [Cofree f a]) -> Cofree f a -> State Node ([LNode (Cofree f a)], [UEdge])
cofreeGraphState = recursiveGraphState (\(_ :< f) -> Just f)
