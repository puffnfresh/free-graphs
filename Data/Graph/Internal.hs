module Data.Graph.Internal (runGraphState, recursiveGraphState, addNode, formatted) where

import Control.Monad.State
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree
import Data.GraphViz

runGraphState :: (Node -> f -> c -> State Node ([LNode c], [UEdge])) -> f -> c -> Gr c ()
runGraphState g t f = uncurry mkGraph . flip evalState 0 $ g 0 t f

recursiveGraphState :: (c -> Maybe (f c)) -> Node -> (f c -> [c]) -> c -> State Node ([(Node, c)], [(Node, Node, ())])
recursiveGraphState f n t c = do
  (v, e) <- addNode n c
  n' <- get
  rest <- mapM (recursiveGraphState f n' t) . maybe [] t $ f c
  return (v ++ (rest >>= fst), e ++ (rest >>= snd))

addNode :: Node -> a -> State Node ([LNode a], [UEdge])
addNode p a = do
  modify (+1)
  n <- get
  return ([(n, a)], if p == 0 then [] else [(p, n, ())])

formatted :: (f -> Attributes) -> GraphvizParams Node f () () f -> GraphvizParams Node f () () f
formatted f p = p { fmtNode = f . snd }
