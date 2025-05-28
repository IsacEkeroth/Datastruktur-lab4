import Data.PSQueue as PQ
import Graph
import Graphics.UI.Threepenny (content)
import Route
import RouteGUI
import Prelude

main :: IO ()
main = do
  --   contents <- getContents

  --     let wordsList = words contents
  --     putStrLn $ show wordsList
  putStrLn "test"

-- | Dijkstra's algorithm.
-- Takes a graph, a function to extract the edge weight (must be Num and Ord), a start node,
-- and returns a map from each reachable node to the minimum distance from the start node,
-- and the map of previous nodes for path reconstruction.
shortestPath :: (Ord a) => Graph a Integer -> a -> a -> Maybe ([a], Integer)
shortestPath graph src dst = dijkstra (PQ.singleton src 0) M.empty M.empty
  where
    dijkstra queue distMap prevMap
      | PQ.null queue = Nothing
      | current == dst = Just (reconstructPath prevMap dst, d)
      | current `M.member` distMap = dijkstra rest distMap prevMap
      | otherwise =
          let edges = adj current graph
              update (q, prevMap) (Edge _ neighbor cost) =
                let newD = d + cost
                    oldPri = PQ.lookup neighbor q
                 in if neighbor `M.member` distMap
                      || maybe False (<= newD) oldPri
                      then (q, prevMap)
                      else (PQ.insert neighbor newD q, M.insert neighbor current prevMap)
              (newQ, newPrev) = foldl update (rest, prevMap) edges
           in dijkstra newQ (M.insert current d distMap) newPrev
      where
        Just (current PQ.:-> d) = PQ.findMin queue
        rest = PQ.deleteMin queue

    reconstructPath prevMap node =
      case M.lookup node prevMap of
        Nothing -> [node]
        Just parent -> reconstructPath prevMap parent ++ [node]
