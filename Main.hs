import Data.Map qualified as M
import Data.Set qualified as S
import Graph
import Route
import Route (LineStop (..), LineTable (..), Stop (..), readLines, readStops)
import RouteGUI (runGUI)
import System.Environment (getArgs)
import Prelude

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> startGUI
    [stopsFile, linesFile, fromStop, toStop] -> do
      stopsResult <- readStops stopsFile
      linesResult <- readLines linesFile
      case (stopsResult, linesResult) of
        (Right stops, Right lines) -> do
          let graph = buildGraph stops lines
          case shortestPath graph fromStop toStop of
            Nothing -> putStrLn "No path found."
            Just (path, cost) -> do
              print cost
              mapM_ putStrLn path
        (Left err, _) -> putStrLn $ "Error reading stops: " ++ err
        (_, Left err) -> putStrLn $ "Error reading lines: " ++ err
    _ -> putStrLn "Usage: Lab3 stopsFile linesFile fromStop toStop"

startGUI :: IO ()
startGUI = do
  Right stops <- readStops "stops-gbg.txt"
  Right lines <- readLines "lines-gbg.txt"
  let graph = buildGraph stops lines
  runGUI stops lines graph shortestPath

-- | Build the graph from stops and lines.
buildGraph :: [Stop] -> [LineTable] -> Graph String Integer
buildGraph _ lines = foldl addLine empty lines
  where
    addLine g (LineTable _ stopsOnLine) = addEdgesFromLineStops g stopsOnLine
    addEdgesFromLineStops g (from : to : rest) =
      let LineStop nameFrom _ = from
          LineStop nameTo costTo = to
          g' = addEdge nameFrom nameTo costTo g
       in addEdgesFromLineStops g' (to : rest)
    addEdgesFromLineStops g _ = g

-- | Returns the shortest path (list of stops) and total cost from start to goal.
shortestPath :: Graph String Integer -> String -> String -> Maybe ([String], Integer)
shortestPath graph start goal =
  let (dist, prev) = dijkstraWithPrev graph start
      inf = 10 ^ 9 :: Integer
   in case M.lookup goal dist of
        Nothing -> Nothing
        Just d
          | d >= inf -> Nothing
          | otherwise -> Just (reconstructPath prev goal, d)

-- | Dijkstra's algorithm that returns distance and predecessor maps
dijkstraWithPrev ::
  Graph String Integer ->
  String ->
  (M.Map String Integer, M.Map String String)
dijkstraWithPrev graph start = go initialDists S.empty M.empty
  where
    nodes = vertices graph
    inf = 10 ^ 9 :: Integer
    initialDists = M.fromList [(v, if v == start then 0 else inf) | v <- nodes]

    pickMin dists visited =
      let unvisited = filter (`S.notMember` visited) nodes
       in if null unvisited
            then Nothing
            else Just $ minimumBy (\x y -> compare (dists M.! x) (dists M.! y)) unvisited

    go dists visited prevs =
      case pickMin dists visited of
        Nothing -> (dists, prevs)
        Just u ->
          let neighbors = adj u graph
              d_u = dists M.! u
              relax (m, p) (Edge _ v w) =
                let alt = d_u + w
                 in if alt < (M.findWithDefault inf v m)
                      then (M.insert v alt m, M.insert v u p)
                      else (m, p)
              (dists', prevs') = foldl relax (dists, prevs) neighbors
              visited' = S.insert u visited
           in go dists' visited' prevs'

    minimumBy :: (a -> a -> Ordering) -> [a] -> a
    minimumBy _ [] = error "minimumBy: empty list"
    minimumBy cmp (x : xs) = foldl (\acc y -> if cmp y acc == LT then y else acc) x xs

-- | Reconstruct the shortest path from the predecessor map
reconstructPath :: M.Map String String -> String -> [String]
reconstructPath prevs node =
  case M.lookup node prevs of
    Nothing -> [node]
    Just parent -> reconstructPath prevs parent ++ [node]