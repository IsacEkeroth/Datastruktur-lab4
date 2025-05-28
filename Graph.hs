module Graph
  ( -- * Edge
    Edge, -- type
    src,
    dst,
    label, -- querying an Edge

    -- * Graph
    Graph, -- type
    empty, -- create an empty map
    addVertex,
    addVertices, -- adding vertices (nodes)
    addEdge,
    addBiEdge, -- adding edges (one way or both ways)
    adj, -- get adjacent nodes for a given node
    vertices,
    edges, -- querying a Graph
  )
where

import Data.Map (Map)
import Data.Map qualified as M

-- An edge with a source and destination node (of type a),
-- together with a label of type b
data Edge a b = Edge
  { -- | Source vertex
    src :: a,
    -- | Destination vertex
    dst :: a,
    -- | The label
    label :: b
  }
  deriving (Show)

-- A graph with nodes of type a and labels of type b.
data Graph a b = Graph (Map a [Edge a b]) deriving (Show) -- TODO: implement a graph with adjacency lists, hint: use a Map.

-- | Create an empty graph
empty :: Graph a b
empty = Graph M.empty

-- | Add a vertex (node) to a graph
addVertex :: (Ord a) => a -> Graph a b -> Graph a b
addVertex v (Graph g) = Graph (M.insertWith (\_ old -> old) v [] g)

-- | Add a list of vertices to a graph
addVertices :: (Ord a) => [a] -> Graph a b -> Graph a b
addVertices vs g = foldr addVertex g vs

-- | Add an edge to a graph, the first parameter is the start vertex (of type a),
-- the second parameter the destination vertex, and the third parameter is the
-- label (of type b)
addEdge :: (Ord a) => a -> a -> b -> Graph a b -> Graph a b
addEdge src dst label (Graph graph) =
  Graph (M.insertWith (++) src [edge] graph)
  where
    edge = Edge src dst label

-- | Add an edge from start to destination, but also from destination to start,
-- with the same label.
addBiEdge :: (Ord a) => a -> a -> b -> Graph a b -> Graph a b
addBiEdge src dst label graph = addEdge src dst label (addEdge dst src label graph)

-- | Get all adjacent vertices (nodes) for a given node
adj :: (Ord a) => a -> Graph a b -> [Edge a b]
adj src (Graph g) = M.findWithDefault [] src g

-- | Get all vertices (nodes) in a graph
vertices :: Graph a b -> [a]
vertices (Graph g) = M.keys g

-- | Get all edges in a graph
edges :: Graph a b -> [Edge a b]
edges (Graph g) = concat $ M.elems g
