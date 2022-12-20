module Day16.FloydWarshall where 

data Edge = Edge {
  edgeStart  :: Int,
  edgeEnd    :: Int,
  edgeWeight :: Int
}

test_edges :: [Edge]
test_edges = 
  [ Edge 2 1 4
  , Edge 2 3 3
  , Edge 1 3 (-2)
  , Edge 3 4 2
  , Edge 4 2 (-1)
  ]