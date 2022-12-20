{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Day16.FloydWarshall where 

import Data.List
import Data.Maybe

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Control.Monad
import Control.Monad.State.Lazy

import Control.Lens

import Data.Set (Set)
import qualified Data.Set as Set

import Debug.Trace

type Edge = (Int, Int)

type EdgeWeights = Map Edge Int

type PathS = State PathE

data PathE = PathE {
  _pathEdges :: EdgeWeights,
  _pathDists :: Map Edge Int
}

makeLenses ''PathE

findShortestPaths :: EdgeWeights -> Map Edge Int
findShortestPaths weights = 
  let path_env = PathE { _pathEdges = weights, _pathDists = weights <> self_dists }
   in (execState findShortest path_env) ^. pathDists
  where 
    findShortest :: PathS ()
    findShortest = mapM_ (\k -> mapM_ (findShortestForEdge k) edge_combs) vertices

    self_dists :: Map Edge Int
    self_dists = Map.fromList (zip (zip vertices vertices) (repeat 0))

    edge_combs :: [Edge]
    edge_combs = [(i, j) | i <- vertices, j <- vertices]

    vertices :: [Int]
    vertices = nub . concatMap ((\(x, y) -> [x, y]) . fst) $ Map.toList weights

findShortestForEdge :: Int -> Edge -> PathS ()
findShortestForEdge k (i, j) = do 
  mdist_ik <- getDistance (i, k)
  mdist_kj <- getDistance (k, j)
  case (mdist_ik, mdist_kj) of 
    (Just dist_ik, Just dist_kj) -> 
      pathDists %= Map.insertWith min (i, j) (dist_ik + dist_kj)
    _ -> pure ()

getDistance :: Edge -> PathS (Maybe Int)
getDistance edge = uses pathDists (Map.lookup edge)

test_edges :: EdgeWeights
test_edges = Map.fromList
  [ ((2, 1), 4)
  , ((2, 3), 3)
  , ((1, 3), (-2))
  , ((3, 4), 2)
  , ((4, 2), (-1))
  ]
