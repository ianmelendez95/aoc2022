{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

type Edge a = (a, a)

type PathS a = State (PathE a)

data PathE a = PathE {
  _pathEdges :: Map (Edge a) Int,
  _pathDists :: Map (Edge a) Int
}

makeLenses ''PathE

floydWarshall :: Ord a => Map (Edge a) Int -> Map (Edge a) Int
floydWarshall = findShortestPaths

findShortestPaths :: forall a. Ord a => Map (Edge a) Int -> Map (Edge a) Int
findShortestPaths weights = 
  let path_env = PathE { _pathEdges = weights, _pathDists = weights <> self_dists }
   in (execState findShortest path_env) ^. pathDists
  where 
    findShortest :: PathS a ()
    findShortest = mapM_ (\k -> mapM_ (findShortestForEdge k) edge_combs) vertices

    self_dists :: Map (Edge a) Int
    self_dists = Map.fromList (zip (zip vertices vertices) (repeat 0))

    edge_combs :: [Edge a]
    edge_combs = [(i, j) | i <- vertices, j <- vertices]

    vertices = nub . concatMap ((\(x, y) -> [x, y]) . fst) $ Map.toList weights

findShortestForEdge :: Ord a => a -> Edge a -> PathS a ()
findShortestForEdge k (i, j) = do 
  mdist_ik <- getDistance (i, k)
  mdist_kj <- getDistance (k, j)
  case (mdist_ik, mdist_kj) of 
    (Just dist_ik, Just dist_kj) -> 
      pathDists %= Map.insertWith min (i, j) (dist_ik + dist_kj)
    _ -> pure ()

getDistance :: Ord a => Edge a -> PathS a (Maybe Int)
getDistance edge = uses pathDists (Map.lookup edge)

test_edges :: Map (Edge Int) Int
test_edges = Map.fromList
  [ ((2, 1), 4)
  , ((2, 3), 3)
  , ((1, 3), (-2))
  , ((3, 4), 2)
  , ((4, 2), (-1))
  ]
