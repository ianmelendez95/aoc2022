{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Day16.FloydWarshall where 

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Control.Monad
import Control.Monad.State.Lazy

import Control.Lens

type Edge = (Int, Int)

type EdgeWeights = Map Edge Int

type PathS = State PathE

data PathE = PathE {
  _pathEdges    :: EdgeWeights,
  _pathShortest :: Map Edge Int
}

makeLenses ''PathE

findShortestPaths :: EdgeWeights -> Map Edge Int
findShortestPaths = _

test_edges :: EdgeWeights
test_edges = Map.fromList
  [ ((2, 1), 4)
  , ((2, 3), 3)
  , ((1, 3), (-2))
  , ((3, 4), 2)
  , ((4, 2), (-1))
  ]
