{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Day16.Soln where

import Control.Lens

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.Functor
import Data.Void
import Data.List
import Data.Maybe
import Data.Char
import Data.Ord

import Data.Set (Set)
import qualified Data.Set as Set

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as Vec

import System.FilePath.Posix

import Control.Monad
import Control.Monad.State.Lazy

import Text.Megaparsec hiding (State (..))
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Day16.FloydWarshall (floydWarshall)

import Debug.Trace

type Edge = (T.Text, T.Text)

data Valve = Valve {
  valveName :: T.Text,
  valveFlow :: Int,
  valveAdj  :: [T.Text]
} deriving Show

type ValveS = State ValveE

data ValveE = ValveE {
  valvesByName :: Map T.Text Int,
  valvesVisited :: Set T.Text
}

shortFile :: FilePath
shortFile = "src/Day16/short-input.txt"

fullFile :: FilePath
fullFile = "src/Day16/full-input.txt"

soln :: FilePath -> IO ()
soln file = do
  input_lines <- T.lines <$> TIO.readFile file
  let valves = map readValveLine input_lines
      all_shortest_paths = valveShortestPaths valves
      flowy_valve_labels = Set.fromList . map valveName . filter ((> 0) . valveFlow) $ valves
      flowy_shortest_paths = Map.filterWithKey (filterFlowyEntry flowy_valve_labels) all_shortest_paths
  -- mapM_ print valves
  -- mapM_ print (Map.toList all_shortest_paths)
  putStrLn $ "Total Valve Count: " <> show (length valves)
  putStrLn $ "Total Edges Count: " <> show (Map.size all_shortest_paths)
  putStrLn $ "Flowing Valve Count: " <> show (length flowy_valve_labels)
  putStrLn $ "Flowing Edges Count: " <> show (Map.size flowy_shortest_paths)
  where 
    filterFlowyEntry :: Set T.Text -> Edge -> Int -> Bool
    filterFlowyEntry flowy_labels (p, q) _ = p `Set.member` flowy_labels && q `Set.member` flowy_labels

valveShortestPaths :: [Valve] -> Map (T.Text, T.Text) Int
valveShortestPaths valves = 
  let edges_txt :: [Edge]
      edges_txt = concatMap valveEdges valves
   in floydWarshall (Map.fromList (zip edges_txt (repeat 1)))
  where 
    valveEdges :: Valve -> [(T.Text, T.Text)]
    valveEdges valve = zip (repeat $ valveName valve) (valveAdj valve)

readValveLine :: T.Text -> Valve
readValveLine input = 
  let ws = T.words input
      vname = ws !! 1
      vflow = parseFlow (ws !! 4)
      vadj  = parseAdj (drop 9 ws)
   in Valve vname vflow vadj
  where 
    parseFlow :: T.Text -> Int
    parseFlow = read . T.unpack . T.init . (!! 1) . T.splitOn "="

    parseAdj :: [T.Text] -> [T.Text]
    parseAdj = map (T.filter isLetter)