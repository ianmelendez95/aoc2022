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
  _valvesByName  :: Map T.Text Valve,
  _valveEdges    :: Map Edge Int,
  _valvesVisited :: Set T.Text
}

makeLenses ''ValveE

shortFile :: FilePath
shortFile = "src/Day16/short-input.txt"

fullFile :: FilePath
fullFile = "src/Day16/full-input.txt"

soln :: FilePath -> IO ()
soln file = do
  input_lines <- T.lines <$> TIO.readFile file
  let valves = map readValveLine input_lines
      all_shortest_paths = valveShortestPaths valves

      flowy_valves = filter ((> 0) . valveFlow) valves
      flowy_valve_labels = Set.fromList . map valveName $ flowy_valves
      flowy_shortest_paths = Map.filterWithKey (filterFlowyEntry flowy_valve_labels) all_shortest_paths

      max_flow = findMaxFlow flowy_valves flowy_shortest_paths
  -- mapM_ print valves
  -- mapM_ print (Map.toList all_shortest_paths)
  putStrLn $ "Total Valve Count: "   <> show (length valves)
  putStrLn $ "Total Edges Count: "   <> show (Map.size all_shortest_paths)
  putStrLn $ "Flowing Valve Count: " <> show (length flowy_valve_labels)
  putStrLn $ "Flowing Edges Count: " <> show (Map.size flowy_shortest_paths)

  putStrLn $ "Answer: " <> show max_flow
  where 
    filterFlowyEntry :: Set T.Text -> Edge -> Int -> Bool
    filterFlowyEntry flowy_labels (p, q) _ = p `Set.member` flowy_labels && q `Set.member` flowy_labels

findMaxFlow :: [Valve] -> Map Edge Int -> Int
findMaxFlow valves edges = 
  evalState (maximum <$> traverse (openValves 30) valves) 
            (ValveE valve_map edges Set.empty)
  where 
    valve_map :: Map T.Text Valve
    valve_map = Map.fromList $ map (\v -> (valveName v, v)) valves

openValves :: Int -> Valve -> ValveS Int
openValves time_left cur_valve 
  | time_left <= 0 = pure 0  -- no time to even open the current valve
  | otherwise  = do
      valvesVisited %= Set.insert (valveName cur_valve)  -- add this valve to visited
      adj_info <- getAdjInfo
      max_flow <- maximumFlow <$> traverse (uncurry openValves) adj_info
      let this_flow = (valveFlow cur_valve) * (time_left - 1)
      -- traceM (show this_flow)
      pure $ max_flow + this_flow
  where 
    getAdjInfo :: ValveS [(Int, Valve)]
    getAdjInfo = do
      visited <- use valvesVisited
      traverse getValveInfo (filter (`Set.member` visited) (valveAdj cur_valve))
    
    maximumFlow :: [Int] -> Int
    maximumFlow [] = 0
    maximumFlow xs = maximum xs

    -- | return (time left after opening this AND traversing to..., target valve)
    getValveInfo :: T.Text -> ValveS (Int, Valve)
    getValveInfo name = (,) <$> ((\t -> t - 1) . (time_left-) <$> getValveTime name) <*> getValve name

    getValveTime :: T.Text -> ValveS Int
    getValveTime name = uses valveEdges (fromJust . Map.lookup (valveName cur_valve, name))

    getValve :: T.Text -> ValveS Valve
    getValve name = uses valvesByName (fromJust . Map.lookup name)

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