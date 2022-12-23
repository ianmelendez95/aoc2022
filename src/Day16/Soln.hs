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
type AdjMap = Map T.Text [T.Text]

data Valve = Valve {
  _valveName :: T.Text,
  _valveFlow :: Int,
  _valveAdj  :: [T.Text]
} deriving Show

type ValveS = State ValveE

data ValveE = ValveE {
  _valvesByName  :: Map T.Text Valve,
  _valveEdges    :: Map Edge Int,
  _valvesVisited :: Set T.Text
}

makeLenses ''Valve
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

      adj_map = adjMapFromEdges all_shortest_paths
      valves' = map (updateValveAdjs adj_map) valves
      valve_AA = head $ filter ((== "AA") . view valveName) valves'

      max_flow = findMaxFlow valve_AA valves' all_shortest_paths
  -- mapM_ print valves
  -- mapM_ print (Map.toList all_shortest_paths)
  -- putStrLn $ "Total Valve Count: "   <> show (length valves)
  -- putStrLn $ "Total Edges Count: "   <> show (Map.size all_shortest_paths)

  -- putStrLn "Flowing Edges:"
  -- mapM_ print (Map.toList all_shortest_paths)

  putStrLn $ "Answer: " <> show max_flow

findMaxFlow :: Valve -> [Valve] -> Map Edge Int -> Int
findMaxFlow valveAA valves edges = 
  evalState (openValves 30 valveAA) (ValveE valve_map edges Set.empty)
  where 
    valve_map :: Map T.Text Valve
    valve_map = Map.fromList $ map (\v -> (v ^. valveName, v)) valves

updateValveAdjs :: AdjMap -> Valve -> Valve
updateValveAdjs adj_map valve = 
  let new_adj = fromJust . Map.lookup (valve ^. valveName) $ adj_map
   in set valveAdj new_adj valve

adjMapFromEdges :: Map Edge Int -> AdjMap
adjMapFromEdges edges = foldl' insertEdge Map.empty (map fst . Map.toList $ edges)
  where 
    insertEdge :: AdjMap -> Edge -> AdjMap
    insertEdge adjs (k, v) = Map.insertWith (++) k [v] adjs

openValves :: Int -> Valve -> ValveS Int
openValves time_left cur_valve 
  | time_left <= 0 = pure 0  -- no time to even open the current valve
  | otherwise  = do
      valvesVisited %= Set.insert (cur_valve ^. valveName)  -- add this valve to visited
      adj_info <- getAdjInfo
      max_flow <- maximumFlow <$> traverse (uncurry openValves) adj_info
      let this_flow = (cur_valve ^. valveFlow) * (time_left - 1)
      pure $ max_flow + this_flow
  where 
    getAdjInfo :: ValveS [(Int, Valve)]
    getAdjInfo = do
      adj_valves <- traverse getValve (cur_valve ^. valveAdj)
      visitable  <- filterM canVisit adj_valves
      traverse getValveInfo visitable
    
    canVisit :: Valve -> ValveS Bool
    canVisit valve = do
      if view valveFlow valve <= 0 
        then pure False
        else uses valvesVisited ((view valveName valve) `Set.notMember`)
    
    maximumFlow :: [Int] -> Int
    maximumFlow [] = 0
    maximumFlow xs = maximum xs

    -- | return (time left after opening this AND traversing to..., target valve)
    getValveInfo :: Valve -> ValveS (Int, Valve)
    getValveInfo valve = (,) <$> (((time_left - 1) -) <$> getValveTime (view valveName valve)) <*> pure valve

    getValveTime :: T.Text -> ValveS Int
    getValveTime name = uses valveEdges (fromJust . Map.lookup (view valveName cur_valve, name))

    getValve :: T.Text -> ValveS Valve
    getValve name = uses valvesByName (fromJust . Map.lookup name)

valveShortestPaths :: [Valve] -> Map (T.Text, T.Text) Int
valveShortestPaths valves = 
  let edges_txt :: [Edge]
      edges_txt = concatMap valveEdges valves
   in floydWarshall (Map.fromList (zip edges_txt (repeat 1)))
  where 
    valveEdges :: Valve -> [(T.Text, T.Text)]
    valveEdges valve = zip (repeat $ view valveName valve) (view valveAdj valve)

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