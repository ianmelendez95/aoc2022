{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Day15.Soln where

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

import Debug.Trace

type Point = (Int, Int)
type Bounds = (Int, Int)
type Sensor = (Point, Point)
type Boundary = [Seg]

data Seg = Seg {
  segStart :: Point,
  segEnd   :: Point,
  segPos   :: Bool,
  segYInt  :: Int
} 

instance Show Seg where 
  show = showSegPoints

showSegPoints :: Seg -> String
showSegPoints s = "[[" <> show (segStart s) <> " -> " <> show (segEnd s) <> "]]"

-- shortFile :: (FilePath, Int)
-- shortFile = ("src/Day15/short-input.txt", 10)
shortFile :: (FilePath, Bounds)
shortFile = ("src/Day15/short-input.txt", (0, 20))

-- fullFile :: (FilePath, Int)
-- fullFile = ("src/Day15/full-input.txt", 2000000)
fullFile :: (FilePath, Bounds)
fullFile = ("src/Day15/full-input.txt", (0, 4000000))

-- soln :: (FilePath, Int) -> IO ()
-- soln (file, y) = do
soln :: (FilePath, Bounds) -> IO ()
soln (file, bounds@(d_min, d_max)) = do
  content <- TIO.readFile file
  let input_lines = T.lines content
      sensors = map parseSensorLine input_lines
      -- y_beacons = map fst . filter ((y ==) . snd) . map snd $ sensors
      -- y_covered = foldl' Set.union Set.empty (map (sensorLineRange y) sensors)
      boundaries = map (\s -> (s, sensorBoundary s)) sensors

      all_segs = concatMap snd boundaries
      pos_segs = filter segPos all_segs
      neg_segs = filter (not . segPos) all_segs

      pos_col_inc = pairParallelByIncidentColinear pos_segs
      neg_col_inc = pairParallelByIncidentColinear neg_segs

      -- first_sensor = head sensors
      -- y_no_beacon = y_covered `Set.difference` Set.fromList y_beacons
      -- all_y = Set.fromList [(x, y) | x <- [d_min..d_max], y <- [d_min..d_max]]
  -- mapM_ (\s -> print (s, sensorLineRange y s)) sensors
  -- putStrLn $ "No Beacons: " <> show y_no_beacon
  -- putStrLn $ "All Points Count: " <> show (Set.size boundaries)
  -- mapM_ (print . Set.size) boundaries
  -- mapM_ (print . Set.size) boundaries
  -- putStrLn $ "Answer: "     <> show (Set.size y_no_beacon)
  -- putStrLn $ "First Sensor: " <> show first_sensor
  -- putStrLn $ "First Range: " <> show (sensorRange first_sensor)
  -- putStrLn $ "First Boundary Count: " <> show (Set.size $ sensorBoundary bounds first_sensor)
  -- mapM_ printBoundary boundaries
  mapM_ print (pos_col_inc <> neg_col_inc)
  where 
    printBoundary b = do 
      print (fst b)
      mapM_ print (snd b)

-- pairColinear :: [Seg] -> [(Seg, Seg)]
-- pairColinear all_segs = 
--   let pos = filter segPos all_segs
--       neg = filter (not . segPos) all_segs

--    in pairParallelColinear pos <> pairParallelColinear neg

perpendicularSegmentsIntersect :: Seg -> Seg -> Bool
perpendicularSegmentsIntersect s_pos s_neg = 
  let pos_y_int = segYInt s_pos
      neg_y_int = segYInt s_neg
      y_int_diff = neg_y_int - pos_y_int
   in if odd y_int_diff
        then False -- cannot intersect, they "pass through" each other
        else let x_int = y_int_diff `div` 2
                 y_int = x_int + pos_y_int
                 int = (x_int, y_int)
              in containsPoint s_pos int && containsPoint s_neg int 
  where 
    containsPoint :: Seg -> Point -> Bool 
    containsPoint s p = p > segStart s && p < segEnd s

-- | given parallel segments, pair up colinear, incident lines
pairParallelByIncidentColinear :: [Seg] -> [(Seg, (Seg, Seg))]
pairParallelByIncidentColinear parallel_segs = 
  let col = groupColinear parallel_segs
   in concatMap colinearCollidingPairs col
  where 
    colinearCollidingPairs :: [Seg] -> [(Seg, (Seg, Seg))]
    colinearCollidingPairs = map (pairResult colinearCollidingIntersect) . pairsBy sortedColinearCollide . sortOn segStart

    colinearCollidingIntersect :: (Seg, Seg) -> Seg
    colinearCollidingIntersect (s1, s2) = 
      let [_, i1, i2, _] = sort [segStart s1, segEnd s1, segStart s2, segEnd s2]
       in segmentFromPoints i1 i2

    sortedColinearCollide :: Seg -> Seg -> Bool
    sortedColinearCollide s1 s2 = segStart s2 <= segEnd s1
    
    groupColinear :: [Seg] -> [[Seg]]
    groupColinear = groupBy (grouping segYInt) . sortOn segYInt

pairResult :: (a -> b) -> a -> (b, a)
pairResult f x = (f x, x)

grouping :: Eq b => (a -> b) -> a -> a -> Bool
grouping f x y = f x == f y

pairsBy :: (a -> a -> Bool) -> [a] -> [(a, a)]
pairsBy f (x:y:xs) = if f x y then (x,y) : pairsBy f (y:xs) else pairsBy f (y:xs)
pairsBy _ _ = []

sensorBoundary :: Sensor -> [Seg]
sensorBoundary sensor@((s_x, s_y), (_, _)) = 
  let s_range = sensorRange sensor
      left    = (s_x - s_range - 1, s_y)
      top     = (s_x, s_y - s_range - 1)
      right   = (s_x + s_range + 1, s_y)
      bottom  = (s_x, s_y + s_range + 1)
   in map (uncurry segmentFromPoints) [(left, top), (left, bottom), (top, right), (bottom, right)]
  
segmentFromPoints :: Point -> Point -> Seg
segmentFromPoints p1@(x1, y1) p2@(x2, y2) = 
  let slope = (y2 - y1) `div` (x2 - x1)
      y_int = y1 - (slope * x1)
   in Seg { segStart = min p1 p2, segEnd = max p1 p2, segPos = slope > 0, segYInt = y_int }

inBounds :: Bounds -> Point -> Bool
inBounds (d_min, d_max) (x, y) = x >= d_min && x <= d_max && y >= d_min && y <= d_max

-- sensorLineRange :: Int -> Sensor -> Set Int
-- sensorLineRange y sensor@((s_x, s_y), _) = 
--   let s_range = sensorRange sensor
--       leftover_range = s_range - abs (s_y - y)
--       x_start = s_x - leftover_range
--       x_end   = s_x + leftover_range
--    in Set.fromList [x_start..x_end]

sensorRange :: Sensor -> Int
sensorRange (s, b) = pointDistance s b

pointDistance :: Point -> Point -> Int
pointDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

parseSensorLine :: T.Text -> (Point, Point)
parseSensorLine s_line = 
  let s_words = T.words s_line
      s_x = parseCoord . T.init $ s_words !! 2
      s_y = parseCoord . T.init $ s_words !! 3
      b_x = parseCoord . T.init $ s_words !! 8
      b_y = parseCoord          $ s_words !! 9
    in ((s_x, s_y), (b_x, b_y))
  where
    parseCoord :: T.Text -> Int
    parseCoord c_word = 
      let [_, val] = T.splitOn "=" c_word
       in read . T.unpack $ val