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
type Seg = (Point, Point)
type Bounds = (Int, Int)
type Sensor = (Point, Point)

type SandS = State SandE

data SandE = SandE {
  _sandPoints :: Set Point,
  _sandLowest :: Int
}

makeLenses ''SandE

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
      boundaries = foldMap (sensorBoundary bounds) sensors
      -- y_no_beacon = y_covered `Set.difference` Set.fromList y_beacons
      -- all_y = Set.fromList [(x, y) | x <- [d_min..d_max], y <- [d_min..d_max]]
  -- mapM_ (\s -> print (s, sensorLineRange y s)) sensors
  -- putStrLn $ "No Beacons: " <> show y_no_beacon
  putStrLn $ "All Points Count: " <> show (Set.size boundaries)
  -- putStrLn $ "Answer: "     <> show (Set.size y_no_beacon)

sensorBoundary :: Bounds -> Sensor -> Set Point
sensorBoundary bounds@(d_min, d_max) sensor@((s_x, s_y), (_, _)) = 
  let s_range = sensorRange sensor
      y_min = (s_y - s_range) - 1
      y_max = (s_y + s_range) + 1
   in foldMap boundaryForY [y_min..y_max]
  where 
    boundaryForY :: Int -> Set Point
    boundaryForY y = 
      let leftover_range = sensor_range - abs (s_y - y)
          x_start = s_x - (leftover_range + 1)
          x_end   = s_x + (leftover_range + 1)
       in Set.fromList (filter (inBounds bounds) [(x_start, y), (x_end, y)])

    sensor_range = sensorRange sensor

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
