{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Day14.Soln where

import Control.Lens

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

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

type SandS = State SandE

data SandE = SandE {
  _sandPoints :: Set Point,
  _sandLowest :: Int
}

makeLenses ''SandE

shortFile :: FilePath
shortFile = "src/Day14/short-input.txt"

fullFile :: FilePath
fullFile = "src/Day14/full-input.txt"

soln :: FilePath -> IO ()
soln file = do
  content <- TIO.readFile file
  let input_lines = T.lines content
      segments = concatMap parseSegmentsLine input_lines
      points = concatMap segmentPoints segments
  -- mapM_ print points
  putStrLn $ "n points: " <> show (length points)
  putStrLn $ "Answer: " <> show (fillCavern points)

fillCavern :: [Point] -> Int
fillCavern walls = 
  let initial_points = Set.fromList walls
      lowest_row = maximum $ map fst walls

      sand_env = execState (dropSand (0, 500)) (SandE initial_points lowest_row)
      sand_points = (sand_env ^. sandPoints) `Set.difference` initial_points
   in Set.size sand_points

dropSand :: Point -> SandS Bool
dropSand cur_point@(m, n) = do
  -- traceM (show cur_point)
  lowest_m <- use sandLowest
  if m >= lowest_m
    then pure False
    else do 
      next_point_result <- findFirstPointM pointAvailable [(m + 1, n), (m + 1, n - 1), (m + 1, n + 1)]
      case next_point_result of 
        Nothing -> do 
          -- traceM ("no next: " <> show cur_point)
          sandPoints %= Set.insert cur_point
          pure True
        (Just next_point) -> do 
          settled <- dropSand next_point
          if settled 
            then dropSand cur_point
            else pure False
  where 
    pointAvailable :: Point -> SandS Bool
    pointAvailable point = uses sandPoints (not . Set.member point)

findFirstPointM :: (Point -> SandS Bool) -> [Point] -> SandS (Maybe Point)
findFirstPointM _ [] = pure Nothing
findFirstPointM f (p:ps) = do
  b <- f p
  if b then pure (Just p) else findFirstPointM f ps

segmentPoints :: Seg -> [Point]
segmentPoints ((m1, n1), (m2, n2)) = [(m, n) | m <- fromTo m1 m2, n <- fromTo n1 n2]
  where 
    fromTo :: Int -> Int -> [Int]
    fromTo x y = if x <= y then [x..y] else [y..x]

parseSegmentsLine :: T.Text -> [Seg]
parseSegmentsLine sline = 
  let points = map parsePoint $ T.splitOn " -> " sline
   in takeSegments points
  where 
    parsePoint :: T.Text -> Point
    parsePoint point_txt = 
      let [n, m] = map (read . T.unpack) $ T.splitOn "," point_txt
       in (m, n)

    takeSegments :: [Point] -> [Seg]
    takeSegments (p1:p2:ps) = (p1,p2) : takeSegments (p2:ps)
    takeSegments _ = []