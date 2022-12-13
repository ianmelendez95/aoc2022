{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Day12.Soln where

import Control.Lens

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

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

import Debug.Trace

type HeightMap = Map Point Int
type Point = (Int, Int)

type HikeS = State HikeE

data HikeE = HikeE {
  _hikeHeights  :: HeightMap,
  _hikeStart    :: Point,
  _hikeEnd      :: Point, 
  _hikeVisitedQ :: Set (Int, Point),
  _hikeVisited  :: Map Point Int
}

makeLenses ''HikeE

shortFile :: FilePath
shortFile = "src/Day12/short-input.txt"

fullFile :: FilePath
fullFile = "src/Day12/full-input.txt"

soln :: FilePath -> IO ()
soln file = do
  content <- TIO.readFile file
  let input_lines = T.lines content
      (height_map, start, end) = parseMap input_lines
      answer = findShortestRoute height_map start end
  -- print start
  -- print end
  -- mapM_ print (Map.toList height_map)

  putStrLn $ "Answer: " <> show answer

findShortestRoute :: HeightMap -> Point -> Point -> Int
findShortestRoute heights start end = 
  let zero_heights = map fst . filter ((0 ==) . snd) . Map.toList $ heights
      initial_visited_q = Set.fromList (zip (repeat (0 :: Int)) zero_heights)
      initial_visited   = Map.fromList (zip zero_heights (repeat (0 :: Int)))
      (Just end_dist) = evalState (explore start) (HikeE heights start end initial_visited_q initial_visited)
   in end_dist

explore :: Point -> HikeS (Maybe Int)
explore cur_point@(m, n) = do 
  -- traceM (show cur_point)
  end_point <- use hikeEnd
  dist      <- uses hikeVisited (Map.findWithDefault (error "") cur_point)
  if cur_point == end_point
    then pure (Just dist) -- reached end
    else do 
      cur_height <- uses hikeHeights (Map.findWithDefault (error "") cur_point)
      neighbors  <- findExplorableNeighbors cur_height
      mapM_ (considerNeighbor (dist + 1)) neighbors
      next_best  <- popBestPoint
      explore next_best
  where 
    -- | Check if neighbor is candidate for exploration,
    -- | and if so add to the queue.
    considerNeighbor :: Int -> Point -> HikeS ()
    considerNeighbor dist neighbor = do 
      neighbor_dist_result <- uses hikeVisited (Map.lookup neighbor)
      let add_neighbor = maybe True (dist <) neighbor_dist_result
      if add_neighbor then addNeighbor dist neighbor else pure ()
    
    addNeighbor :: Int -> Point -> HikeS ()
    addNeighbor dist neighbor = do
      hikeVisited  %= Map.insert neighbor dist 
      hikeVisitedQ %= Set.insert (dist, neighbor)

    findExplorableNeighbors :: Int -> HikeS [Point]
    findExplorableNeighbors cur_height = filterTraversablePoints cur_height
      [ (m - 1, n)
      , (m + 1, n)
      , (m, n - 1)
      , (m, n + 1)
      ]

    filterTraversablePoints :: Int -> [Point] -> HikeS [Point]
    filterTraversablePoints _ [] = pure []
    filterTraversablePoints cur_height (p:ps) = do
      height_result <- uses hikeHeights (Map.lookup p)
      case height_result of 
        Nothing -> filterTraversablePoints cur_height ps
        (Just neighbor_height) -> 
          if (neighbor_height - cur_height) > 1
            then filterTraversablePoints cur_height ps
            else (p:) <$> filterTraversablePoints cur_height ps

popBestPoint :: HikeS Point
popBestPoint = do 
  (best, new_set) <- uses hikeVisitedQ Set.deleteFindMin
  hikeVisitedQ .= new_set
  pure (snd best)

parseMap :: [T.Text] -> (HeightMap, Point, Point)
parseMap lines = 
  let parsed_lines = zipWith parseMapLine [0..] lines
      (hmap, Just start, Just end) = foldl1' joinLineResults parsed_lines
   in (hmap, start, end)
  where 
    joinLineResults :: (HeightMap, Maybe Point, Maybe Point) -> (HeightMap, Maybe Point, Maybe Point) -> (HeightMap, Maybe Point, Maybe Point)
    joinLineResults (hmap1, start1, end1) (hmap2, start2, end2) = 
      (hmap1 `Map.union` hmap2, joinMaybes start1 start2, joinMaybes end1 end2)
    
    joinMaybes :: Maybe a -> Maybe a -> Maybe a
    joinMaybes (Just x) _ = Just x
    joinMaybes _ y        = y

    parseMapLine :: Int -> T.Text -> (HeightMap, Maybe Point, Maybe Point)
    parseMapLine row line = 
      let points = zipWith (\col height_char -> ((row, col), height_char)) [0..] (T.unpack line)
       in foldl' collectPoints (Map.empty, Nothing, Nothing) points
    
    collectPoints :: (HeightMap, Maybe Point, Maybe Point) -> (Point, Char) -> (HeightMap, Maybe Point, Maybe Point)
    collectPoints (hmap, mstart, mend) (point, char) = 
      let mstart' = if char == 'S' then Just point else mstart
          mend'   = if char == 'E' then Just point else mend
       in (Map.insert point (charHeight char) hmap, mstart', mend')
    
    charHeight :: Char -> Int
    charHeight 'S' = charHeight 'a'
    charHeight 'E' = charHeight 'z'
    charHeight c   = ord c   - ord 'a'