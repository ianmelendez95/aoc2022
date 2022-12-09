{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Day8.Soln where

import Control.Lens

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.List
import Data.Maybe
import Data.Char

import Data.Set (Set)
import qualified Data.Set as Set

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as Vec

import System.FilePath.Posix

import Control.Monad.State.Lazy

import Debug.Trace
      
type AdjHighest = (Int, Int, Int, Int) -- (highest left, up, right, down)
type Point = (Int, Int)

data Dir = DUp | DRight | DDown | DLeft deriving (Eq, Ord)

type TreeS = State TreeE 

data TreeE = TreeE {
  _treeMaxRow   :: Int,
  _treeMaxCol   :: Int,
  _treeHeights  :: Map Point Int,
  _treeBlockers :: Map (Dir, Point) (Maybe Point)
}

makeLenses ''TreeE

shortFile :: FilePath
shortFile = "src/Day8/short-input.txt"

fullFile :: FilePath
fullFile = "src/Day8/full-input.txt"

soln :: FilePath -> IO ()
soln file = do
  content <- TIO.readFile file
  let tree_lines = T.lines content
      tree_heights = parseTreeLines tree_lines
      tree_scores = treeScores tree_heights
      -- answer = length . filter id . map snd . Map.toList $ tree_scores
  -- mapM_ print (Map.toList tree_heights)
  mapM_ print (Map.toList tree_scores)
  -- putStrLn $ "Answer: " <> show answer



-- treeVisibility :: Map Point Int -> Map Point Bool
-- treeVisibility heights = evalState run (TreeE heights Map.empty) 
--   where 
--     run :: TreeS (Map Point Bool)
--     run = do 
--       all_points <- uses treeHeights (map fst . Map.toList)
--       point_vis  <- traverse (tupleResultA isVisible) all_points
--       pure $ Map.fromList point_vis

-- tupleResultA :: Applicative m => (a -> m b) -> a -> m (a, b)
-- tupleResultA f x = (x,) <$> f x

-- isVisible :: Point -> TreeS Bool
-- isVisible point = or <$> traverse dirVisible [DUp, DDown, DLeft, DRight]
--   where 
--     dirVisible :: Dir -> TreeS Bool
--     dirVisible dir = do 
--       dir_highest  <- resolveBlocker dir point
--       point_height <- pointHeight point
--       pure $ point_height > dir_highest

-- resolveHighest :: Dir -> Point -> TreeS Int
-- resolveHighest dir point = do
--   let adj_point = adjPoint dir point
--   adj_exists <- pointExists adj_point
--   if not adj_exists 
--     then pure (-1) 
--     else do
--       existing_highest <- uses treeHighest (Map.lookup (dir, point))
--       case existing_highest of 
--         Just height -> pure height
--         Nothing -> do 
--           adj_height  <- pointHeight adj_point
--           adj_highest <- max adj_height <$> resolveHighest dir adj_point
--           treeHighest%= Map.insert (dir, point) adj_highest
--           pure adj_highest

-- treeBlockers :: Map Point Int -> Map (Dir, Point) Bool
-- treeBlockers heights = evalState run (TreeE heights Map.empty) 
--   where 
--     run :: TreeS (Map Point Bool)
--     run = do 
--       all_points <- uses treeHeights (map fst . Map.toList)
--       point_vis  <- traverse (tupleResultA isVisible) all_points
--       pure $ Map.fromList point_vis

treeScores :: Map Point Int -> Map Point Int
treeScores heights = 
  let (max_m, max_n) = max_point
   in evalState run (TreeE max_m max_n heights Map.empty) 
  where 
    run :: TreeS (Map Point Int)
    run = do 
      all_points <- uses treeHeights (map fst . Map.toList)
      point_vis  <- traverse (tupleResultA treeScore) all_points
      pure $ Map.fromList point_vis

    max_point :: Point
    max_point = 
      let (Just (max_point, _)) = Map.lookupMax heights
      in max_point

    tupleResultA :: Applicative m => (a -> m b) -> a -> m (a, b)
    tupleResultA f x = (x,) <$> f x

treeScore :: Point -> TreeS Int
treeScore point = do 
  scores <- traverse (`treeScoreDir` point) [DDown, DLeft, DUp, DRight]
  pure $ product scores

treeScoreDir :: Dir -> Point -> TreeS Int
treeScoreDir dir point = do 
  blocker_result <- resolveBlocker dir point
  case blocker_result of 
    Nothing -> dirTreeCount dir point 
    Just blocker -> pure $ adjPointDistance point blocker

adjPointDistance :: Point -> Point -> Int
adjPointDistance (m1, n1) (m2, n2) 
  | m1 == m2  = abs (n1 - n2)
  | otherwise = abs (m1 - m2)

dirTreeCount :: Dir -> Point -> TreeS Int
dirTreeCount DLeft (_, n) = pure n
dirTreeCount DUp   (m, _) = pure m 
dirTreeCount DRight (_, n) = do 
  max_col <- use treeMaxCol
  pure (max_col - n)
dirTreeCount DDown (m, _) = do 
  max_row <- use treeMaxRow
  pure (max_row - m)

resolveBlocker :: Dir -> Point -> TreeS (Maybe Point)
resolveBlocker dir cur_point = do 
  let adj_point = pointAdj dir cur_point
  adj_exists <- pointExists adj_point
  if not adj_exists 
    then pure Nothing
    else do
      existing_highest <- uses treeBlockers (Map.lookup (dir, cur_point))
      case existing_highest of 
        Just blocker -> pure blocker
        Nothing -> do 
          cur_height <- pointHeight cur_point
          adj_height <- pointHeight adj_point
          result <- 
            if adj_height >= cur_height
              then pure $ Just adj_point
              else do
                adj_blockers <- resolveBlockers dir adj_point
                findBlocker cur_height adj_blockers
          treeBlockers %= Map.insert (dir, cur_point) result
          pure result
  where 
    findBlocker :: Int -> [Point] -> TreeS (Maybe Point)
    findBlocker _ [] = pure Nothing
    findBlocker h (p:ps) = do 
      p_height <- pointHeight p
      if p_height >= h
        then pure (Just p)
        else findBlocker h ps

resolveBlockers :: Dir -> Point -> TreeS [Point]
resolveBlockers dir cur_point = do 
  adj_blocker_m <- resolveBlocker dir cur_point
  case adj_blocker_m of 
    Nothing -> pure []
    (Just adj_blocker) -> do 
      rest_blockers <- resolveBlockers dir adj_blocker 
      pure $ adj_blocker : rest_blockers


pointAdj :: Dir -> Point -> Point
pointAdj DUp    (m, n) = (m - 1, n)
pointAdj DDown  (m, n) = (m + 1, n)
pointAdj DLeft  (m, n) = (m, n - 1)
pointAdj DRight (m, n) = (m, n + 1)

pointExists :: Point -> TreeS Bool
pointExists point = uses treeHeights (point `Map.member`)

pointHeight :: Point -> TreeS Int
pointHeight point = uses treeHeights (fromMaybe 0 . Map.lookup point)

-- computeHighestUp :: Point -> TreeS Int
-- computeHighestUp point = 

parseTreeLines :: [T.Text] -> Map Point Int
parseTreeLines input = 
  foldr Map.union Map.empty (zipWith parseRow [0..] input)
  where 
    parseRow :: Int -> T.Text -> Map Point Int
    parseRow row line = 
      let heights = map digitToInt (T.unpack line)
       in Map.fromList $ zipWith (\col h -> ((row, col), h)) [0..] heights