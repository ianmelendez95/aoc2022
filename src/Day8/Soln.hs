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

data TreeE = TreeE{
  _treeHeights :: Map Point Int,
  _treeHighest :: Map (Dir, Point) Int
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
      tree_vis = treeVisibility tree_heights
      answer = length . filter id . map snd . Map.toList $ tree_vis
  mapM_ print (Map.toList tree_heights)
  mapM_ print (Map.toList tree_vis)
  putStrLn $ "Answer: " <> show answer


treeVisibility :: Map Point Int -> Map Point Bool
treeVisibility heights = evalState run (TreeE heights Map.empty) 
  where 
    run :: TreeS (Map Point Bool)
    run = do 
      all_points <- uses treeHeights (map fst . Map.toList)
      point_vis  <- traverse (tupleResultA isVisible) all_points
      pure $ Map.fromList point_vis

tupleResultA :: Applicative m => (a -> m b) -> a -> m (a, b)
tupleResultA f x = (x,) <$> f x

isVisible :: Point -> TreeS Bool
isVisible point = or <$> traverse dirVisible [DUp, DDown, DLeft, DRight]
  where 
    dirVisible :: Dir -> TreeS Bool
    dirVisible dir = do 
      dir_highest  <- resolveHighest dir point
      point_height <- pointHeight point
      pure $ point_height > dir_highest

resolveHighest :: Dir -> Point -> TreeS Int
resolveHighest dir point = do
  let adj_point = adjPoint dir point
  adj_exists <- pointExists adj_point
  if not adj_exists 
    then pure (-1) 
    else do
      existing_highest <- uses treeHighest (Map.lookup (dir, point))
      case existing_highest of 
        Just height -> pure height
        Nothing -> do 
          adj_height  <- pointHeight adj_point
          adj_highest <- max adj_height <$> resolveHighest dir adj_point
          treeHighest%= Map.insert (dir, point) adj_highest
          pure adj_highest
  where
    adjPoint :: Dir -> Point -> Point
    adjPoint DUp    (m, n) = (m - 1, n)
    adjPoint DDown  (m, n) = (m + 1, n)
    adjPoint DLeft  (m, n) = (m, n - 1)
    adjPoint DRight (m, n) = (m, n + 1)

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