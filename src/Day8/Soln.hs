{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

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

type TreeS = State TreeE 

data TreeE = TreeE{
  _treeHeights      :: Map Point Int,
  _treeHighestLeft  :: Map Point Int,
  _treeHighestRight :: Map Point Int,
  _treeHighestUp    :: Map Point Int,
  _treeHighestDown  :: Map Point Int 
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
  mapM_ print (Map.toList tree_heights)

parseTreeLines :: [T.Text] -> Map Point Int
parseTreeLines input = 
  foldr Map.union Map.empty (zipWith parseRow [0..] input)
  where 
    parseRow :: Int -> T.Text -> Map Point Int
    parseRow row line = 
      let heights = map digitToInt (T.unpack line)
       in Map.fromList $ zipWith (\col h -> ((row, col), h)) [0..] heights

-- resolveTreeVisible :: Point -> TreeS Bool
-- resolveTreeVisible point = do
--   existing_vis <- uses treeVisible (Map.lookup point)
--   case existing_vis of 
--     Just vis -> pure vis
--     Nothing -> do 
--       left_vis  <- resolveTreeVisible (left point)
--       right_vis <- resolveTreeVisible (right point)
--       up_vis    <- resolveTreeVisible (up point)
--       down_vis  <- resolveTreeVisible (down point)
--       let vis = left_vs || right_vis || up_vis || down_vis
--       undefined

resolveHighestUp :: Point -> TreeS Int
resolveHighestUp point = do
  existing_highest <- uses treeHighestUp (Map.lookup point)
  case existing_highest of 
    Just height -> pure height
    Nothing -> do 
      let up_point = up point
      up_height  <- getHeight up_point
      up_highest <- max up_height <$> resolveHighestUp up_point
      treeHighestUp %= Map.insert point up_highest
      pure up_highest

getHeight :: Point -> TreeS Int
getHeight point = uses treeHeights (fromMaybe 0 . Map.lookup point)

-- computeHighestUp :: Point -> TreeS Int
-- computeHighestUp point = 


left :: Point -> Point
left (x, y) = (x - 1, y)

right :: Point -> Point
right (x, y) = (x + 1, y)

up :: Point -> Point
up (x, y) = (x, y + 1)

down :: Point -> Point
down (x, y) = (x, y + 1)