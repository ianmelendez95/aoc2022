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

shortFile :: FilePath
shortFile = "src/Day12/short-input.txt"

fullFile :: FilePath
fullFile = "src/Day12/full-input.txt"

soln :: FilePath -> IO ()
soln file = do
  content <- TIO.readFile file
  let input_lines = T.lines content
      (height_map, start, end) = parseMap input_lines
  print start
  print end
  mapM_ print (Map.toList height_map)

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