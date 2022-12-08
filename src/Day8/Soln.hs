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
      
type Point = (Int, Int)

parseTreeLines :: [T.Text] -> Map Point Int
parseTreeLines input = 
  foldr Map.union Map.empty (zipWith parseRow [0..] input)
  where 
    parseRow :: Int -> T.Text -> Map Point Int
    parseRow row line = 
      let heights = map digitToInt (T.unpack line)
       in Map.fromList $ zipWith (\col h -> ((row, col), h)) [0..] heights
  