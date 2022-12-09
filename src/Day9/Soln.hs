{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Day9.Soln where

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

data Move = Move Dir Int deriving Show
data Dir = DRight | DLeft | DUp | DDown deriving Show
      
shortFile :: FilePath
shortFile = "src/Day9/short-input.txt"

fullFile :: FilePath
fullFile = "src/Day9/full-input.txt"

soln :: FilePath -> IO ()
soln file = do
  content <- TIO.readFile file
  let move_lines = T.lines content
      moves = map parseMoveLine move_lines
  mapM_ print moves

parseMoveLine :: T.Text -> Move
parseMoveLine line = 
  let [dir_str, count] = T.words line
      dir = 
        case dir_str of 
          "R" -> DRight
          "L" -> DLeft
          "D" -> DDown
          "U" -> DUp
          _ -> error ""
   in Move dir (read (T.unpack count))
    
