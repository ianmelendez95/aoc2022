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

type Move = (Dir, Int)
data Dir  = DRight | DLeft | DUp | DDown deriving Show

type Rope  = (Point, Point) -- (head, tail)
type Point = (Int, Int)
      
shortFile :: FilePath
shortFile = "src/Day9/short-input.txt"

fullFile :: FilePath
fullFile = "src/Day9/full-input.txt"

soln :: FilePath -> IO ()
soln file = do
  content <- TIO.readFile file
  let move_lines = T.lines content
      moves = map parseMoveLine move_lines
      ropes = scanl' iterMove ((0, 0), (0, 0)) (expandMoves moves)
      tail_points = map snd ropes
      unique_tail_points = Set.toList (Set.fromList tail_points)
  -- mapM_ print moves
  -- mapM_ print (zip [0..] ropes)
  putStrLn $ "Answer: " <> show (length unique_tail_points)

iterMove :: Rope -> Move -> Rope
iterMove (rhead, rtail) move = 
  let rhead' = movePoint move rhead
      d@(dx, dy) = subPoints rhead' rtail
   in if abs dx <= 1 && abs dy <= 1
        then (rhead', rtail)
        else let dunit = unit d
              in (rhead', addPoints rtail dunit)

unit :: Point -> Point
unit (x, y) = (dimUnit x, dimUnit y)
  where 
    dimUnit xy = 
      case compare xy 0 of 
        LT -> (-1)
        EQ -> 0
        GT -> 1

addPoints :: Point -> Point -> Point
addPoints (x, y) (x', y') = (x + x', y + y')

subPoints :: Point -> Point -> Point
subPoints (x, y) (x', y') = (x - x', y - y')

movePoint :: Move -> Point -> Point
movePoint (DRight, n) (x, y) = (x + n, y)
movePoint (DLeft, n)  (x, y) = (x - n, y)
movePoint (DUp, n)    (x, y) = (x, y + n)
movePoint (DDown, n)  (x, y) = (x, y - n)

expandMoves :: [Move] -> [Move]
expandMoves = concatMap replicateMove
  where 
    replicateMove :: Move -> [Move]
    replicateMove (dir, count) = replicate count (dir, 1)

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
   in (dir, (read (T.unpack count)))
    
