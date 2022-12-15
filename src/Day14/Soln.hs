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

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Debug.Trace

type Point = (Int, Int)
type Seg = (Point, Point)

shortFile :: FilePath
shortFile = "src/Day14/short-input.txt"

fullFile :: FilePath
fullFile = "src/Day14/full-input.txt"

soln :: FilePath -> IO ()
soln file = do
  content <- TIO.readFile file
  let input_lines = T.lines content
      segments = concatMap parseSegmentsLine input_lines
  mapM_ print segments

parseSegmentsLine :: T.Text -> [Seg]
parseSegmentsLine sline = 
  let points = map parsePoint $ T.splitOn " -> " sline
   in takeSegments points
  where 
    parsePoint :: T.Text -> Point
    parsePoint point_txt = 
      let [m, n] = map (read . T.unpack) $ T.splitOn "," point_txt
       in (m, n)

    takeSegments :: [Point] -> [Seg]
    takeSegments (p1:p2:ps) = (p1,p2) : takeSegments (p2:ps)
    takeSegments _ = []