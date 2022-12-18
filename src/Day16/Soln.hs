{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Day16.Soln where

import Control.Lens

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.Functor
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

data Valve = Valve {
  valveName :: T.Text,
  valveFlow :: Int,
  valveAdj  :: [T.Text]
} deriving Show

shortFile :: FilePath
shortFile = "src/Day16/short-input.txt"

fullFile :: FilePath
fullFile = "src/Day16/full-input.txt"

soln :: FilePath -> IO ()
soln file = do
  input_lines <- T.lines <$> TIO.readFile file
  let valves = map readValveLine input_lines
  mapM_ print valves

readValveLine :: T.Text -> Valve
readValveLine input = 
  let ws = T.words input
      vname = ws !! 1
      vflow = parseFlow (ws !! 4)
      vadj  = parseAdj (drop 9 ws)
   in Valve vname vflow vadj
  where 
    parseFlow :: T.Text -> Int
    parseFlow = read . T.unpack . T.init . (!! 1) . T.splitOn "="

    parseAdj :: [T.Text] -> [T.Text]
    parseAdj = map (T.filter isLetter)