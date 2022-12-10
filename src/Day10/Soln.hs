{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Day10.Soln where

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

data Instr = Addx Int | Noop deriving Show

shortFile :: FilePath
shortFile = "src/Day10/short-input.txt"

fullFile :: FilePath
fullFile = "src/Day10/full-input.txt"

soln :: FilePath -> IO ()
soln file = do
  content <- TIO.readFile file
  let instr_lines = T.lines content
      instrs = map parseInstr instr_lines
  mapM_ print instrs

parseInstr :: T.Text -> Instr
parseInstr "noop" = Noop
parseInstr input = 
  let [_, amt] = T.words input
   in Addx (read (T.unpack amt))