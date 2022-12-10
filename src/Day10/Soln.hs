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

data Instr = AddX Int | Noop deriving Show

type SigS = State [Int]

emptySigE :: [Int]
emptySigE = [1]

shortFile :: FilePath
shortFile = "src/Day10/short-input.txt"

shortFile2 :: FilePath
shortFile2 = "src/Day10/short-input2.txt"

fullFile :: FilePath
fullFile = "src/Day10/full-input.txt"

soln :: FilePath -> IO ()
soln file = do
  content <- TIO.readFile file
  let instr_lines = T.lines content
      instrs = map parseInstr instr_lines
      sigs = evalInstrs instrs
  mapM_ print instrs
  mapM_ print (zip [1..] (reverse sigs))

evalInstrs :: [Instr] -> [Int]
evalInstrs instrs = execState (mapM_ execInstr instrs) [1]

execInstr :: Instr -> SigS ()
execInstr Noop = do 
  cur_strength <- gets head
  modify (cur_strength :)
execInstr (AddX x) = do 
  cur_strength <- gets head
  modify (cur_strength :)
  modify (cur_strength + x :)

parseInstr :: T.Text -> Instr
parseInstr "noop" = Noop
parseInstr input = 
  let [_, amt] = T.words input
   in AddX (read (T.unpack amt))
  