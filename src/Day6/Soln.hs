{-# LANGUAGE OverloadedStrings #-}

module Day6.Soln where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.List
import Data.Maybe
import Data.Char

import Data.Set (Set)
import qualified Data.Set as Set

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as Vec

import Debug.Trace

shortFile :: FilePath
shortFile = "src/Day6/short-input.txt"

fullFile :: FilePath
fullFile = "src/Day6/full-input.txt"

soln :: IO ()
soln = solnForFile shortFile

solnForFile :: FilePath -> IO ()
solnForFile file = do
  content <- TIO.readFile file
  let unique_i = findUnique 0 (T.unpack content)
  putStrLn $ "Answer: " <> show unique_i

findUnique :: Int -> [Char] -> Int
findUnique i cs = 
  if Set.size (Set.fromList (take 4 cs)) < 4
    then findUnique (i + 1) (drop 1 cs)
    else i + 4