{-# LANGUAGE OverloadedStrings #-}

module Day4.Soln where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.List
import Data.Maybe
import Data.Char

import Data.Set (Set)
import qualified Data.Set as Set

import Debug.Trace

shortFile :: FilePath
shortFile = "src/Day4/short-input.txt"

fullFile :: FilePath
fullFile = "src/Day4/full-input.txt"

soln :: IO ()
soln = solnForFile shortFile

solnForFile :: FilePath -> IO ()
solnForFile file = do
  content <- TIO.readFile file
  let range_lines = T.lines content
      range_pairs = map parseRangePairs range_lines
      answer = length (filter rangePairsOverlap range_pairs)
  putStrLn "Pairs: "
  mapM_ (\p -> putStrLn $ showPair p <> ": " <> show (rangePairsOverlap p)) range_pairs 
  putStrLn $ "Answer: "   <> show answer

type Range = (Int, Int)

showPair :: (Range, Range) -> String
showPair (left, right) = showRange left <> "," <> showRange right
  where 
    showRange :: Range -> String
    showRange (s,e) = show s <> "-" <> show e

rangePairsOverlap :: (Range, Range) -> Bool
rangePairsOverlap ((start1, end1), (start2, end2))   
  | start1 == start2 = True  -- start at the same spot, always overlap
  | end1   == end2   = True  -- end at the same spot, also always overlap (overcooked, undercooked)
  | start1 < start2  = end1   >= start2
  | otherwise        = start1 <= end2

rangePairsContained :: (Range, Range) -> Bool
rangePairsContained ((start1, end1), (start2, end2))   
  | start1 == start2 = True  -- start at the same spot, always contained
  | end1   == end2   = True  -- end at the same spot, also always contained (overcooked, undercooked)
  | start1 < start2  = end1 > end2
  | otherwise        = end1 < end2

parseRangePairs :: T.Text -> (Range, Range)
parseRangePairs line = 
  let [left, right] = map parseRange (T.splitOn "," line)
   in (left, right)
  where 
    parseRange :: T.Text -> Range
    parseRange input = 
      let [start, end] = map (read . T.unpack) $ T.splitOn "-" input
       in (start, end)
