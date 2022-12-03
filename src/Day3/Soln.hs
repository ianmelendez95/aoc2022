{-# LANGUAGE OverloadedStrings #-}

module Day3.Soln where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.List
import Data.Maybe
import Data.Char

import Data.Set (Set)
import qualified Data.Set as Set

import Debug.Trace

shortFile :: FilePath
shortFile = "src/Day3/short-input.txt"

fullFile :: FilePath
fullFile = "src/Day3/full-input.txt"

soln :: IO ()
soln = solnForFile shortFile

solnForFile :: FilePath -> IO ()
solnForFile file = do
  content <- TIO.readFile file
  let sack_lines = T.lines content
      sack_compartments = map compartments sack_lines
      shared_items = map compartmentsSharedItem sack_compartments

      sack_groups = splitGroups sack_lines
      badges = map groupBadge sack_groups

      result = sum (map itemPriority badges)
  putStrLn $ "Groups: " <> show sack_groups
  putStrLn $ "Badges: " <> show badges
  putStrLn $ "Answer: " <> show result

groupBadge :: [T.Text] -> Char
groupBadge sacks = 
  let sack_items = map (Set.fromList . T.unpack) sacks
   in head . Set.toList $ foldl1' Set.intersection sack_items

splitGroups :: [T.Text] -> [[T.Text]]
splitGroups [] = []
splitGroups sacks = 
  let (group, rest) = splitAt 3 sacks
   in group : splitGroups rest
  
compartmentsSharedItem :: (Set Char, Set Char) -> Char
compartmentsSharedItem (left, right) = head . Set.toList $ Set.intersection left right

compartments :: T.Text -> (Set Char, Set Char)
compartments items = 
  let (left, right) = T.splitAt (T.length items `div` 2) items
   in (Set.fromList (T.unpack left),  Set.fromList (T.unpack right))

itemPriority :: Char -> Int
itemPriority item
  | isLower item = (ord item - ord 'a') + 1
  | otherwise = (ord item - ord 'A') + 27
