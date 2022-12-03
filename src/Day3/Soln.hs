
{-# LANGUAGE OverloadedStrings #-}

module Day3.Soln where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.List
import Data.Maybe
import Data.Char

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
  undefined

itemPriority :: Char -> Int
itemPriority item
  | isLower item = (ord item - ord 'a') + 1
  | otherwise = (ord item - ord 'A') + 27
