{-# LANGUAGE OverloadedStrings #-}

module Day5.Soln where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.List
import Data.Maybe
import Data.Char

import Data.Set (Set)
import qualified Data.Set as Set

import Debug.Trace

shortFile :: FilePath
shortFile = "src/Day5/short-input.txt"

fullFile :: FilePath
fullFile = "src/Day5/full-input.txt"

soln :: IO ()
soln = solnForFile shortFile

solnForFile :: FilePath -> IO ()
solnForFile file = do
  content <- TIO.readFile file
  let input_lines = T.lines content
      cubes = map parseCubes input_lines
  putStrLn $ "Cubes: "
  mapM_ print (take 3 cubes)
  putStrLn $ "Answer: "   <> show input_lines

parseCubes :: T.Text -> [Maybe Char]
parseCubes input_line = 
  case T.uncons input_line of 
    Nothing -> []
    Just (c, cs) -> 
      case c of 
        '[' ->
          let Just (cube_letter, rest) = T.uncons cs
           in Just cube_letter : parseCubes (T.drop 2 rest)
        ' ' -> Nothing : parseCubes (T.drop 3 cs)
        _ -> error $ "Unexpected rest of input: " ++ T.unpack input_line
