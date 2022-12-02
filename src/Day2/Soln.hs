{-# LANGUAGE OverloadedStrings #-}


module Day2.Soln where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.List
import Data.Maybe

data Shape   = Rock | Paper | Scissors deriving Show
data Outcome = Win  | Lose  | Draw     deriving Show

shortFile :: FilePath
shortFile = "src/Day2/short-input.txt"

fullFile :: FilePath
fullFile = "src/Day2/full-input.txt"

soln :: IO ()
soln = solnForFile shortFile

solnForFile :: FilePath -> IO ()
solnForFile file = do
  content <- TIO.readFile file
  let round_lines = T.lines content
      rounds = map parseRoundLine round_lines
  putStrLn $ "Rounds:  " ++ show rounds

parseRoundLine :: T.Text -> (Shape, Shape)
parseRoundLine line = 
  let [their_txt, my_txt] = T.words line
   in (parseTheirShape their_txt, parseMyShape my_txt)

parseTheirShape :: T.Text -> Shape
parseTheirShape txt
  | "A" == txt = Rock
  | "B" == txt = Paper
  | "C" == txt = Scissors
  | otherwise = error $ "Unknown shape: " <> T.unpack txt

parseMyShape :: T.Text -> Shape
parseMyShape txt
  | "X" == txt = Rock
  | "Y" == txt = Paper
  | "Z" == txt = Scissors
  | otherwise = error $ "Unknown shape: " <> T.unpack txt

