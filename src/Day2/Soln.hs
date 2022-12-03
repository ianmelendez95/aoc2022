{-# LANGUAGE OverloadedStrings #-}


module Day2.Soln where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.List
import Data.Maybe

import Debug.Trace

data Shape   = Rock | Paper | Scissors deriving (Show, Eq)
data Outcome = Win  | Lose  | Draw     deriving (Show, Eq)

shortFile :: FilePath
shortFile = "src/Day2/short-input.txt"

fullFile :: FilePath
fullFile = "src/Day2/full-input.txt"

soln :: IO ()
soln = solnForFile fullFile

solnForFile :: FilePath -> IO ()
solnForFile file = do
  content <- TIO.readFile file
  let round_lines = T.lines content
      rounds = map parseRoundLine round_lines
      points = sum $ map (resultPoints . roundResult . parseRoundLine) round_lines
  print (length rounds)
  putStrLn $ "Points:  " ++ show points

resultPoints :: (Outcome, Shape) -> Int
resultPoints (outcome, shape) = outcomePoints outcome + shapePoints shape
  where
    outcomePoints :: Outcome -> Int
    outcomePoints Win  = 6
    outcomePoints Draw = 3
    outcomePoints Lose = 0

    shapePoints :: Shape -> Int
    shapePoints Rock     = 1
    shapePoints Paper    = 2
    shapePoints Scissors = 3

roundResult :: (Shape, Outcome) -> (Outcome, Shape)
roundResult (their_shape, outcome) = (outcome, my_shape)
  where
    my_shape :: Shape
    my_shape =
      if outcome == Draw
        then their_shape
        else case their_shape of 
              Rock     -> if outcome == Win then Paper    else Scissors
              Paper    -> if outcome == Win then Scissors else Rock
              Scissors -> if outcome == Win then Rock     else Paper

    -- round_outcome :: Outcome
    -- round_outcome =
    --   if their_shape == my_shape
    --     then Draw
    --     else case their_shape of 
    --           Rock     -> if my_shape == Paper    then Win else Lose
    --           Paper    -> if my_shape == Scissors then Win else Lose
    --           Scissors -> if my_shape == Rock     then Win else Lose

parseRoundLine :: T.Text -> (Shape, Outcome)
parseRoundLine line = 
  let [their_txt, my_txt] = T.words line
   in (parseTheirShape their_txt, parseOutcome my_txt)
  where
    parseTheirShape :: T.Text -> Shape
    parseTheirShape txt
      | "A" == txt = Rock
      | "B" == txt = Paper
      | "C" == txt = Scissors
      | otherwise = error $ "Unknown shape: " <> T.unpack txt

    parseOutcome :: T.Text -> Outcome
    parseOutcome txt
      | "X" == txt = Lose
      | "Y" == txt = Draw
      | "Z" == txt = Win
      | otherwise = error $ "Unknown outcome: " <> T.unpack txt

    -- parseMyShape :: T.Text -> Shape
    -- parseMyShape txt
    --   | "X" == txt = Rock
    --   | "Y" == txt = Paper
    --   | "Z" == txt = Scissors
    --   | otherwise = error $ "Unknown shape: " <> T.unpack txt

