module Main (main) where

import Day15.Soln

import System.Environment

main :: IO ()
main = do
  [input_type] <- getArgs 
  case input_type of 
    "short" -> Day15.Soln.soln Day15.Soln.shortFile
    "full"  -> Day15.Soln.soln Day15.Soln.fullFile
