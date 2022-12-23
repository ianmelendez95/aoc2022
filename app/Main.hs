module Main (main) where

import Day16.Soln

import System.Environment

main :: IO ()
main = do
  [input_type] <- getArgs 
  case input_type of 
    "short" -> Day16.Soln.soln Day16.Soln.shortFile
    "full"  -> Day16.Soln.soln Day16.Soln.fullFile
