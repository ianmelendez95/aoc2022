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
      (cubes, instrs) = parseInput input_lines
  putStrLn "Cubes: "
  mapM_ print cubes

  putStrLn "\nInstructions"
  mapM_ print instrs

data Instr = Instr {
  instrCount :: Int, 
  instrFrom  :: Int, 
  instrTo    :: Int
} deriving Show

parseInput :: [T.Text] -> ([[Char]], [Instr])
parseInput input_lines = 
  let (cube_lines, rest) = span (\l -> "[" `T.isInfixOf` l) input_lines
      instr_lines = drop 2 rest

      input_cubes = map parseCubes cube_lines
   in (inputCubesToCols input_cubes, map parseInstr instr_lines)

inputCubesToCols :: [[Maybe Char]] -> [[Char]]
inputCubesToCols input_cubes = 
  foldr (zipWith consCube) (replicate (length (head input_cubes)) []) input_cubes
  where 
    consCube :: Maybe Char -> [Char] -> [Char]
    consCube Nothing col = col
    consCube (Just c) col = c : col
  
parseInstr :: T.Text -> Instr
parseInstr line = 
  let [_, count, _, from, _, to] = map (read . T.unpack) $ T.words line
   in Instr count from to

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
