{-# LANGUAGE OverloadedStrings #-}

module Day5.Soln where

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
  putStrLn "Cols:"
  mapM_ print cubes
  putStrLn ""

  cols <- vecFromList cubes

  mapM_ (`iterInstr` cols) instrs 

  tops <- getTopCrates cols

  putStrLn $ "Answer: " <> show tops

data Instr = Instr {
  instrCount :: Int, 
  instrFrom  :: Int, 
  instrTo    :: Int
} deriving Show

getTopCrates :: IOVector [Char] -> IO [Char]
getTopCrates = Vec.foldr consHead []
  where 
    consHead :: [Char] -> [Char] -> [Char]
    consHead [] cs = cs
    consHead (c:_) cs = c : cs

iterInstr :: Instr -> IOVector [Char] -> IO ()
iterInstr instr cols = do
  execInstr instr cols
  putStrLn $ "Instr: " <> show instr
  putStrLn "Cols: "
  Vec.mapM_ print cols
  putStrLn ""

execInstr :: Instr -> IOVector [Char] -> IO ()
execInstr (Instr count from to) cols = do 
  from_col <- Vec.read cols from_i
  to_col   <- Vec.read cols to_i
  let (from_cubes, from_rest) = splitAt count from_col
  Vec.write cols from_i from_rest
  Vec.write cols to_i   (reverse from_cubes <> to_col)
  where 
    from_i = from - 1
    to_i = to - 1

vecFromList :: [[Char]] -> IO (IOVector [Char])
vecFromList list = Vec.generate (length list) (list !!)

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
