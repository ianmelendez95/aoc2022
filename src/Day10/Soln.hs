{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Day10.Soln where

import Control.Lens

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.List
import Data.Maybe
import Data.Char

import Data.Set (Set)
import qualified Data.Set as Set

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as Vec

import System.FilePath.Posix

import Control.Monad.State.Lazy

import Debug.Trace

data Instr = AddX Int | Noop deriving Show

type SigS = State [Int]

emptySigE :: [Int]
emptySigE = [1]

shortFile :: FilePath
shortFile = "src/Day10/short-input.txt"

shortFile2 :: FilePath
shortFile2 = "src/Day10/short-input2.txt"

fullFile :: FilePath
fullFile = "src/Day10/full-input.txt"

soln :: FilePath -> IO ()
soln file = do
  content <- TIO.readFile file
  let instr_lines = T.lines content
      instrs = map parseInstr instr_lines
      sigs = reverse $ evalInstrs instrs
      -- samples = collectSigs sigs
      pixels = zipWith spriteVisible [0..] sigs
  -- mapM_ print instrs
  -- mapM_ print (zip [1..] sigs)
  -- mapM_ print samples
  -- putStrLn "[Answer]"
  -- print $ sum (map (uncurry (*)) samples)
  mapM_ print (zip [1..] (zip sigs pixels))
  TIO.putStrLn $ renderPixels pixels

renderPixels :: [Bool] -> T.Text
renderPixels = T.unlines . renderPixelRows

renderPixelRows :: [Bool] -> [T.Text]
renderPixelRows [] = []
renderPixelRows ps = 
  let (row_ps, rest) = splitAt 40 ps
   in T.pack (map renderPixel row_ps) : renderPixelRows rest
  where 
    renderPixel :: Bool -> Char
    renderPixel True = '#'
    renderPixel False = '.'

spriteVisible :: Int -> Int -> Bool
spriteVisible crt_pos sprite_pos = abs ((crt_pos `mod` 40) - sprite_pos) <= 1

collectSigs :: [Int] -> [(Int, Int)]
collectSigs sigs = 
  let (first : rest) = drop 19 (zip [1..] sigs)
   in first : collectRest rest
  where 
    collectRest :: [(Int, Int)] -> [(Int, Int)] 
    collectRest sigs = 
      case drop 39 sigs of 
        [] -> []
        (next : rest) -> next : collectRest rest

evalInstrs :: [Instr] -> [Int]
evalInstrs instrs = execState (mapM_ execInstr instrs) [1]

execInstr :: Instr -> SigS ()
execInstr Noop = do 
  cur_strength <- gets head
  modify (cur_strength :)
execInstr (AddX x) = do 
  cur_strength <- gets head
  modify (cur_strength :)
  modify (cur_strength + x :)

parseInstr :: T.Text -> Instr
parseInstr "noop" = Noop
parseInstr input = 
  let [_, amt] = T.words input
   in AddX (read (T.unpack amt))
  