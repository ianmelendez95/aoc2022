{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Day11.Soln where

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

data Monkey = Monkey Int [Int] Op Test deriving Show

data Op     = Op OpType OpVal deriving Show
data OpType = Add | Mult deriving Show
data OpVal  = VInt Int | VOld deriving Show

data Test = Test Int Int Int deriving Show -- divisible true false

shortFile :: FilePath
shortFile = "src/Day11/short-input.txt"

fullFile :: FilePath
fullFile = "src/Day11/full-input.txt"

soln :: FilePath -> IO ()
soln file = do
  content <- TIO.readFile file
  let input_lines = T.lines content
      monkeys = parseMonkeys input_lines
  mapM_ print monkeys

parseMonkeys :: [T.Text] -> [Monkey]
parseMonkeys [] = []
parseMonkeys lines = 
  let (monkey_lines, rest) = splitAt 6 lines
   in parseMonkey monkey_lines : parseMonkeys (drop 1 rest)

parseMonkey :: [T.Text] -> Monkey
parseMonkey [num, items, operation, test_div_txt, test_true_txt, test_false_txt] = 
  Monkey num_val item_vals operation_val (Test test_div test_true test_false)
  where 
    num_val :: Int
    num_val = read . T.unpack . T.takeWhile isDigit . T.drop (T.length "Monkey ") $ num

    item_vals :: [Int]
    item_vals = map (read . T.unpack) . T.splitOn ", " . T.drop (T.length "  Starting items: ") $ items

    operation_val :: Op
    operation_val = 
      let [op_type_txt, op_val_txt] = T.words . T.drop (T.length "  Operation: new = old ") $ operation
          op_type = 
            case op_type_txt of 
              "*" -> Mult
              "+" -> Add
              _ -> error ""
          op_val = 
            case op_val_txt of 
              "old" -> VOld
              _ -> VInt (read . T.unpack $ op_val_txt)
       in Op op_type op_val
    
    test_div :: Int
    test_div = read . T.unpack . T.drop (T.length "  Test: divisible by ") $ test_div_txt

    test_true :: Int
    test_true = read . T.unpack . T.drop (T.length "    If true: throw to monkey ") $ test_true_txt

    test_false :: Int
    test_false = read . T.unpack . T.drop (T.length "    If false: throw to monkey ") $ test_false_txt
parseMonkey _ = error ""