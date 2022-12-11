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

import Control.Monad
import Control.Monad.State.Lazy

import Debug.Trace

data Monkey = Monkey Int [Int] Op Test deriving Show
type Items  = Map Int [Int]

data Op     = Op OpType OpVal deriving Show
data OpType = Add | Mult deriving Show
data OpVal  = VInt Int | VOld deriving Show

data Test = Test Int Int Int deriving Show -- divisible true false

type MonS = State MonE

data MonE = MonE {
  _moneItems    :: Map Int [Int],
  _moneActivity :: Map Int Int
}

makeLenses ''MonE

shortFile :: FilePath
shortFile = "src/Day11/short-input.txt"

fullFile :: FilePath
fullFile = "src/Day11/full-input.txt"

soln :: FilePath -> IO ()
soln file = do
  content <- TIO.readFile file
  let input_lines = T.lines content
      monkeys = parseMonkeys input_lines
      monkey_items = monkeyItems monkeys
      monkey_rounds = zip [(1 :: Int)..] . take 20 . tail $ iterate (monkeyRound monkeys) (MonE monkey_items Map.empty)
      (_, MonE _ final_activity) = last monkey_rounds
      most_active_prod = product . take 2 . reverse . sort . map snd $ Map.toList final_activity
  -- mapM_ print monkeys

  -- putStrLn "\n[Initial]"
  -- mapM_ print (Map.toList monkey_items)

  -- mapM_ printRound monkey_rounds

  putStrLn $ "Answer: " <> show most_active_prod
  where 
    printRound :: (Int, MonE) -> IO ()
    printRound (round, MonE items activity) = do
      putStrLn $ "\n[Post Round " ++ show round ++ "]"
      putStrLn "-- Items --"
      mapM_ print (Map.toList items)
      putStrLn "-- Activity --"
      mapM_ print (Map.toList activity)


monkeyRound :: [Monkey] -> MonE -> MonE
monkeyRound monkeys = execState (mapM_ monkeyInspect monkeys)

monkeyItems :: [Monkey] -> Items
monkeyItems = Map.fromList . map (\(Monkey id items _ _) -> (id, items)) 

monkeyInspect :: Monkey -> MonS ()
monkeyInspect (Monkey id _ op (Test test_div test_true test_false)) = do 
  cur_items_res <- uses moneItems (Map.lookup id)
  let (Just items) = cur_items_res
      inspected_items = map inspectItem items
      inspected_count = length inspected_items
  mapM_ (\item -> yeetItem item (testItem item)) inspected_items
  moneItems %= Map.insert id []  -- clear out the monkey after it's turn
  moneActivity %= Map.insertWith (+) id inspected_count
  where
    inspectItem :: Int -> Int
    inspectItem worry_val = evalOp op worry_val `div` 3

    testItem :: Int -> Int
    testItem worry_val = 
      if worry_val `mod` test_div == 0
        then test_true
        else test_false
    
    yeetItem :: Int -> Int -> MonS ()
    yeetItem worry_val dest_monkey =
      moneItems %= Map.adjust (worry_val :) dest_monkey

evalOp :: Op -> Int -> Int
evalOp (Op op_type op_val) worry_val = worry_val `valOp` eval_val
  where 
    valOp :: Int -> Int -> Int
    valOp = 
      case op_type of 
        Mult -> (*)
        Add  -> (+)
    
    eval_val :: Int 
    eval_val = 
      case op_val of 
        VOld -> worry_val
        (VInt v) -> v

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