{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Day13.Soln where

import Control.Lens

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.Void
import Data.List
import Data.Maybe
import Data.Char
import Data.Ord

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

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Debug.Trace

type Parser = Parsec Void T.Text

data Packet = PList [Packet] | PInt Int deriving Show

shortFile :: FilePath
shortFile = "src/Day13/short-input.txt"

fullFile :: FilePath
fullFile = "src/Day13/full-input.txt"

soln :: FilePath -> IO ()
soln file = do
  content <- TIO.readFile file
  let input_lines = T.lines content
      packet_pairs = zipWith (\i pair -> (i, uncurry packetsOrder pair, pair)) [1..] $ parsePacketLines input_lines
      ordered_pairs = filter (\(_, order, _) -> order == LT || order == EQ) packet_pairs
      ordered_indexes = map (\(i, _, _) -> i) ordered_pairs

      -- equal_pairs = filter (\(_, order, _) -> order == EQ) packet_pairs
  -- mapM_ print packet_pairs
  -- mapM_ print equal_pairs
  putStrLn $ "Answer: " <> show (sum ordered_indexes)

packetsOrder :: Packet -> Packet -> Ordering
packetsOrder (PInt p1)    (PInt p2)   = compare p1 p2
packetsOrder (PList p1)   (PList p2)  = listOrder p1   p2
packetsOrder p1@(PInt _)  (PList p2)  = listOrder [p1] p2
packetsOrder (PList p1)   p2@(PInt _) = listOrder p1   [p2]

listOrder :: [Packet] -> [Packet] -> Ordering
listOrder [] [] = EQ
listOrder [] _  = LT
listOrder _ []  = GT
listOrder (p1:ps1) (p2:ps2) = 
  case packetsOrder p1 p2 of 
    EQ    -> listOrder ps1 ps2
    order -> order

parsePacketLines :: [T.Text] -> [(Packet, Packet)]
parsePacketLines [] = []
parsePacketLines plines = 
  let ([left, right], rest) = splitAt 2 plines
   in (parsePacketLine left, parsePacketLine right) : parsePacketLines (drop 1 rest)

parsePacketLine :: T.Text -> Packet
parsePacketLine input = 
  case parse packetParser "" input of
    Left bundle  -> error (errorBundlePretty bundle)
    Right packet -> packet

packetParser :: Parser Packet
packetParser = choice 
  [ PList <$> between (char '[') (char ']') (sepBy packetParser (char ','))
  , PInt  <$> L.decimal
  ]
