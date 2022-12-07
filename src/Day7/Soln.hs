{-# LANGUAGE OverloadedStrings #-}

module Day7.Soln where

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
shortFile = "src/Day7/short-input.txt"

fullFile :: FilePath
fullFile = "src/Day7/full-input.txt"

soln :: IO ()
soln = solnForFile shortFile

solnForFile :: FilePath -> IO ()
solnForFile file = do
  content <- TIO.readFile file
  let term_lines = T.lines content
      cmds = parseTermLines term_lines
  mapM_ print cmds
  putStrLn $ "Answer: "

data Cmd = CdUp | CdRoot | CdDir T.Text | Ls [LsOut] deriving Show
data LsOut = LsFile T.Text Int | LsDir T.Text deriving Show 

parseTermLines :: [T.Text] -> [Cmd]
parseTermLines [] = []
parseTermLines (line:lines) 
  | "$ cd" `T.isPrefixOf` line = parseCdArg (T.drop 5 line) : parseTermLines lines
  | otherwise = 
    let (ls_out, rest) = span (\l -> not ("$" `T.isPrefixOf` l)) lines
     in Ls (map parseLsOut ls_out) : parseTermLines rest
  where
    parseCdArg :: T.Text -> Cmd
    parseCdArg arg
      | arg == "/"  = CdRoot
      | arg == ".." = CdUp
      | otherwise   = CdDir arg

parseLsOut :: T.Text -> LsOut
parseLsOut input = 
  case T.splitAt 3 input of 
    ("dir", rest) -> LsDir (skipSpace rest)
    _ -> parseFileListing input
  where 
    parseFileListing :: T.Text -> LsOut
    parseFileListing ls_out = 
      let (size, name) = T.span isNumber ls_out
       in LsFile (skipSpace name) (read (T.unpack size))

skipSpace :: T.Text -> T.Text
skipSpace = T.dropWhile (== ' ')
