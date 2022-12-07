{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Day7.Soln where

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

type FSState = State FSEnv

data FSEnv = FSEnv {
  _fsenvFS    :: FS,
  _fsenvSizes :: DirSizes
}

data Cmd = CdUp | CdRoot | CdDir String | Ls [LsOut] deriving Show
data LsOut = LsFile String Int | LsDir String deriving Show 

data Dir = Dir Int [String] deriving Show
type FS = Map String Dir
type DirSizes = Map String Int

makeLenses ''FSEnv

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
      fs = replayCmds "/" cmds Map.empty
      sizes = resolveSizes fs
  mapM_ print cmds
  mapM_ print (Map.toList fs)
  putStrLn "\n[Sizes]"
  mapM_ print (Map.toList sizes)

  let answer = sum $ filter (<= 100000) (map snd (Map.toList sizes))
  putStrLn $ "Answer: " <> show answer

resolveSizes :: FS -> DirSizes
resolveSizes fs = 
  let (FSEnv _ sizes) = execState (resolveSize "/") (FSEnv fs Map.empty)
   in sizes

resolveSize :: String -> FSState Int
resolveSize path = do
  existing_size <- uses fsenvSizes (Map.lookup path)
  case existing_size of 
    Just size -> pure size
    Nothing   -> do 
      path_dir <- uses fsenvFS (Map.lookup path)
      let Just (Dir fsize subs) = path_dir
      sub_sizes <- traverse (resolveSize . (path </>)) subs
      let path_size = fsize + sum sub_sizes
      fsenvSizes %= Map.insert path path_size
      pure path_size

replayCmds :: String -> [Cmd] -> FS -> FS
replayCmds _ [] fs = fs
replayCmds cwd (CdUp : cmds) fs         = replayCmds (takeDirectory cwd) cmds fs
replayCmds _   (CdRoot : cmds) fs       = replayCmds "/" cmds fs
replayCmds cwd ((CdDir dir) : cmds) fs  = replayCmds (cwd </> dir) cmds fs
replayCmds cwd ((Ls ls_out) : cmds) fs  = 
  case Map.lookup cwd fs of 
    Just _  -> replayCmds cwd cmds fs -- already accounted for
    Nothing -> replayCmds cwd cmds (Map.insert cwd dir_content fs)
  where 
    dir_content :: Dir
    dir_content = foldr foldLsOut (Dir 0 []) ls_out
    
    foldLsOut :: LsOut -> Dir -> Dir
    foldLsOut (LsDir dir)      (Dir size dirs) = Dir size (dir : dirs) 
    foldLsOut (LsFile _ fsize) (Dir size dirs) = Dir (fsize + size) dirs

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
      | otherwise   = CdDir (T.unpack arg)

parseLsOut :: T.Text -> LsOut
parseLsOut input = 
  case T.splitAt 3 input of 
    ("dir", rest) -> LsDir (T.unpack $ skipSpace rest)
    _ -> parseFileListing input
  where 
    parseFileListing :: T.Text -> LsOut
    parseFileListing ls_out = 
      let (size, name) = T.span isNumber ls_out
       in LsFile (T.unpack $ skipSpace name) (read (T.unpack size))

skipSpace :: T.Text -> T.Text
skipSpace = T.dropWhile (== ' ')
