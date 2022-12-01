module Day1.Soln where 

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.List
import Data.Maybe

shortFile :: FilePath
shortFile = "src/Day1/short-input.txt"

fullFile :: FilePath
fullFile = "src/Day1/full-input.txt"

soln :: IO ()
soln = solnForFile fullFile

solnForFile :: FilePath -> IO ()
solnForFile file = do
  content <- TIO.readFile file
  let cal_lines = T.lines content
      max_cal = findMaxElfCals cal_lines
  putStrLn $ "Max 3:   " ++ show max_cal
  putStrLn $ "Max Sum: " ++ show (sum max_cal)

findMaxElfCals :: [T.Text] -> [Int]
findMaxElfCals = maximum3 . readElfCalStrs . map parseCal

maximum3 :: [Int] -> [Int]
maximum3 xs = 
  let sorted = sort xs
   in take 3 (reverse sorted)

readElfCalStrs :: [Maybe Int] -> [Int] 
readElfCalStrs cals = 
  let skip_empty = dropWhile isNothing cals
   in case skip_empty of 
        [] -> []
        cs -> case span isJust cs of
                ([], rest) -> error $ "Should have skipped Nothing" ++ show rest
                (elf_cals, rest) -> sum (catMaybes elf_cals) : readElfCalStrs rest

parseCal :: T.Text -> Maybe Int
parseCal txt = 
  if T.null txt 
    then Nothing 
    else Just (read (T.unpack txt))
