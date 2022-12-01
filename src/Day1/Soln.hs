module Day1.Soln where 

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

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
      max_cal = findMaxElfCal cal_lines
  putStrLn $ "Max: " ++ show max_cal

findMaxElfCal :: [T.Text] -> Int
findMaxElfCal = maximum . readElfCalStrs . map parseCal

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
