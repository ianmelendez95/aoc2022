module Day1.Soln where 

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

shortFile :: FilePath
shortFile = "src/Day1/short-input.txt"

soln :: IO ()
soln = do 
  content <- TIO.readFile shortFile
  let cal_lines = T.lines content
  mapM_ TIO.putStrLn cal_lines
