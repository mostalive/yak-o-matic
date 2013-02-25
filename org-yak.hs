module Main(main) where
import System.Environment
import System.IO(readFile)
import Data.GraphViz.Printing(toDot,renderDot)
import Data.Text.Lazy(unpack)

import YakCli

main :: IO ()
main = do
  [file] <- getArgs
  content <- readFile file
  let g = toGraph ".org" (Just content) 
  putStrLn $ (unpack.renderDot .toDot) g 
