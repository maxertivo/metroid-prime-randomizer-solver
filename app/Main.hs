module Main where

import Parser
import System.IO  
import Control.Monad
import Node
import Graph

main :: IO ()
main = do
  fileContents <- readFile "resources/sample.txt"
  print (parse fileContents)
