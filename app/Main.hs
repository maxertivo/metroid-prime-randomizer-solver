module Main where

import Parser
import State
import Node
import Graph
import System.Directory

main :: IO ()
main = do 
    putStrLn "Enter Directory to check: "
    directory <- getLine
    filePaths <- getDirectoryContents directory
    main' directory filePaths

main' :: String -> [String] -> IO ()
main' _ [] = return ()
main' directory ("..":rest) = main' directory rest
main' directory (".":rest) = main' directory rest
main' directory (fileName:rest) = do
    fileContents <- readFile (directory ++ "/" ++ fileName)
    putStr fileName ++ ": "
    let graph = buildMap $ buildNodes ++ parse fileContents
    print (isCompletable graph)
    main' directory rest
