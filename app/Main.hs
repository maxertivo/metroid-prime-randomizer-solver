module Main where

import Data.List
import Graph
import Node
import Parser
import Solver
import System.Directory

main :: IO ()
main = do
    putStrLn "Enter Directory or File to check: "
    directory <- getLine
    putStrLn "Enter Difficulty ([e]asy, [m]edium, [h]ard, [v]eryHard, e[x]pert):"
    difficultyString <- getLine
    let difficulty = parseDifficulty difficultyString
    if ".txt" `isSuffixOf` directory
        then do
            checkFile directory difficulty >>= print
        else do
            filePaths <- getDirectoryContents directory
            checkAllFiles directory filePaths difficulty

checkAllFiles :: String -> [String] -> Difficulty -> IO ()
checkAllFiles _ [] _ = return ()
checkAllFiles directory ("..":rest) diff = checkAllFiles directory rest diff
checkAllFiles directory (".":rest) diff = checkAllFiles directory rest diff
checkAllFiles directory (fileName:rest) diff = do
    putStr $ fileName ++ ": "
    checkFile (directory ++ "/" ++ fileName) diff >>= print
    checkAllFiles directory rest diff

checkFile :: String -> Difficulty -> IO Bool
checkFile filePath diff = do
    fileContents <- readFile filePath
    let graph = buildMap $ buildNodes diff ++ parse fileContents
    return $ isCompletable graph
