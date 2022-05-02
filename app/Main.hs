module Main where

import Data.List
import Graph
import Node
import Parser
import Solver
import System.Directory
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    difficultyString <-
        case parseArg args "-d" of
            Just dif -> return dif
            Nothing -> putStrLn "Enter Difficulty ([e]asy, [m]edium, [h]ard, [v]eryHard, e[x]pert):" >> getLine
    directory <-
        case parseArg args "-l" of
            Just dir -> return dir
            Nothing -> putStrLn "Enter Directory or File to check: " >> getLine
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
    let graph2 = replaceElevators graph (parseElevators fileContents)
    return $ isCompletable graph2
