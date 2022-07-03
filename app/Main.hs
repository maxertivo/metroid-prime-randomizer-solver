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
            Nothing -> putStrLn "Enter Difficulty ([e]asy, [m]edium, [h]ard, [v]eryHard, e[x]pert, [a]ll):" >> getLine
    directory <-
        case parseArg args "-l" of
            Just dir -> return dir
            Nothing -> putStrLn "Enter Directory or File to check: " >> getLine
    let difficulty = parseDifficulty difficultyString
    if ".txt" `isSuffixOf` directory
        then do
            fileContents <- readFile directory
            putStrLn $ checkFile fileContents difficulty
        else do
            filePaths <- getDirectoryContents directory
            checkAllFiles directory filePaths difficulty

checkAllFiles :: String -> [String] -> DifficultyArg -> IO ()
checkAllFiles _ [] _ = return ()
checkAllFiles directory ("..":rest) diff = checkAllFiles directory rest diff
checkAllFiles directory (".":rest) diff = checkAllFiles directory rest diff
checkAllFiles directory (fileName:rest) diff = do
    fileContents <- readFile (directory ++ "/" ++ fileName)
    putStr $ fileName ++ ": "
    putStrLn $ checkFile fileContents diff
    checkAllFiles directory rest diff

checkFile :: String -> DifficultyArg -> String
checkFile fileContents All = checkAllDifficulties fileContents (reverse [Easy .. Expert])
checkFile fileContents (Arg difficulty) = show $ isLogCompletable fileContents difficulty

checkAllDifficulties :: String -> [Difficulty] -> String
checkAllDifficulties _ [] = "False"
checkAllDifficulties fileContents [diff] = if isLogCompletable fileContents diff then "Easy" else "Medium"
checkAllDifficulties fileContents (Expert:xs) = if isLogCompletable fileContents Expert then checkAllDifficulties fileContents xs else "False"
checkAllDifficulties fileContents (diff1:diff2:xs) = if isLogCompletable fileContents diff2 then checkAllDifficulties fileContents (diff2 : xs) else show diff1

isLogCompletable :: String -> Difficulty -> Bool
isLogCompletable fileContents diff =
    let graph = buildMap $ buildNodes diff ++ parse fileContents
        graph2 = replaceElevators graph (parseElevators fileContents)
    in  isCompletable graph2

