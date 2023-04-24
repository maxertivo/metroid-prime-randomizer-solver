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
checkFile fileContents All = maybe "False" show (checkAllDifficulties fileContents)
checkFile fileContents (Arg difficulty) = show $ isLogCompletable fileContents difficulty

checkAllDifficulties :: String -> Maybe Difficulty
checkAllDifficulties fileContents =
    if isLogCompletable fileContents Expert
        then checkAllDifficulties' fileContents (reverse [Easy .. Expert])
        else Nothing
  where
    checkAllDifficulties' :: String -> [Difficulty] -> Maybe Difficulty
    checkAllDifficulties' logContents [Easy] =
        if isLogCompletable logContents Easy
            then Just Easy
            else Just Medium
    checkAllDifficulties' logContents (diff1:diff2:xs) =
        if isLogCompletable logContents diff2
            then checkAllDifficulties' logContents (diff2 : xs)
            else Just diff1
    checkAllDifficulties' _ _ = Nothing

isLogCompletable :: String -> Difficulty -> Bool
isLogCompletable fileContents diff =
    let roomGraph = buildMap roomId $ buildNodes diff
        roomGraph2 = replaceElevators roomGraph (parseElevators fileContents)
        itemGraph = buildMap itemId $ parse fileContents ++ pseudoItems
     in isCompletable roomGraph2 itemGraph
