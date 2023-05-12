module Main where

import Data.Text (Text)
import Data.Text.IO (readFile)
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
            fileContents <- Data.Text.IO.readFile directory
            putStrLn $ checkFile fileContents difficulty
        else do
            filePaths <- getDirectoryContents directory
            checkAllFiles directory filePaths difficulty

checkAllFiles :: String -> [String] -> DifficultyArg -> IO ()
checkAllFiles _ [] _ = return ()
checkAllFiles directory ("..":rest) diff = checkAllFiles directory rest diff
checkAllFiles directory (".":rest) diff = checkAllFiles directory rest diff
checkAllFiles directory (fileName:rest) diff = do
    fileContents <- Data.Text.IO.readFile (directory ++ "/" ++ fileName)
    putStr $ fileName ++ ": "
    putStrLn $ checkFile fileContents diff
    checkAllFiles directory rest diff

checkFile :: Text -> DifficultyArg -> String
checkFile fileContents All = maybe "False" show (checkAllDifficulties fileContents Expert)
checkFile fileContents (Arg difficulty) = show $ isLogCompletable fileContents difficulty

checkAllDifficulties :: Text -> Difficulty -> Maybe Difficulty
checkAllDifficulties fileContents diff =
    case diff of 
        Expert -> if isLogCompletable fileContents Expert then checkAllDifficulties fileContents VeryHard else Nothing
        VeryHard -> if isLogCompletable fileContents VeryHard then checkAllDifficulties fileContents Hard else Just Expert
        Hard -> if isLogCompletable fileContents Hard then checkAllDifficulties fileContents Medium else Just VeryHard
        Medium -> if isLogCompletable fileContents Medium then checkAllDifficulties fileContents Easy else Just Hard
        Easy -> if isLogCompletable fileContents Easy then Just Easy else Just Medium

isLogCompletable :: Text -> Difficulty -> Bool
isLogCompletable fileContents diff =
    let roomMap = buildRoomMap $! buildNodes diff
        roomMap2 = replaceElevators roomMap $! parseElevators fileContents
        itemMap = buildItemMap $! parse fileContents ++ pseudoItems
     in isCompletable roomMap2 itemMap
