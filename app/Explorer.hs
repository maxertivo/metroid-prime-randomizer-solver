module Main where

import Parser
import Util
import System.IO  
import Control.Monad
import Node
import Graph
import Data.Maybe
import Control.Exception
import System.Console.ANSI
import Data.Map (Map)
import qualified Data.Map as Map

data GraphException = MissingWarp !Id | MissingNode | InvalidArgument !String
                    deriving (Show)

instance Exception GraphException

main :: IO ()
main = do
    fileContents <- readFile "resources/sample.txt"
    let graph = buildMap $ buildNodes Extreme ++ parse fileContents
    case Map.lookup (R RHiveTotem) graph of
        Just node -> explore graph Map.empty node
        Nothing -> return ()
    
  

explore :: Map Id Node -> Map ItemName Int -> Node -> IO ()
explore nodes items (Item id name warp) =  case Map.lookup (R warp) nodes of 
                            Just warpNode -> explore nodes (addItem name items) warpNode
                            Nothing -> throw $ MissingWarp $ R warp
explore nodes items (Room roomId edges) = do
    let predicates = map canUse edges
        bools = eval predicates items
        ids = map nodeId edges
    
    putStrLn  "---------------------------------------"
    putStrLn  $ "Current Room: " ++ show roomId
    putStrLn  $ "Items: " ++ show (Map.assocs items)
    printEdges ids bools
    command <- getLine

    let index = (read command :: Integer) - 1
        allowed = fromMaybe False (getIndex bools index)
        newNode = getIndex ids index >>= (`Map.lookup` nodes)

    if allowed 
    then case newNode of
        Nothing -> throw MissingNode
        Just a -> explore nodes items a
    else explore nodes items (Room roomId edges)

printEdges :: [Id] -> [Bool] -> IO ()
printEdges  = printEdgesHelper 1

printEdgesHelper :: Integer -> [Id] -> [Bool] -> IO ()
printEdgesHelper count (id:ids) (bool:bools) = do
    if bool then setSGR [SetColor Foreground Vivid Green] else setSGR [SetColor Foreground Vivid Red]
    putStrLn $ show count ++ ". " ++ show id ++ " - " ++ show bool
    setSGR [Reset]
    printEdgesHelper (count+1) ids bools
printEdgesHelper _ _ _ = return ()
