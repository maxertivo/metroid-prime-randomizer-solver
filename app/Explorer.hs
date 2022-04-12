module Main where

import Parser
import System.IO  
import Control.Monad
import Node
import Graph
import Data.Maybe
import Control.Exception
import System.Console.ANSI

data GraphException = MissingWarp !Id | MissingNode | InvalidArgument !String
                    deriving (Show)

instance Exception GraphException

main :: IO ()
main = do
    fileContents <- readFile "resources/sample.txt"
    let nodes = buildNodes ++ parse fileContents
    case getNode nodes (R RHiveTotem) of
        Just node -> explore nodes [] node
        Nothing -> return ()
    
  

explore :: [Node] -> [ItemName] -> Node -> IO ()
explore nodes items (Item id name warp) =  case getNode nodes (R warp) of 
                            Just warpNode -> explore nodes (name : items) warpNode
                            Nothing -> throw $ MissingWarp $ R warp
explore nodes items (Room roomId edges) = do
    let predicates = map canUse edges
        bools = eval predicates items
        ids = map nodeId edges
    
    putStrLn  "---------------------------------------"
    putStrLn  $ "Current Room: " ++ show roomId
    putStrLn  $ "Items: " ++ show items
    printEdges ids bools
    command <- getLine

    let index = (read command :: Integer) - 1
        allowed = fromMaybe False (getIndex bools index)
        newNode = getIndex ids index >>= getNode nodes

    if allowed 
    then case newNode of
        Nothing -> throw MissingNode
        Just a -> explore nodes items a
    else explore nodes items (Room roomId edges)


getNode :: [Node] -> Id ->  Maybe Node
getNode ((Item item name warp):rest) (I id) = if id == item then return (Item item name warp) else getNode rest (I id)
getNode ((Room room edges):rest) (R id) = if id == room then return (Room room edges) else getNode rest (R id)
getNode [] _ = Nothing
getNode (elem:rest) x = getNode rest x

getIndex :: [a] -> Integer -> Maybe a
getIndex [] i = Nothing
getIndex (x:rest) 0 = Just x
getIndex (x:rest) i = getIndex rest (i-1) 

eval :: [[ItemName] -> Bool] -> [ItemName] -> [Bool]
eval predicates items = map (\ x -> x items) predicates

printEdges :: [Id] -> [Bool] -> IO ()
printEdges  = printEdgesHelper 1

printEdgesHelper :: Integer -> [Id] -> [Bool] -> IO ()
printEdgesHelper count (id:ids) (bool:bools) = do
    if bool then setSGR [SetColor Foreground Vivid Green] else setSGR [SetColor Foreground Vivid Red]
    putStrLn $ show count ++ ". " ++ show id ++ " - " ++ show bool
    setSGR [Reset]
    printEdgesHelper (count+1) ids bools
printEdgesHelper _ _ _ = return ()
