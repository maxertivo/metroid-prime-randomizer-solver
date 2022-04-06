module Main where

import Parser
import System.IO  
import Control.Monad
import Node
import Graph
import Data.Maybe
import Control.Exception

data GraphException = MissingWarp !Id | MissingNode
                    deriving (Show)

instance Exception GraphException

main :: IO ()
main = do
    case getNode buildNodes (R RHiveTotem) of
        Just node -> explore buildNodes [] node
        Nothing -> return ()
    
  

explore :: [Node] -> [ItemName] -> Node -> IO ()
explore nodes items node = do
    let edgeList = edges node
    let predicates = map canUse edgeList
    let ids = map nodeId edgeList
    let bools = eval predicates items
    
    putStrLn  "---------------------------------------"
    putStrLn  $ "Current Room: " ++ show (roomId node)
    putStrLn  $ "Items: " ++ show items
    printEdges ids bools
    command <- getLine

    let index = (read command :: Integer) - 1
    let allowed = fromMaybe False (getIndex bools index)
    let newNode = getIndex ids index >>= getNode nodes

    if allowed 
    then case newNode of
        Just (Item id name warp) -> case getNode nodes (R warp) of 
                                    Just warpNode -> explore nodes (name : items) warpNode
                                    Nothing -> throw $ MissingWarp $ R warp
        Just (Room id edges) -> explore nodes items (Room id edges)
        Nothing -> throw MissingNode
    else explore nodes items node



getNode :: [Node] -> Id ->  Maybe Node
getNode ((Item item name warp):rest) (I id) = if id == item then return (Item item name warp) else getNode rest (I id)
getNode ((Room room edges):rest) (R id) = if id == room then return (Room room edges) else getNode rest (R id)
getNode _ _ = Nothing

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
    putStrLn $ show count ++ ". " ++ show id ++ " - " ++ show bool
    printEdgesHelper (count+1) ids bools
printEdgesHelper _ _ _ = return ()
