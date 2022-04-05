module Main where

import Parser
import System.IO  
import Control.Monad
import Node
import Graph
import Data.Maybe

main :: IO ()
main = do
   explore buildNodes [] (getNode buildNodes (R RHiveTotem))
  

explore :: [Node] -> [ItemName] -> Node -> IO ()
explore nodes items node = do
    let edgeList = edges node
    let predicates = map canUse edgeList
    let ids = map nodeId edgeList
    let bools = eval predicates items
    print $ roomId node
    print items
    print ids
    print bools
    command <- getLine
    let index = read command :: Integer
    let newNodeId = fromMaybe (R OLandingSite) (getIndex ids (index-1))
    let newNode = getNode nodes newNodeId
    let allowed = fromMaybe False (getIndex bools (index-1))
    if allowed 
        then case newNodeId of
            I id -> explore nodes (itemName (getNode nodes (I id)) : items) (getNode nodes (R $ warp newNode))
            R id -> explore nodes items (getNode nodes (R id))
        else explore nodes items node



getNode :: [Node] -> Id ->  Node
getNode [] x = Room OLandingSite [] -- Hopefully this doesn't happen
getNode ((Item item name warp):rest) x = case x of 
                            I id -> if id == item then Item item name warp else getNode rest x
                            R id -> getNode rest x
getNode ((Room room edges):rest) x = case x of 
                            I id -> getNode rest x
                            R id -> if id == room then Room room edges else getNode rest x

getIndex :: [a] -> Integer -> Maybe a
getIndex [] i = Nothing
getIndex (x:rest) 0 = Just x
getIndex (x:rest) i = getIndex rest (i-1) 

eval :: [[ItemName] -> Bool] -> [ItemName] -> [Bool]
eval predicates items = map (\ x -> x items) predicates

printBool :: Bool -> IO ()
printBool bool = do
    print bool

printEdge :: Edge -> IO ()
printEdge edge = do
    print $ nodeId edge
