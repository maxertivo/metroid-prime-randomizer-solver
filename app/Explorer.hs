{-- 
    This application was mostly used for debugging in the early stages of development. 
    If you want to run it, run the following from the root of the project:

    cabal run explorer
--}
module Main where

import Control.Exception
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Graph
import Node
import Parser
import System.Console.ANSI
import System.IO
import Util

data Id = R Int | I Int 

instance Show Id where
    show id = case id of 
        R roomId -> show (getRoomIdFromKey roomId)
        I itemId -> show (getItemIdFromKey itemId)

data GenericEdge = GenericEdge {genPredicate :: Map ItemName Int -> Set Int -> Bool, nodeId :: Id}

main :: IO ()
main = do
    fileContents <- readFile "resources/sample.txt"
    let roomMap = buildRoomMap $ buildNodes Expert
        roomMap2 = replaceElevators roomMap (parseElevators fileContents)
        itemMap = buildItemMap $ parse fileContents ++ pseudoItems
    case IntMap.lookup (getRoomMapKey OLandingSite) roomMap2 of
        Just node -> explore roomMap2 itemMap Map.empty Set.empty node
        Nothing -> return ()

explore :: IntMap Room -> IntMap Item -> Map ItemName Int -> Set Int -> Room -> IO ()
explore roomMap itemMap items colItems (Room roomId roomEdges itemEdges) = do
    let edges = convertEdges roomEdges itemEdges
        predicates = map genPredicate edges
        bools = eval2 predicates items colItems
        ids = map nodeId edges
    putStrLn "---------------------------------------"
    putStrLn $ "Current Room: " ++ show (getRoomIdFromKey roomId)
    putStrLn $ "Items: " ++ show (Map.assocs items)
    printEdges ids bools
    command <- getLine
    let index = (read command :: Integer) - 1
        allowed = fromMaybe False (getIndex bools index)
        nodeId = getIndex ids index
    if allowed
        then case nodeId of
                 Nothing -> error "Missing id"
                 Just (R roomId) -> explore roomMap itemMap items colItems (getVal (IntMap.lookup roomId roomMap) "Missing room")
                 Just (I itemId) -> case IntMap.lookup itemId itemMap of
                                Nothing -> error "Missing item"
                                Just (Item id name warp) -> explore roomMap itemMap (addItem name items) (Set.insert id colItems) (getVal (IntMap.lookup warp roomMap) "Missing room")
        else explore roomMap itemMap items colItems (Room roomId roomEdges itemEdges)

printEdges :: [Id] -> [Bool] -> IO ()
printEdges = printEdgesHelper 1

printEdgesHelper :: Integer -> [Id] -> [Bool] -> IO ()
printEdgesHelper count (id:ids) (bool:bools) = do
    if bool
        then setSGR [SetColor Foreground Vivid Green]
        else setSGR [SetColor Foreground Vivid Red]
    putStrLn $ show count ++ ". " ++ show id ++ " - " ++ show bool
    setSGR [Reset]
    printEdgesHelper (count + 1) ids bools
printEdgesHelper _ _ _ = return ()

getIndex :: [a] -> Integer -> Maybe a
getIndex [] _ = Nothing
getIndex (x:_) 0 = Just x
getIndex (_:rest) i = getIndex rest (i - 1)

convertEdges :: [Edge] -> [IEdge] -> [GenericEdge]
convertEdges [] [] = []
convertEdges [] (IEdge itemPredicate itemId:rest) = GenericEdge itemPredicate (I itemId) : convertEdges [] rest
convertEdges (Edge predicate roomId : rest) iEdges = GenericEdge predicate (R roomId) : convertEdges rest iEdges