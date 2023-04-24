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
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Graph
import Node
import Parser
import System.Console.ANSI
import System.IO
import Util

data GraphException
    = MissingWarp !Id
    | MissingNode
    | InvalidArgument !String
    deriving (Show)

instance Exception GraphException

main :: IO ()
main = do
    fileContents <- readFile "resources/sample.txt"
    let roomMap = buildMap roomId $ buildNodes Expert
        roomMap2 = replaceElevators roomMap (parseElevators fileContents)
        itemMap = buildMap itemId $ parse fileContents ++ pseudoItems
    case Map.lookup OLandingSite roomMap2 of
        Just node -> explore roomMap2 itemMap Map.empty Set.empty node
        Nothing -> return ()

explore :: Map RoomId Room -> Map ItemId Item -> Map ItemName Int -> Set ItemId -> Room -> IO ()
explore roomMap itemMap items colItems (Room roomId edges) = do
    let predicates = map predicate edges
        bools = eval2 predicates items colItems
        ids = map nodeId edges
    putStrLn "---------------------------------------"
    putStrLn $ "Current Room: " ++ show roomId
    putStrLn $ "Items: " ++ show (Map.assocs items)
    printEdges ids bools
    command <- getLine
    let index = (read command :: Integer) - 1
        allowed = fromMaybe False (getIndex bools index)
        nodeId = getIndex ids index
    if allowed
        then case nodeId of
                 Nothing -> error "Missing id"
                 Just (R roomId) -> explore roomMap itemMap items colItems (getVal (Map.lookup roomId roomMap) "Missing room")
                 Just (I itemId) -> case Map.lookup itemId itemMap of
                                Nothing -> error "Missing item"
                                Just (Item id name warp) -> explore roomMap itemMap (addItem name items) (Set.insert id colItems) (getVal (Map.lookup warp roomMap) "Missing room")
        else explore roomMap itemMap items colItems (Room roomId edges)

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
