module Util where

import Node

import Data.Map (Map)
import qualified Data.Map as Map
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

-- Apply all functions in a list to the same two args
eval2 :: [a -> b -> c] -> a -> b -> [c]
eval2 funcList arg1 arg2 = map (\x -> x arg1 arg2) funcList

getVal :: Maybe a -> String -> a
getVal m msg =
    case m of
        Nothing -> error msg
        Just a -> a

nonEmpty :: [a] -> Bool
nonEmpty [] = False
nonEmpty _ = True

countOf :: [ItemName] -> [ItemName] -> Int
countOf mainList itemsToCount = countOf' mainList itemsToCount 0
  where
    countOf' :: [ItemName] -> [ItemName] -> Int -> Int
    countOf' items (check:rest) count =
        if check `elem` items
            then countOf' items rest (count + 1)
            else countOf' items rest count
    countOf' _ [] count = count

addItem :: ItemName -> Map ItemName Int -> Map ItemName Int
addItem name graph =
    case Map.lookup name graph of
        Just num -> Map.insert name (num + 1) graph
        Nothing -> Map.insert name 1 graph

removeAll :: Map ItemName Int -> [ItemName] -> Map ItemName Int
removeAll graph [] = graph
removeAll graph (y:rest) =
    case Map.lookup y graph of
        Just num ->
            if num == 1
                then removeAll (Map.delete y graph) rest
                else removeAll (Map.insert y (num - 1) graph) rest
        Nothing -> removeAll graph rest

removeSet :: [ItemId] -> Set ItemId -> [ItemId]
removeSet [] _ = []
removeSet (x:rest) set =
    if Set.member x set
        then removeSet rest set
        else x : removeSet rest set

-- Helper functions for Predicates
containsCount :: Int -> ItemName -> Map ItemName Int -> Bool
containsCount num element graph
    | num < 0 = False
    | num == 0 = True
    | otherwise = num <= fromMaybe 0 (Map.lookup element graph)

contains :: Map ItemName Int -> ItemName -> Bool
contains m x = Map.member x m

containsAll :: Map ItemName Int -> [ItemName] -> Bool
containsAll _ [] = True
containsAll graph (x:rest) = contains graph x && containsAll graph rest

containsAny :: Map ItemName Int -> [ItemName] -> Bool
containsAny _ [] = False
containsAny graph (x:rest) = contains graph x || containsAny graph rest

listContains :: [ItemName] -> ItemName -> Bool
listContains items item = item `Prelude.elem` items

listContainsAll :: [ItemName] -> [ItemName] -> Bool
listContainsAll [] [] = True
listContainsAll _ [] = True
listContainsAll [] _ = False
listContainsAll items (x:rest) = listContains items x && listContainsAll items rest

listContainsAny :: [ItemName] -> [ItemName] -> Bool
listContainsAny [] [] = False
listContainsAny _ [] = False
listContainsAny [] _ = False
listContainsAny items (x:rest) = listContains items x || listContainsAny items rest

checkBools :: [a] -> [Bool] -> [a]
checkBools (i:rest1) (bool:rest2) =
    if bool
        then i : checkBools rest1 rest2
        else checkBools rest1 rest2
checkBools _ _ = []

mapLookup :: (Ord a, Show a) => a -> Map a b -> b
mapLookup x m = case Map.lookup x m of
                Just result -> result
                Nothing -> error $ "failed to lookup " ++ show x

intMapLookup :: Int -> IntMap a -> a
intMapLookup x m = case IntMap.lookup x m of
                Just result -> result
                Nothing -> error $ "failed to lookup " ++ show x
