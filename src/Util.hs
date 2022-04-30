module Util where

import Node

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

getIndex :: [a] -> Integer -> Maybe a
getIndex [] _ = Nothing
getIndex (x:_) 0 = Just x
getIndex (_:rest) i = getIndex rest (i - 1)
    
-- Apply all functions in a list to the same two args
eval2 :: [a -> b -> c] -> a -> b -> [c]
eval2 funcList arg1 arg2 = map (\x -> x arg1 arg2) funcList

getVal :: Maybe a -> String -> a
getVal m msg =
    case m of
        Nothing -> error msg
        Just a -> a

compareElem :: (Ord a) => [a] -> [a] -> Ordering
compareElem [] [] = EQ
compareElem (a:rest1) (b:rest2)
    | a < b = LT
    | a > b = GT
    | otherwise = compareElem rest1 rest2
compareElem _ [] = GT
compareElem [] _ = LT

nonEmpty :: [a] -> Bool
nonEmpty [] = False
nonEmpty _ = True

countOf :: (Eq a) => [a] -> [a] -> Int
countOf mainList itemsToCount = f mainList itemsToCount 0
  where
    f :: (Eq a) => [a] -> [a] -> Int -> Int
    f items (check:rest) count =
        if check `elem` items
            then f items rest (count + 1)
            else f items rest count
    f _ [] count = count

addItem :: ItemName -> Map ItemName Int -> Map ItemName Int
addItem name graph = case Map.lookup name graph of
    Just num -> Map.insert name (num+1) graph
    Nothing -> Map.insert name 1 graph

removeAll :: Map ItemName Int -> [ItemName] -> Map ItemName Int
removeAll graph [] = graph
removeAll graph (y:rest) = case Map.lookup y graph of
    Just num -> if num == 1 then removeAll (Map.delete y graph) rest else removeAll (Map.insert y (num-1) graph) rest
    Nothing -> removeAll graph rest

removeSet :: (Ord a) => [a] -> Set a -> [a]
removeSet [] _ = []
removeSet (x:rest) set = if Set.member x set then removeSet rest set else x : removeSet rest set

minMaybe :: (Ord a) => Maybe a -> Maybe a -> Maybe a
minMaybe Nothing Nothing = Nothing
minMaybe Nothing (Just y) = Just y
minMaybe (Just x) Nothing = Just x
minMaybe (Just x) (Just y) = Just (min x y)

-- Helper functions for Predicates
containsCount :: Int -> ItemName -> Map ItemName Int-> Bool
containsCount num element graph
    | num < 0 = False
    | num == 0 = True
    | otherwise = num <= fromMaybe 0 (Map.lookup element graph)

contains :: Map ItemName Int -> ItemName -> Bool
contains graph item = case Map.lookup item graph of 
    Just _ -> True
    Nothing -> False

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
                        
checkBools :: [Id] -> [Bool] -> [Id]
checkBools (i:rest1) (bool:rest2) = if bool then i : checkBools rest1 rest2  else checkBools rest1 rest2
checkBools _ _ = []

getRoomIds :: [Id] -> [RoomId]                   
getRoomIds ((R roomId):rest) = roomId : getRoomIds rest
getRoomIds (_:rest) = getRoomIds rest
getRoomIds [] = []

getItemIds :: [Id] -> [ItemId]                   
getItemIds ((I itemId):rest) = itemId : getItemIds rest
getItemIds (_:rest) = getItemIds rest
getItemIds [] = []