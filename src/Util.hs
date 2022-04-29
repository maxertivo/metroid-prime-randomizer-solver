module Util where

import Node

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

getIndex :: [a] -> Integer -> Maybe a
getIndex [] i = Nothing
getIndex (x:rest) 0 = Just x
getIndex (x:rest) i = getIndex rest (i - 1)
    
-- Apply all functions in a list to the same two args
eval2 :: [a -> b -> c] -> a -> b -> [c]
eval2 funcList arg1 arg2 = map (\x -> x arg1 arg2) funcList

getVal :: Maybe a -> String -> a
getVal maybe msg =
    case maybe of
        Nothing -> error msg
        Just a -> a

compareElem :: (Ord a) => [a] -> [a] -> Ordering
compareElem [] [] = EQ
compareElem (a:rest1) (b:rest2)
    | a < b = LT
    | a > b = GT
    | otherwise = compareElem rest1 rest2
compareElem (a:rest1) [] = GT
compareElem [] (b:rest2) = LT

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
    f items [] count = count

addItem :: ItemName -> Map ItemName Int -> Map ItemName Int
addItem name map = case Map.lookup name map of
    Just num -> Map.insert name (num+1) map
    Nothing -> Map.insert name 1 map

removeAll :: Map ItemName Int -> [ItemName] -> Map ItemName Int
removeAll map [] = map
removeAll map (y:rest) = case Map.lookup y map of
    Just num -> if num == 1 then Map.delete y map else Map.insert y (num-1) map
    Nothing -> map

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
containsCount num element map
    | num < 0 = False
    | num == 0 = True
    | otherwise = num <= fromMaybe 0 (Map.lookup element map)

contains :: Map ItemName Int -> ItemName -> Bool
contains map item = case Map.lookup item map of 
    Just num -> True
    Nothing -> False

containsAll :: Map ItemName Int -> [ItemName] -> Bool
containsAll _ [] = True
containsAll map (x:rest) = contains map x && containsAll map rest

containsAny :: Map ItemName Int -> [ItemName] -> Bool
containsAny _ [] = False
containsAny map (x:rest) = contains map x || containsAny map rest

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
checkBools (id:rest1) (bool:rest2) = if bool then id : checkBools rest1 rest2  else checkBools rest1 rest2
checkBools _ _ = []

getRoomIds :: [Id] -> [RoomId]                   
getRoomIds ((R roomId):rest) = roomId : getRoomIds rest
getRoomIds (item:rest) = getRoomIds rest
getRoomIds [] = []

getItemIds :: [Id] -> [ItemId]                   
getItemIds ((I itemId):rest) = itemId : getItemIds rest
getItemIds (room:rest) = getItemIds rest
getItemIds [] = []