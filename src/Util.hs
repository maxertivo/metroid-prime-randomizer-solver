module Util where

    getIndex :: [a] -> Integer -> Maybe a
    getIndex [] i = Nothing
    getIndex (x:rest) 0 = Just x
    getIndex (x:rest) i = getIndex rest (i-1) 

    -- Apply all functions in a list to the same arg
    eval :: [a -> b] -> a -> [b]
    eval funcList arg = map (\ x -> x arg) funcList

    getVal :: Maybe a -> String -> a
    getVal maybe msg = case maybe of
        Nothing -> error msg
        Just a -> a
    
    nonEmpty :: [a] -> Bool
    nonEmpty [] = False
    nonEmpty _ = True

    countOf :: (Eq a) => [a] -> [a] -> Int
    countOf mainList itemsToCount = f mainList itemsToCount 0
        where f :: (Eq a) => [a] -> [a] -> Int -> Int
              f items (check:rest) count = if check `elem` items then f items rest (count+1) else f items rest count
              f items [] count = count