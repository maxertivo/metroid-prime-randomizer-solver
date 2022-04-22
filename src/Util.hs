module Util where

getIndex :: [a] -> Integer -> Maybe a
getIndex [] i = Nothing
getIndex (x:rest) 0 = Just x
getIndex (x:rest) i = getIndex rest (i - 1)
    
-- Apply all functions in a list to the same arg
eval :: [a -> b] -> a -> [b]
eval funcList arg = map (\x -> x arg) funcList
    
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
