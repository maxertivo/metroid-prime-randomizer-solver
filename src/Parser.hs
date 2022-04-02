module Parser where 
    
    parse :: String ->  [(String,String,String)]
    parse input =  createTuples $ handleExpansions $ shortenAreas $ removePunc $ removePrefixes $ removeEmpty $ split $ drop 3 $ addDash $ lines input

    handleExpansions :: [String] -> [String]
    handleExpansions (x:rest) = case x of
        "MissileExpansion" -> "Missile" : handleExpansions rest
        "MissileLauncher" -> "Missile" : handleExpansions rest
        "PowerBombExpansion" -> "PowerBomb" : handleExpansions rest
        _ -> x : handleExpansions rest
    handleExpansions [] = []

    removePrefixes :: [String] -> [String]
    removePrefixes (a:rest) =  case stripPrefix "Warps to:" a of
        Just b -> b : removePrefixes rest 
        Nothing ->  a : removePrefixes rest
    removePrefixes [] = []

    shortenAreas :: [String] -> [String]
    shortenAreas (a:rest) =  case stripPrefix "TallonOverworld" a of
        Just b -> ("O" ++ b) : shortenAreas rest 
        Nothing -> case stripPrefix "MagmoorCaverns" a of
            Just b -> ("C" ++ b) : shortenAreas rest 
            Nothing -> case stripPrefix "PhazonMines" a of
                Just b -> ("M" ++ b) : shortenAreas rest 
                Nothing -> case stripPrefix "ChozoRuins" a of
                    Just b -> ("R" ++ b) : shortenAreas rest 
                    Nothing -> case stripPrefix "PhendranaDrifts" a of
                        Just b -> ("D" ++ b) : shortenAreas rest 
                        Nothing -> a : shortenAreas rest
    shortenAreas [] = []

    addDash :: [String] -> [String]
    addDash (a:rest) = if length a < 77 then (a ++ " - Warps to:") : addDash rest else a : addDash rest
    addDash [] = []

    createTuples :: [String] -> [(String,String,String)]
    createTuples (a:b:c:d:rest) = (b,c,d): createTuples rest
    createTuples x = [] 

    removeEmpty :: [String] -> [String]
    removeEmpty ("":rest) = removeEmpty rest
    removeEmpty (x:rest) = x : removeEmpty rest
    removeEmpty [] = []

    removePunc :: [String] -> [String]
    removePunc = map (\ xs -> [x | x <- xs, x `notElem` " ()\"1234567890-|"])

    split :: [String] -> [String]
    split = concatMap (splitOn "- ")

    splitOn :: String -> String -> [String]
    splitOn delim str = splitOnHelper delim str []

    splitOnHelper :: [Char] -> [Char] -> [Char] -> [[Char]]
    splitOnHelper delim [] accum = [accum]
    splitOnHelper delim (x:rest) accum = case stripPrefix delim (x:rest) of
        Just a ->  accum : splitOnHelper delim a []
        Nothing -> splitOnHelper delim rest (accum ++ [x])
    
    stripPrefix :: String -> String -> Maybe String
    stripPrefix [] [] = Just []
    stripPrefix [] str = Just str
    stripPrefix delim [] = Nothing
    stripPrefix (x:delim) (y:str) = if x == y then stripPrefix delim str else Nothing

    