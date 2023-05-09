module Parser (parse, parseElevators, parseArg, parseDifficulty) where

import Data.Char
import Node
import Data.Bifunctor (bimap)

parse :: String -> [Item]
parse input = createItemNodes $ createTuples $ handleExpansions $ shortenAreas $ removePrefixes $ removePunc $ removeEmpty $ split $ dropLines $ addDash $ removeEmpty $ lines input

parseElevators :: String -> [(Int, Int)]
parseElevators input = createElevatorTuples $ shortenAreas $ splitElevators $ removePunc $ removeEmpty $ getElevatorLines $ lines input

createItemNodes :: [(String, String, String)] -> [Item]
createItemNodes ((a, b, c):rest) = Item (readItemId a) (readItemName b) (getWarp c a) : createItemNodes rest
createItemNodes [] = []

readItemName :: String -> ItemName
readItemName str = readItemName' str [(minBound :: ItemName) .. (maxBound :: ItemName)]
                where
                readItemName' :: String -> [ItemName] -> ItemName
                readItemName' s [] = error $ "Unable to read item name " ++ s
                readItemName' s (name:rest) = if s == show name then name else readItemName' s rest

readItemId :: String -> Int
readItemId str = readItemId' str [(minBound :: ItemId) .. (maxBound :: ItemId)]
                where
                readItemId' :: String -> [ItemId] -> Int
                readItemId' s [] = error $ "Unable to read item ID " ++ s
                readItemId' s (itemId:rest) = if s == show itemId then fromEnum itemId else readItemId' s rest

readRoomId :: String -> Int
readRoomId str = readRoomId' str [(minBound :: RoomId) .. (maxBound :: RoomId)]
                where
                readRoomId' :: String -> [RoomId] -> Int
                readRoomId' s [] = error $ "Unable to read room ID " ++ s
                readRoomId' s (room:rest) = if s == show room then fromEnum room else readRoomId' s rest

getWarp :: String -> String -> Int
getWarp warp item
    | warp == "" = getDefaultWarp (readItemId item) defaultWarps
    | warp == "OSavestation" = fromEnum OSaveStation -- There is a typo in early versions of the randomizer
    | otherwise = readRoomId warp

getDefaultWarp :: Int -> [(Int, Int)] -> Int
getDefaultWarp itemId ((room, item):rest) =
    if itemId == item
        then room
        else getDefaultWarp itemId rest
getDefaultWarp itemId [] = error $ "Couldn't find default warp for itemId: " ++ show itemId

handleExpansions :: [String] -> [String]
handleExpansions (a:b:c:d:rest) =
    case c of
        'M' : 'i' : 's' : _ -> a : b : "Missile" : d : handleExpansions rest
        'P' : 'o' : 'w' : _ -> a : b : "PowerBomb" : d : handleExpansions rest
        'E' : 'n' : 'e' : _ -> a : b : "EnergyTank" : d : handleExpansions rest
        'A' : 'r' : 't' : _ -> a : b : "Artifact" : d : handleExpansions rest
        _ -> a : b : c : d : handleExpansions rest
handleExpansions [] = []
handleExpansions x = error $ show x

removePrefixes :: [String] -> [String]
removePrefixes = map (replacePrefix "Warpsto:" "")

shortenAreas :: [String] -> [String]
shortenAreas =
    map (replacePrefix "PhendranaDrifts" "D" .
         replacePrefix "ChozoRuins" "R" . replacePrefix "PhazonMines" "M" . replacePrefix "MagmoorCaverns" "C" . replacePrefix "TallonOverworld" "O")

replacePrefix :: String -> String -> String -> String
replacePrefix prefix replacement string =
    case stripPrefix prefix string of
        Just b -> replacement ++ b
        Nothing -> string

addDash :: [String] -> [String]
addDash (a:rest) =
    if length a < 77
        then (a ++ " - Warps to:") : addDash rest
        else a : addDash rest
addDash [] = []

createTuples :: [String] -> [(String, String, String)]
createTuples (_:b:c:d:rest) = (b, c, d) : createTuples rest
createTuples [] = []
createTuples _ = error "list length must be divisible by 4"

createElevatorTuples :: [String] -> [(Int, Int)]
createElevatorTuples (a:b:rest) = (readRoomId a, readRoomId b) : createElevatorTuples rest
createElevatorTuples [] = []
createElevatorTuples _ = error "elevators should be in pairs"

removeEmpty :: [String] -> [String]
removeEmpty ("":rest) = removeEmpty rest
removeEmpty (x:rest) = x : removeEmpty rest
removeEmpty [] = []

removePunc :: [String] -> [String]
removePunc = map (\xs -> [x | x <- xs, x `notElem` " '()\"-.|"])

split :: [String] -> [String]
split = concatMap (splitOn "- ")

splitElevators :: [String] -> [String]
splitElevators = concatMap (splitOn "<>")

splitOn :: String -> String -> [String]
splitOn delim str = splitOnHelper delim str []

splitOnHelper :: [Char] -> [Char] -> [Char] -> [[Char]]
splitOnHelper _ [] accum = [accum]
splitOnHelper delim (x:rest) accum =
    case stripPrefix delim (x : rest) of
        Just a -> accum : splitOnHelper delim a []
        Nothing -> splitOnHelper delim rest (accum ++ [x])

stripPrefix :: String -> String -> Maybe String
stripPrefix [] str = Just str
stripPrefix _ [] = Nothing
stripPrefix (x:prefix) (y:str) =
    if x == y
        then stripPrefix prefix str
        else Nothing

startsWith :: String -> String -> Bool
startsWith [] _ = True
startsWith _ [] = False
startsWith (x:prefix) (y:str) = x == y && startsWith prefix str

dropLines :: [String] -> [String]
dropLines [] = []
dropLines (x:rest) =
    if startsWith "Chozo" x
        then dropElevators (x : rest)
        else dropLines rest

dropElevators :: [String] -> [String]
dropElevators [] = []
dropElevators (x:rest) =
    if startsWith "Elevators:" x
        then []
        else x : dropElevators rest

getElevatorLines :: [String] -> [String]
getElevatorLines [] = []
getElevatorLines (x:rest) =
    if startsWith "Elevators:" x
        then rest
        else getElevatorLines rest

lowerString :: String -> String
lowerString str = [toLower loweredString | loweredString <- str]

parseDifficulty :: String -> DifficultyArg
parseDifficulty x
    | lowerString x == "e" || lowerString x == "easy" = Arg Easy
    | lowerString x == "m" || lowerString x == "medium" = Arg Medium
    | lowerString x == "h" || lowerString x == "hard" = Arg Hard
    | lowerString x == "v" || lowerString x == "veryhard" = Arg VeryHard
    | lowerString x == "x" || lowerString x == "expert" = Arg Expert
    | lowerString x == "a" || lowerString x == "all" = All
    | otherwise = error "Invalid difficulty"

parseArg :: [String] -> String -> Maybe String
parseArg [] _ = Nothing
parseArg [_] _ = Nothing 
parseArg (first:second:rest) flag = if first == flag then Just second else parseArg (second:rest) flag

defaultWarps :: [(Int, Int)]
defaultWarps = map (Data.Bifunctor.bimap fromEnum fromEnum)
    [ (RMainPlaza, MainPlazaHalfPipe)
    , (RMainPlaza, MainPlazaGrappleLedge)
    , (RMainPlaza, MainPlazaTree)
    , (RMainPlazaLedge, MainPlazaLockedDoor)
    , (RRuinedFountainNonWarp, RuinedFountain)
    , (RRuinedShrine, RuinedShrineBeetleBattle)
    , (RRuinedShrine, RuinedShrineHalfPipe)
    , (RRuinedShrine, RuinedShrineLowerTunnel)
    , (RVault, Vault)
    , (RTrainingChamber, TrainingChamber)
    , (RRuinedNursery, RuinedNursery)
    , (RTrainingChamberAccess, TrainingChamberAccess)
    , (RMagmaPool, MagmaPool)
    , (RTowerofLight, TowerofLight)
    , (RTowerChamber, TowerChamber)
    , (RRuinedGallery, RuinedGalleryMissileWall)
    , (RRuinedGallery, RuinedGalleryTunnel)
    , (RTransportAccessNorth, TransportAccessNorth)
    , (RGatheringHall, GatheringHall)
    , (RHiveTotem, HiveTotem)
    , (RSunchamber, SunchamberFlaahgra)
    , (RSunchamber, SunchamberGhosts)
    , (RWateryHallAccess, WateryHallAccess)
    , (RWateryHall, WateryHallScanPuzzle)
    , (RWateryHall, WateryHallUnderwater)
    , (RDynamo, DynamoLower)
    , (RDynamo, DynamoSpiderTrack)
    , (RBurnDome, BurnDomeMissile)
    , (RBurnDome, BurnDomeIDrone)
    , (RFurnace, FurnaceSpiderTracks)
    , (RFurnaceFront, FurnaceInsideFurnace)
    , (RHalloftheElders, HalloftheElders)
    , (RCrossway, Crossway)
    , (RHalloftheElders, ElderChamber) -- If non-warps, assume we are back in HotE after collecting this item.
    , (RAntechamber, Antechamber)
    , (DPhendranaShorelines, PhendranaShorelinesBehindIce)
    , (DPhendranaShorelines, PhendranaShorelinesSpiderTrack)
    , (DChozoIceTemple, ChozoIceTemple)
    , (DIceRuinsWest, IceRuinsWest)
    , (DIceRuinsEast, IceRuinsEastBehindIce)
    , (DIceRuinsEast, IceRuinsEastSpiderTrack)
    , (DChapeloftheElders, ChapeloftheElders)
    , (DCourtyardEntryway, RuinedCourtyard) -- Since the warp point is at the top, you are basically in courtyard entryway
    , (DPhendranaCanyon, PhendranaCanyon)
    , (DQuarantineCave, QuarantineCave)
    , (DResearchLabHydra, ResearchLabHydra)
    , (DQuarantineMonitor, QuarantineMonitor)
    , (DObservatory, Observatory)
    , (DTransportAccess, TransportAccess)
    , (DControlTower, ControlTower)
    , (DResearchCore, ResearchCore)
    , (DFrostCave, FrostCave)
    , (DResearchLabAether, ResearchLabAetherTank)
    , (DResearchLabAether, ResearchLabAetherMorphTrack)
    , (DGravityChamber, GravityChamberUnderwater)
    , (DGravityChamberTop, GravityChamberGrappleLedge)
    , (DStorageCave, StorageCave)
    , (DSecurityCave, SecurityCave)
    , (OLandingSite, LandingSite)
    , (OAlcove, Alcove)
    , (OFrigateCrashSite, FrigateCrashSite)
    , (OOvergrownCavern, OvergrownCavern)
    , (ORootCave, RootCave)
    , (OArtifactTemple, ArtifactTemple)
    , (OTransportTunnelB, TransportTunnelB)
    , (OArborChamber, ArborChamber)
    , (OCargoFreightLifttoDeckGamma, CargoFreightLifttoDeckGamma)
    , (OBiohazardContainment, BiohazardContainment)
    , (OHydroAccessTunnel, HydroAccessTunnel)
    , (OGreatTreeChamber, GreatTreeChamber)
    , (OLifeGroveTunnel, LifeGroveTunnel)
    , (OLifeGrove, LifeGroveStart)
    , (OLifeGrove, LifeGroveUnderwaterSpinner)
    , (MMainQuarry, MainQuarry)
    , (MSecurityAccessA, SecurityAccessA)
    , (MStorageDepotB, StorageDepotB)
    , (MStorageDepotA, StorageDepotA)
    , (MEliteResearch, EliteResearchPhazonElite)
    , (MEliteResearch, EliteResearchLaser)
    , (MEliteControlAccess, EliteControlAccess)
    , (MVentilationShaft, VentilationShaft)
    , (MPhazonProcessingCenter, PhazonProcessingCenter)
    , (MProcessingCenterAccess, ProcessingCenterAccess)
    , (MEliteQuarters, EliteQuarters)
    , (MCentralDynamo, CentralDynamo)
    , (MMetroidQuarantineBBack, MetroidQuarantineB)
    , (MMetroidQuarantineABack, MetroidQuarantineA)
    , (MFungalHallB, FungalHallB)
    , (MPhazonMiningTunnel, PhazonMiningTunnel)
    , (MFungalHallAccess, FungalHallAccess)
    , (CLavaLake, LavaLake)
    , (CTriclopsPit, TriclopsPit)
    , (CStorageCavern, StorageCavern)
    , (CTransportTunnelA, TransportTunnelA)
    , (CWarriorShrine, WarriorShrine)
    , (CShoreTunnel, ShoreTunnel)
    , (CFieryShores, FieryShoresMorphTrack)
    , (CFieryShores, FieryShoresWarriorShrineTunnel)
    , (CPlasmaProcessing, PlasmaProcessing)
    , (CMagmoorWorkstation, MagmoorWorkstation)
    ]
