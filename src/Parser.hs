module Parser (parse, parseElevators, parseArg, parseDifficulty) where

import Data.List
import Data.Text (Text, pack, unpack, isPrefixOf, filter, splitOn, empty, drop, lines, take, cons, stripPrefix)
import Data.Char
import Node
import Data.Bifunctor (bimap)

parse :: Text -> [Item]
parse input = createItemNodes $ handleExpansions $ tupleToString $ shortenTupleAreas $ removePunc $ createTuples $ dropLines $ removeEmpty $ Data.Text.lines input

parseElevators :: Text -> [(Int, Int)]
parseElevators input = createElevatorTuples $ map Data.Text.unpack $ shortenAreas $ splitElevators $ map filterPunc $ removeEmpty $ getElevatorLines $ Data.Text.lines input

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
                readItemId' s (itemId:rest) = if s == show itemId then getItemMapKey itemId else readItemId' s rest

readRoomId :: String -> Int
readRoomId str = readRoomId' str [(minBound :: RoomId) .. (maxBound :: RoomId)]
                where
                readRoomId' :: String -> [RoomId] -> Int
                readRoomId' s [] = error $ "Unable to read room ID " ++ s
                readRoomId' s (room:rest) = if s == show room then getRoomMapKey room else readRoomId' s rest

getWarp :: String -> String -> Int
getWarp warp item
    | warp == "" = getDefaultWarp (readItemId item) defaultWarps
    | warp == "OSavestation" = getRoomMapKey OSaveStation -- There is a typo in early versions of the randomizer
    | otherwise = readRoomId warp

getDefaultWarp :: Int -> [(Int, Int)] -> Int
getDefaultWarp itemId ((room, item):rest) =
    if itemId == item
        then room
        else getDefaultWarp itemId rest
getDefaultWarp itemId [] = error $ "Couldn't find default warp for itemId: " ++ show itemId

handleExpansions :: [(String, String, String)] -> [(String, String, String)]
handleExpansions = map (\(a,b,c) ->
    case b of
        'M' : 'i' : 's' : _ -> (a,"Missile",c)
        'P' : 'o' : 'w' : _ -> (a,"PowerBomb",c)
        'E' : 'n' : 'e' : _ -> (a,"EnergyTank",c)
        'A' : 'r' : 't' : _ -> (a,"Artifact",c)
        _ -> (a,b,c))

shortenTupleAreas :: [(Text, Text, Text)] -> [(Text, Text, Text)]
shortenTupleAreas = map (\(a,b,c) -> (a,b,shortenArea c))

shortenAreas :: [Text] -> [Text]
shortenAreas = map shortenArea

shortenArea :: Text -> Text
shortenArea x = 
    case Data.Text.stripPrefix (Data.Text.pack "PhendranaDrifts") x of 
        Just y -> Data.Text.cons 'D' y
        Nothing -> case Data.Text.stripPrefix (Data.Text.pack "ChozoRuins") x of
            Just z -> Data.Text.cons 'R' z
            Nothing -> case Data.Text.stripPrefix (Data.Text.pack "PhazonMines") x of
                Just w -> Data.Text.cons 'M' w
                Nothing -> case Data.Text.stripPrefix (Data.Text.pack "MagmoorCaverns") x of 
                    Just v ->  Data.Text.cons 'C' v
                    Nothing -> case Data.Text.stripPrefix  (Data.Text.pack "TallonOverworld") x of
                        Just u -> Data.Text.cons 'O' u
                        Nothing -> x

tupleToString :: [(Text,Text,Text)] -> [(String, String, String)]
tupleToString = map (\(a,b,c) -> (Data.Text.unpack a, Data.Text.unpack b ,Data.Text.unpack c))

createTuples :: [Text] -> [(Text, Text, Text)]
createTuples
  = map
      (\ x -> (Data.Text.take 38 (Data.Text.drop 12 x), Data.Text.take 24 (Data.Text.drop 51 x), Data.Text.drop 86 x))

createElevatorTuples :: [String] -> [(Int, Int)]
createElevatorTuples (a:b:rest) = (readRoomId a, readRoomId b) : createElevatorTuples rest
createElevatorTuples [] = []
createElevatorTuples _ = error "elevators should be in pairs"

removeEmpty :: [Text] -> [Text]
removeEmpty = Data.List.filter (/= Data.Text.empty)

removePunc :: [(Text, Text, Text)] -> [(Text, Text, Text)]
removePunc = map (\(a,b,c) -> (filterPunc a, filterPunc b, filterPunc c))

filterPunc :: Text -> Text
filterPunc = Data.Text.filter (`notElem` " '()\"-.|")

splitElevators :: [Text] -> [Text]
splitElevators = concatMap (Data.Text.splitOn (Data.Text.pack "<>"))

dropLines :: [Text] -> [Text]
dropLines [] = []
dropLines (x:rest) =
    if Data.Text.isPrefixOf (Data.Text.pack "Chozo") x
        then dropElevators (x : rest)
        else dropLines rest

dropElevators :: [Text] -> [Text]
dropElevators [] = []
dropElevators (x:rest) =
    if Data.Text.isPrefixOf (Data.Text.pack "Elevators:") x
        then []
        else x : dropElevators rest

getElevatorLines :: [Text] -> [Text]
getElevatorLines [] = []
getElevatorLines (x:rest) =
    if Data.Text.isPrefixOf (Data.Text.pack "Elevators:") x
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
defaultWarps = map (Data.Bifunctor.bimap getRoomMapKey getItemMapKey)
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
