module Parser (parse, parseElevators, parseArg, parseDifficulty) where

import Data.List
import Data.Map (Map, fromList, (!))
import Data.Text (Text, pack, isPrefixOf, filter, splitOn, empty, drop, lines, take, cons, stripPrefix)
import Data.Char
import Node
import Data.Bifunctor (bimap)

parse :: Text -> [Item]
parse = createItemNodes . handleExpansions . shortenTupleAreas . removePunc . createTuples . dropLines . removeEmpty . Data.Text.lines

parseElevators :: Text -> [(Int, Int)]
parseElevators = createElevatorTuples . shortenAreas . splitElevators . map filterPunc . removeEmpty . getElevatorLines . Data.Text.lines

createItemNodes :: [(Text, Text, Text)] -> [Item]
createItemNodes ((a, b, c):rest) = Item (readItemId a) (readItemName b) (getWarp c a) : createItemNodes rest
createItemNodes [] = []

readItemName :: Text -> ItemName
readItemName text = itemNameMap ! text

readItemId :: Text -> Int
readItemId text = itemIdMap ! text

readRoomId :: Text -> Int
readRoomId text = roomIdMap ! text

roomIdMap :: Map Text Int
roomIdMap = Data.Map.fromList (map (\x -> (Data.Text.pack (show x), getRoomMapKey x)) [(minBound :: RoomId) .. (maxBound :: RoomId)])

itemIdMap :: Map Text Int
itemIdMap = Data.Map.fromList (map (\x -> (Data.Text.pack (show x), getItemMapKey x)) [(minBound :: ItemId) .. (maxBound :: ItemId)])

itemNameMap :: Map Text ItemName
itemNameMap = Data.Map.fromList (map (\x -> (Data.Text.pack (show x), x)) [(minBound :: ItemName) .. (maxBound :: ItemName)])

getWarp :: Text -> Text -> Int
getWarp warp item
    | warp == Data.Text.empty = getDefaultWarp (readItemId item) defaultWarps
    | warp == Data.Text.pack "OSavestation" = getRoomMapKey OSaveStation -- There is a typo in early versions of the randomizer
    | otherwise = readRoomId warp

getDefaultWarp :: Int -> [(Int, Int)] -> Int
getDefaultWarp itemId ((room, item):rest) =
    if itemId == item
        then room
        else getDefaultWarp itemId rest
getDefaultWarp itemId [] = error $ "Couldn't find default warp for itemId: " ++ show itemId

handleExpansions :: [(Text, Text, Text)] -> [(Text, Text, Text)]
handleExpansions = map (\(a,b,c) -> (a, handleExpansion b (Data.Text.take 2 b), c))

handleExpansion :: Text -> Text -> Text
handleExpansion b first2 
    | first2 == Data.Text.pack "Mi" = Data.Text.take 7 b  --Missile
    | first2 == Data.Text.pack "Po" = Data.Text.take 9 b  --PowerBomb
    | first2 == Data.Text.pack "En" = Data.Text.take 10 b --EnergyTank
    | first2 == Data.Text.pack "Ar" = Data.Text.take 8 b  --Artifact
    | otherwise = b

shortenTupleAreas :: [(Text, Text, Text)] -> [(Text, Text, Text)]
shortenTupleAreas = map (\(a,b,c) -> (a,b,shortenArea c))

shortenAreas :: [Text] -> [Text]
shortenAreas = map shortenArea

shortenArea :: Text -> Text
shortenArea = replacePrefix "PhendranaDrifts" 'D' . replacePrefix "ChozoRuins" 'R' . replacePrefix "PhazonMines" 'M' 
                . replacePrefix "MagmoorCaverns" 'C' . replacePrefix "TallonOverworld" 'O'

replacePrefix :: String -> Char -> Text -> Text
replacePrefix prefix replacement x = case Data.Text.stripPrefix (Data.Text.pack prefix) x of
                        Just y -> Data.Text.cons replacement y
                        Nothing -> x

createTuples :: [Text] -> [(Text, Text, Text)]
createTuples
  = map
      (\ x -> (Data.Text.take 38 (Data.Text.drop 12 x), Data.Text.take 24 (Data.Text.drop 51 x), Data.Text.drop 86 x))

createElevatorTuples :: [Text] -> [(Int, Int)]
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
