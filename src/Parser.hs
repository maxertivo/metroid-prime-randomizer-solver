module Parser where

import Data.Char
import Node
import Text.Read

parse :: String -> [Node]
parse input = createItemNodes $ createTuples $ handleExpansions $ shortenAreas $ removePrefixes $ removePunc $ removeEmpty $ split $ dropLines $ addDash $ removeEmpty $ lines input

createItemNodes :: [(String, String, String)] -> [Node]
createItemNodes ((a, b, c):rest) = Item (readStr a :: ItemId) (readStr b :: ItemName) (getWarp c a) : createItemNodes rest
createItemNodes [] = []

getWarp :: String -> String -> RoomId
getWarp warp item
    | warp == "" = getDefaultWarp (readStr item :: ItemId) defaultWarps
    | warp == "OSavestation" = OSaveStation -- There is a typo in early versions of the randomizer
    | otherwise = readStr warp :: RoomId

getDefaultWarp :: ItemId -> [(RoomId, ItemId)] -> RoomId
getDefaultWarp itemId ((room, item):rest) =
    if itemId == item
        then room
        else getDefaultWarp itemId rest
getDefaultWarp itemId [] = error $ "Couldn't find default warp for itemId: " ++ show itemId

readStr :: Read a => String -> a
readStr str =
    case readMaybe str of
        Just result -> result
        Nothing -> error $ "Cannot read string: " ++ str

handleExpansions :: [String] -> [String]
handleExpansions (a:b:c:d:rest) =
    case removeNum c of
        "MissileExpansion" -> a : b : "Missile" : d : handleExpansions rest
        "MissileLauncher" -> a : b : "Missile" : d : handleExpansions rest
        "PowerBombExpansion" -> a : b : "PowerBomb" : d : handleExpansions rest
        "EnergyTank" -> a : b : "EnergyTank" : d : handleExpansions rest
        'A':'r':'t':'i':'f':'a':'c':'t':_ -> a : b : "Artifact" : d : handleExpansions rest
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
createTuples _ = []

removeEmpty :: [String] -> [String]
removeEmpty ("":rest) = removeEmpty rest
removeEmpty (x:rest) = x : removeEmpty rest
removeEmpty [] = []

removePunc :: [String] -> [String]
removePunc = map (\xs -> [x | x <- xs, x `notElem` " '()\"-.|"])

removeNum :: String -> String
removeNum xs = [x | x <- xs, x `notElem` "1234567890"]

split :: [String] -> [String]
split = concatMap (splitOn "- ")

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

lowerString :: String -> String
lowerString str = [ toLower loweredString | loweredString <- str]

parseDifficulty :: String -> Difficulty
parseDifficulty x
    | lowerString x == "e" || lowerString x == "easy" = Easy
    | lowerString x == "m" || lowerString x == "medium" = Medium
    | lowerString x == "h" || lowerString x == "hard" = Hard
    | lowerString x == "v" || lowerString x == "veryhard" = VeryHard
    | lowerString x == "x" || lowerString x == "expert" = Expert
    | otherwise = error "Invalid difficulty"

defaultWarps :: [(RoomId, ItemId)]
defaultWarps =
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
    , (RElderChamber, ElderChamber)
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
    , (MMineSecurityStation, MMineSecurityStation)
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
