module Graph where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Node
import Predicates
import Util

{-- 
The graph is split into two parts: a room map and an item map.

We calculate if a seed is completable by constructing the graph, and then calling isCompletable.
This function does the following:
    1. Collects all "free" items. That is, items where we can handle the warp and the warp is not useful for later
    2. Checks if you can complete the game with current items. If so, we are done and can complete the game.
    3. Tries to find a sequence of at most 5 warps that collects at least one progression item
    4. If no such sequences can be found, we are done and cannot complete the game. Otherwise, repeat steps 1-3.

Graph Construction:
Much of the graph construction is hard-coded. During construction, the difficulty is passed in to any edge predicate that requires it, 
thereby eliminating the need to pass in the difficulty when solving the graph.

Non-Warping Items:
If the item does not warp, it is treated as if it warps to the room containing the item (Or in some cases, an adjacent room that is more appropriate).
This is mostly useful for non-warp randomizer seeds. 
--}

buildItemMap :: [Item] -> IntMap Item
buildItemMap items = IntMap.fromList (map (\x -> (fromEnum (itemId x), x)) items)

buildRoomMap :: [Room] -> IntMap Room
buildRoomMap rooms = IntMap.fromList (map (\x -> (fromEnum (roomId x), x)) rooms)

replaceElevators :: IntMap Room -> [(Int, Int)] -> IntMap Room
replaceElevators graph [] = graph
replaceElevators graph ((a, b):rest) =
    let oldNode = getVal (IntMap.lookup a graph) "Missing room"
        newNode = replaceEdgeRoom oldNode a b
     in replaceElevators (IntMap.insert a newNode graph) rest

replaceEdgeRoom :: Room -> Int -> Int -> Room
replaceEdgeRoom (Room rId edgeList itemEdges) original replacement =
    let newEdges = f edgeList original replacement
     in Room (fromEnum rId)newEdges itemEdges
  where
    f :: [Edge] -> Int -> Int -> [Edge]
    f [] _ _ = []
    f ((Edge p roomId):rest) orig replace =
        if roomId `elem` elevatorRooms
            then Edge p (fromEnum replace) : f rest orig replace
            else Edge p (fromEnum roomId) : f rest orig replace

elevatorRooms :: [Int]
elevatorRooms = map fromEnum [RTransporttoTallonOverworldNorth ,RTransporttoMagmoorCavernsNorth ,RTransporttoTallonOverworldEast ,RTransporttoTallonOverworldSouth
    ,DTransporttoMagmoorCavernsWest ,DTransporttoMagmoorCavernsSouth ,OTransporttoChozoRuinsWest ,OTransporttoChozoRuinsEast ,OTransporttoMagmoorCavernsEast 
    ,OTransporttoChozoRuinsSouth ,OTransporttoPhazonMinesEast,MTransporttoTallonOverworldSouth ,MTransporttoMagmoorCavernsSouth ,CTransporttoChozoRuinsNorth
    ,CTransporttoPhendranaDriftsNorth ,CTransporttoTallonOverworldWest ,CTransporttoPhazonMinesWest ,CTransporttoPhendranaDriftsSouth]

pseudoItems :: [Item]
pseudoItems = [Item (fromEnum FrigatePowerDoorTrigger) FrigatePowerDoor (fromEnum OMainVentilationShaftSectionB)
            ,Item (fromEnum MainQuarryBarrierTrigger) MainQuarryBarrier (fromEnum MMainQuarry)
            ,Item (fromEnum MainQuarrySaveTrigger) MainQuarrySaveUnlocked (fromEnum MSaveStationMinesA)
            ,Item (fromEnum ChozoIceTempleTrigger) ChozoIceTempleBarrier (fromEnum DChozoIceTemple)
            ,Item (fromEnum StorageDepotATrigger) StorageDepotABarrier (fromEnum MMineSecurityStation)
            ,Item (fromEnum ResearchLabHydraTrigger) ResearchLabHydraBarrier (fromEnum DResearchLabHydra)
            ,Item (fromEnum EliteControlTrigger) EliteControlBarrier (fromEnum MEliteControl)
            ,Item (fromEnum MetroidQuarantineATrigger) MetroidQuarantineABarrier (fromEnum MMetroidQuarantineA)
            ,Item (fromEnum MetroidQuarantineBTrigger) MetroidQuarantineBBarrier (fromEnum MMetroidQuarantineB)
            ,Item (fromEnum OmegaPirateEntranceTrigger) OmegaPirateEntranceBarrier (fromEnum MEliteQuartersAccess)]

pseudoItemNames :: [ItemName]
pseudoItemNames = map itemName pseudoItems

{-- This creates all rooms to add to the graph --}
buildNodes :: Difficulty -> [Room]
buildNodes diff = [ -- Tallon Overworld Rooms
            Room (fromEnum OLandingSite)[Edge noReq (fromEnum OCanyonCavern)
                                    ,Edge noReq (fromEnum OWaterfallCavern)
                                    ,Edge (sjf diff) (fromEnum OGully)
                                    ,Edge (sjf diff) (fromEnum OAlcove)
                                    ,Edge noReq (fromEnum OTempleHall)]
                                    [IEdge morph (fromEnum LandingSite)]
            ,Room (fromEnum OAlcove)[Edge noReq (fromEnum OLandingSite)]
                                    [IEdge noReq (fromEnum Alcove)]
            ,Room (fromEnum OCanyonCavern)[Edge noReq (fromEnum OLandingSite)
                                    ,Edge noReq (fromEnum OTallonCanyon)
                                    ,Edge (tallonCanyonSw diff) (fromEnum OTallonFrontSw)] []
            ,Room (fromEnum OTallonCanyon)[Edge noReq (fromEnum OCanyonCavern)
                                    ,Edge boostBombs (fromEnum OGully)
                                    ,Edge noReq (fromEnum ORootTunnel)
                                    ,Edge noReq (fromEnum OTransportTunnelA)] []
            ,Room (fromEnum OGully)[Edge bombs (fromEnum OTallonCanyon)
                                    ,Edge noReq (fromEnum OLandingSite)] []
            ,Room (fromEnum ORootTunnel)[Edge noReq (fromEnum OTallonCanyon)
                                    ,Edge missile (fromEnum ORootCave)] []
            ,Room (fromEnum ORootCave)[Edge missile (fromEnum ORootTunnel)
                                    ,Edge noReq (fromEnum OTransportTunnelB)
                                    ,Edge (arbor diff) (fromEnum OArborChamber)]
                                    [IEdge (rootCaveItem diff) (fromEnum RootCave)]
            ,Room (fromEnum OTransportTunnelB)[Edge noReq (fromEnum ORootCave)
                                    ,Edge noReq (fromEnum OTransporttoMagmoorCavernsEast)]
                                    [IEdge noReq (fromEnum TransportTunnelB)]
            ,Room (fromEnum OTransporttoMagmoorCavernsEast)[Edge noReq (fromEnum CTransporttoTallonOverworldWest)
                                    ,Edge noReq (fromEnum OTransportTunnelB)] []
            ,Room (fromEnum OArborChamber)[Edge noReq (fromEnum ORootCave)]
                                    [IEdge noReq (fromEnum ArborChamber)]
            ,Room (fromEnum OTransportTunnelA)[Edge noReq (fromEnum OTallonCanyon)
                                    ,Edge noReq (fromEnum OTransporttoChozoRuinsWest)] []
            ,Room (fromEnum OTransporttoChozoRuinsWest)[Edge noReq (fromEnum OTransportTunnelA)
                                    ,Edge noReq (fromEnum RTransporttoTallonOverworldNorth)] []
            ,Room (fromEnum OWaterfallCavern)[Edge noReq (fromEnum OLandingSite)
                                    ,Edge morphMissile (fromEnum OFrigateCrashSite)] []
            ,Room (fromEnum OFrigateCrashSite)[Edge noReq (fromEnum OWaterfallCavern)
                                    ,Edge (fcsClimb diff) (fromEnum OOvergrownCavern)
                                    ,Edge (fcsEntry diff) (fromEnum OFrigateAccessTunnel)]
                                    [IEdge (fcsItem diff) (fromEnum FrigateCrashSite)]
            ,Room (fromEnum OOvergrownCavern)[Edge ice (fromEnum OFrigateCrashSite)
                                    ,Edge ice (fromEnum OTransportTunnelC)]
                                    [IEdge morph (fromEnum OvergrownCavern)]
            ,Room (fromEnum OTransportTunnelC)[Edge ice (fromEnum OOvergrownCavern)
                                    ,Edge ice (fromEnum OTransporttoChozoRuinsEast)] []
            ,Room (fromEnum OTransporttoChozoRuinsEast)[Edge noReq (fromEnum RTransporttoTallonOverworldEast)
                                    ,Edge ice (fromEnum OTransportTunnelC)] []
            ,Room (fromEnum OFrigateAccessTunnel)[Edge ice (fromEnum OFrigateCrashSite)
                                    ,Edge noReq (fromEnum OMainVentilationShaftSectionC)] []
            ,Room (fromEnum OMainVentilationShaftSectionC)[Edge noReq (fromEnum OFrigateAccessTunnel)
                                    ,Edge noReq (fromEnum OMainVentilationShaftSectionB)] []
            ,Room (fromEnum OMainVentilationShaftSectionB)[Edge wave (fromEnum OMainVentilationShaftSectionA)
                                    ,Edge (climbFrigateMvs diff) (fromEnum OMainVentilationShaftSectionC)]
                                    [IEdge wave (fromEnum FrigatePowerDoorTrigger)]
            ,Room (fromEnum OMainVentilationShaftSectionA)[Edge (frigatePowerDoor diff) (fromEnum OMainVentilationShaftSectionB)
                                    ,Edge noReq (fromEnum OReactorCore)] []
            ,Room (fromEnum OReactorCore)[Edge (climbReactorCore diff) (fromEnum OMainVentilationShaftSectionA)
                                    ,Edge wave (fromEnum OReactorAccess)] []
            ,Room (fromEnum OReactorAccess)[Edge wave (fromEnum OCargoFreightLifttoDeckGamma)
                                    ,Edge noReq (fromEnum OReactorCore)
                                    ,Edge noReq (fromEnum OSaveStation)] []
            ,Room (fromEnum OSaveStation)[Edge noReq (fromEnum OReactorAccess)] []
            ,Room (fromEnum OCargoFreightLifttoDeckGamma)[Edge (cargoFreightLift diff) (fromEnum ODeckBetaTransitHall)
                                    ,Edge noReq (fromEnum OReactorAccess)]
                                    [IEdge missile (fromEnum CargoFreightLifttoDeckGamma)]
            ,Room (fromEnum ODeckBetaTransitHall)[Edge noReq (fromEnum OCargoFreightLifttoDeckGamma)
                                    ,Edge noReq (fromEnum OBiohazardContainment)] []
            ,Room (fromEnum OBiohazardContainment)[Edge noReq (fromEnum ODeckBetaTransitHall)
                                    ,Edge (biohazard diff) (fromEnum ODeckBetaSecurityHall)]
                                    [IEdge supers (fromEnum BiohazardContainment)]
            ,Room (fromEnum ODeckBetaSecurityHall)[Edge (climbBiohazard diff) (fromEnum OBiohazardContainment)
                                    ,Edge noReq (fromEnum OBiotechResearchArea1)]
                                    [IEdge supers (fromEnum BiohazardContainment)]
            ,Room (fromEnum OBiotechResearchArea1)[Edge noReq (fromEnum ODeckBetaSecurityHall)
                                    ,Edge (biotech diff) (fromEnum ODeckBetaConduitHall)] []
            ,Room (fromEnum ODeckBetaConduitHall)[Edge (biotechReverse diff) (fromEnum OBiotechResearchArea1)
                                    ,Edge noReq (fromEnum OConnectionElevatortoDeckBeta)] []
            ,Room (fromEnum OConnectionElevatortoDeckBeta)[Edge noReq (fromEnum ODeckBetaConduitHall)
                                    ,Edge noReq (fromEnum OHydroAccessTunnel)] []
            ,Room (fromEnum OHydroAccessTunnel)[Edge gravSpace (fromEnum OConnectionElevatortoDeckBeta)
                                    ,Edge (hydroTunnel diff) (fromEnum OGreatTreeHall)]
                                    [IEdge morph (fromEnum HydroAccessTunnel)]
            --Great Tree Hall is split into two rooms, an upper and lower section
            ,Room (fromEnum OGreatTreeHall)[Edge (hydroTunnel diff) (fromEnum OHydroAccessTunnel)
                                    ,Edge ice (fromEnum OTransportTunnelE)
                                    ,Edge (gthClimb diff) (fromEnum OGreatTreeHallTop)] []
            ,Room (fromEnum OGreatTreeHallTop)[Edge ice (fromEnum OTransportTunnelD)
                                    ,Edge (gtcEnter diff) (fromEnum OGreatTreeChamber)
                                    ,Edge (gthSpiderTrack diff) (fromEnum OLifeGroveTunnel)
                                    ,Edge (bars diff) (fromEnum OGreatTreeHall)] []
            ,Room (fromEnum OTransportTunnelD)[Edge ice (fromEnum OGreatTreeHallTop)
                                    ,Edge ice (fromEnum OTransporttoChozoRuinsSouth)] []
            ,Room (fromEnum OTransporttoChozoRuinsSouth)[Edge ice (fromEnum OTransportTunnelD)
                                    ,Edge noReq (fromEnum RTransporttoTallonOverworldSouth)] []
            ,Room (fromEnum OGreatTreeChamber)[Edge noReq (fromEnum OGreatTreeHallTop)
                                    ,Edge (gtcSw diff) (fromEnum OTallonBackSw)]
                                    [IEdge noReq (fromEnum GreatTreeChamber)]
            ,Room (fromEnum OLifeGroveTunnel)[Edge noReq (fromEnum OGreatTreeHallTop)
                                    ,Edge (lifeGroveTunnel diff) (fromEnum OLifeGrove)]
                                    [IEdge (lifeGroveTunnelItem diff) (fromEnum LifeGroveTunnel)]
            ,Room (fromEnum OLifeGrove)[Edge morph (fromEnum OLifeGroveTunnel)]
                                    [IEdge noReq (fromEnum LifeGroveStart)
                                    ,IEdge (lgUnderWater diff) (fromEnum LifeGroveUnderwaterSpinner)]
            ,Room (fromEnum OTransportTunnelE)[Edge ice (fromEnum OTransporttoPhazonMinesEast)
                                    ,Edge ice (fromEnum OGreatTreeHall)] []
            ,Room (fromEnum OTransporttoPhazonMinesEast)[Edge ice (fromEnum OTransportTunnelE)
                                    ,Edge noReq (fromEnum MTransporttoTallonOverworldSouth)] []
            ,Room (fromEnum OTempleHall)[Edge noReq (fromEnum OLandingSite)
                                    ,Edge noReq (fromEnum OTempleSecurityStation)] []
            ,Room (fromEnum OTempleSecurityStation)[Edge missile (fromEnum OTempleLobby)
                                    ,Edge noReq (fromEnum OTempleHall)] []
            ,Room (fromEnum OTempleLobby)[Edge missile (fromEnum OTempleSecurityStation)
                                    ,Edge noReq (fromEnum OArtifactTemple)] []
            ,Room (fromEnum OArtifactTemple)[Edge noReq (fromEnum OTempleLobby)]
                                    [IEdge noReq (fromEnum ArtifactTemple)]
            ,Room (fromEnum OTallonBackSw)[Edge bombs (fromEnum OLifeGrove)
                                    ,Edge bombs (fromEnum OGreatTreeHallTop)
                                    ,Edge (wallcrawlIntoFrigate diff) (fromEnum ODeckBetaConduitHall)]
                                    [IEdge bombs (fromEnum LifeGroveUnderwaterSpinner)]
            ,Room (fromEnum OTallonFrontSw)[Edge bombs (fromEnum OFrigateCrashSite)
                                    ,Edge bombs (fromEnum OTallonCanyon)]
                                    [IEdge bombs (fromEnum ArborChamber)
                                    ,IEdge bombs (fromEnum RootCave)]

            -- Chozo Ruins Rooms
            ,Room (fromEnum RTransporttoTallonOverworldNorth)[Edge noReq (fromEnum OTransporttoChozoRuinsWest)
                                    ,Edge noReq (fromEnum RRuinsEntrance)] []
            ,Room (fromEnum RRuinsEntrance)[Edge noReq (fromEnum RTransporttoTallonOverworldNorth)
                                    ,Edge noReq (fromEnum RMainPlaza)] []
            ,Room (fromEnum RMainPlaza)[Edge noReq (fromEnum RRuinsEntrance)
                                    ,Edge morph (fromEnum RRuinedFountainAccess)
                                    ,Edge missile (fromEnum RRuinedShrineAccess)
                                    ,Edge noReq (fromEnum RNurseryAccess)
                                    ,Edge (mainPlazaGrappleLedge diff) (fromEnum RPistonTunnelInbounds)
                                    ,Edge (mainPlazaLedge diff) (fromEnum RMainPlazaLedge)
                                    ,Edge (mainPlazaSw diff) (fromEnum RChozoFrontSw)]
                                    [IEdge (mainPipe diff) (fromEnum MainPlazaHalfPipe)
                                    ,IEdge (mainPlazaGrappleLedge diff) (fromEnum MainPlazaGrappleLedge)
                                    ,IEdge supers (fromEnum MainPlazaTree)]
            --Created new room to hold the main plaza ledge item, and allow one-way traversal through Vault
            ,Room (fromEnum RMainPlazaLedge)[Edge noReq (fromEnum RMainPlaza)]
                                    [IEdge noReq (fromEnum MainPlazaLockedDoor)] 
            ,Room (fromEnum RPlazaAccess)[Edge noReq (fromEnum RVault)
                                    ,Edge noReq (fromEnum RMainPlazaLedge)] []
            ,Room (fromEnum RVault)[Edge noReq (fromEnum RPlazaAccess)
                                    ,Edge morph (fromEnum RVaultAccess)]
                                    [IEdge bombs (fromEnum Vault)]
            ,Room (fromEnum RVaultAccess)[Edge morph (fromEnum RVault)
                                    ,Edge noReq (fromEnum RTransporttoMagmoorCavernsNorth)] []
            ,Room (fromEnum RTransporttoMagmoorCavernsNorth)[Edge noReq (fromEnum RVaultAccess)
                                    ,Edge noReq (fromEnum CTransporttoChozoRuinsNorth)
                                    ,Edge (climbSunTower diff) (fromEnum RSunTower)
                                    ,Edge morph (fromEnum RTransportAccessNorth)] []
            ,Room (fromEnum RTransportAccessNorth)[Edge morph (fromEnum RTransporttoMagmoorCavernsNorth)
                                    ,Edge missile (fromEnum RHiveTotem)]
                                    [IEdge noReq (fromEnum TransportAccessNorth)]
            ,Room (fromEnum RHiveTotem)[Edge missile (fromEnum RTransportAccessNorth)
                                    ,Edge noReq (fromEnum RTotemAccess)]
                                    [IEdge noReq (fromEnum HiveTotem)]
            ,Room (fromEnum RTotemAccess)[Edge noReq (fromEnum RHiveTotem)
                                    ,Edge noReq (fromEnum RRuinedGallery)] []
            ,Room (fromEnum RRuinedGallery)[Edge noReq (fromEnum RTotemAccess)
                                    ,Edge missile (fromEnum RMapStation)
                                    ,Edge noReq (fromEnum RNorthAtrium)]
                                    [IEdge missile (fromEnum RuinedGalleryMissileWall)
                                    ,IEdge bombs (fromEnum RuinedGalleryTunnel)]
            ,Room (fromEnum RMapStation)[Edge missile (fromEnum RRuinedGallery)] []
            ,Room (fromEnum RNorthAtrium)[Edge noReq (fromEnum RRuinedGallery)
                                    ,Edge noReq (fromEnum RRuinedNursery)] []
            ,Room (fromEnum RRuinedNursery)[Edge noReq (fromEnum RNorthAtrium)
                                    ,Edge noReq (fromEnum RSaveStation1)
                                    ,Edge noReq (fromEnum REyonTunnel)]
                                    [IEdge bombs (fromEnum RuinedNursery)]
            ,Room (fromEnum RSaveStation1)[Edge noReq (fromEnum RRuinedNursery)] []
            ,Room (fromEnum REyonTunnel)[Edge noReq (fromEnum RRuinedNursery)
                                    ,Edge noReq (fromEnum RNurseryAccess)] []
            ,Room (fromEnum RNurseryAccess)[Edge noReq (fromEnum REyonTunnel)
                                    ,Edge noReq (fromEnum RMainPlaza)] []
            ,Room (fromEnum RRuinedShrineAccess)[Edge noReq (fromEnum RRuinedShrine)
                                    ,Edge missile (fromEnum RMainPlaza)] []
            ,Room (fromEnum RRuinedShrine)[Edge noReq (fromEnum RRuinedShrineAccess)
                                    ,Edge (tolAccess diff) (fromEnum RTowerofLightAccess)]
                                    [IEdge bombs (fromEnum RuinedShrineLowerTunnel)
                                    ,IEdge (rsHalf diff) (fromEnum RuinedShrineHalfPipe)
                                    ,IEdge noReq (fromEnum RuinedShrineBeetleBattle)]
            ,Room (fromEnum RTowerofLightAccess)[Edge wave (fromEnum RRuinedShrine)
                                    ,Edge wave (fromEnum RTowerofLight)] []
            ,Room (fromEnum RTowerofLight)[Edge wave (fromEnum RTowerofLightAccess)
                                    ,Edge (towerChamber diff) (fromEnum RTowerChamber)]
                                    [IEdge (towerOfLight diff) (fromEnum TowerofLight)]
            ,Room (fromEnum RTowerChamber)[Edge wave (fromEnum RTowerofLight)]
                                    [IEdge noReq (fromEnum TowerChamber)]
            ,Room (fromEnum RRuinedFountainAccess)[Edge noReq (fromEnum RRuinedFountainNonWarp)
                                    ,Edge morph (fromEnum RMainPlaza)] []
            -- The Ruined Fountain Warp puts you on top of the item and forces you to collect it
            ,Room (fromEnum RRuinedFountainNonWarp)[Edge (ruinedFountainItem diff) (fromEnum RRuinedFountain)
                                    ,Edge noReq (fromEnum RRuinedFountainAccess)
                                    ,Edge noReq (fromEnum RMeditationFountain)
                                    ,Edge noReq (fromEnum RArboretumAccess)] []
            ,Room (fromEnum RRuinedFountain)[Edge (leaveRuinedFountainItem diff) (fromEnum RRuinedFountainNonWarp)]
                                    [IEdge noReq (fromEnum RuinedFountain)]
            ,Room (fromEnum RMeditationFountain)[Edge noReq (fromEnum RRuinedFountainNonWarp)
                                    ,Edge noReq (fromEnum RMagmaPool)] []
            ,Room (fromEnum RMagmaPool)[Edge noReq (fromEnum RMeditationFountain)
                                    ,Edge (crossMagmaPool diff) (fromEnum RTrainingChamberAccess)]
                                    [IEdge (magmaPoolItem diff) (fromEnum MagmaPool)]
            ,Room (fromEnum RTrainingChamberAccess)[Edge (crossMagmaPool diff) (fromEnum RMagmaPool)
                                    ,Edge wave (fromEnum RTrainingChamber)]
                                    [IEdge wavePb (fromEnum MagmaPool)
                                    ,IEdge morph (fromEnum TrainingChamberAccess)]
            ,Room (fromEnum RTrainingChamber)[Edge wave (fromEnum RTowerofLightAccess)
                                    ,Edge (tcTunnel diff) (fromEnum RPistonTunnelInbounds)]
                                    [IEdge (tcItem diff) (fromEnum TrainingChamber)]
            ,Room (fromEnum RPistonTunnelInbounds)[Edge morph (fromEnum RMainPlaza)
                                    ,Edge blocked (fromEnum RTrainingChamber)] [] -- Since it is blocked initially, it's simpler to consider it one-way
            ,Room (fromEnum RPistonTunnel)[Edge morph (fromEnum RPistonTunnelInbounds) -- If you jump after being warped here, you go oob
                                    ,Edge (wallcrawl diff) (fromEnum RChozoFrontSw)] []
            ,Room (fromEnum RArboretumAccess)[Edge noReq (fromEnum RRuinedFountainNonWarp)
                                    ,Edge missile (fromEnum RArboretum)] []
            ,Room (fromEnum RArboretum)[Edge missile (fromEnum RArboretumAccess)
                                    ,Edge bombs (fromEnum RSunchamberLobby)
                                    ,Edge missile (fromEnum RGatheringHallAccess)] []
            ,Room (fromEnum RSunchamberLobby)[Edge missile (fromEnum RArboretum)
                                    ,Edge noReq (fromEnum RSunchamberAccess)] []
            ,Room (fromEnum RSunchamberAccess)[Edge noReq (fromEnum RSunchamberLobby)
                                    ,Edge noVines (fromEnum RSunchamber)] []
            ,Room (fromEnum RSunchamber)[Edge noVines (fromEnum RSunchamberAccess)
                                    ,Edge noReq (fromEnum RSunTowerAccess)]
                                    [IEdge bombs (fromEnum SunchamberFlaahgra)
                                    ,IEdge (sunchamberghost diff) (fromEnum SunchamberGhosts)]
            ,Room (fromEnum RSunTowerAccess)[Edge noReq (fromEnum RSunchamber)
                                    ,Edge noReq (fromEnum RSunTower)] []
            -- The spawn point is at the top of the room, so to approximate this, items are required to enter the room from the elevator
            ,Room (fromEnum RSunTower)[Edge noReq (fromEnum RSunTowerAccess)
                                    ,Edge noReq (fromEnum RTransporttoMagmoorCavernsNorth)] []
            ,Room (fromEnum RGatheringHallAccess)[Edge missile (fromEnum RArboretum)
                                    ,Edge noReq (fromEnum RGatheringHall)] []
            ,Room (fromEnum RGatheringHall)[Edge noReq (fromEnum RGatheringHallAccess)
                                    ,Edge missile (fromEnum RSaveStation2)
                                    ,Edge noReq (fromEnum RWateryHallAccess)
                                    ,Edge morph (fromEnum REastAtrium)
                                    ,Edge (gatheringHallSw diff) (fromEnum RChozoBackSw)]
                                    [IEdge bombs (fromEnum GatheringHall)]
            ,Room (fromEnum RWateryHallAccess)[Edge noReq (fromEnum RGatheringHall)
                                    ,Edge missile (fromEnum RWateryHall)]
                                    [IEdge missile (fromEnum WateryHallAccess)]
            ,Room (fromEnum RWateryHall)[Edge missile (fromEnum RWateryHallAccess)
                                    ,Edge (wateryHallTraverse diff) (fromEnum RDynamoAccess)
                                    ,Edge (wateryHallSw diff) (fromEnum RChozoBackSw)]
                                    [IEdge (wateryHallWater diff) (fromEnum WateryHallUnderwater)
                                    ,IEdge noReq (fromEnum WateryHallScanPuzzle)]
            ,Room (fromEnum RDynamoAccess)[Edge missile (fromEnum RWateryHall)
                                    ,Edge missile (fromEnum RDynamo)] []
            ,Room (fromEnum RDynamo)[Edge missile (fromEnum RDynamoAccess)]
                                    [IEdge missile (fromEnum DynamoLower)
                                    ,IEdge spider (fromEnum DynamoSpiderTrack)]
            ,Room (fromEnum RSaveStation2)[Edge missile (fromEnum RGatheringHall)] []
            ,Room (fromEnum REastAtrium)[Edge noReq (fromEnum RGatheringHall)
                                    ,Edge noReq (fromEnum REnergyCoreAccess)] []
            ,Room (fromEnum REnergyCoreAccess)[Edge noReq (fromEnum REastAtrium)
                                    ,Edge noReq (fromEnum REnergyCore)] []
            ,Room (fromEnum REnergyCore)[Edge noReq (fromEnum REnergyCoreAccess)
                                    ,Edge morph (fromEnum RBurnDomeAccess)
                                    ,Edge bombs (fromEnum RWestFurnaceAccess)] []
            ,Room (fromEnum RBurnDomeAccess)[Edge bombs (fromEnum REnergyCore)
                                    ,Edge morph (fromEnum RBurnDome)] []
            ,Room (fromEnum RBurnDome)[Edge noReq (fromEnum RBurnDomeAccess)]
                                    [IEdge bombs (fromEnum BurnDomeMissile)
                                    ,IEdge noReq (fromEnum BurnDomeIDrone)]
            ,Room (fromEnum RWestFurnaceAccess)[Edge noReq (fromEnum REnergyCore)
                                    ,Edge noReq (fromEnum RFurnaceFront)] []
            ,Room (fromEnum RFurnaceFront)[Edge noReq (fromEnum RWestFurnaceAccess)
                                    ,Edge (furnaceTraverse diff) (fromEnum RFurnace)]
                                    [IEdge bombs (fromEnum FurnaceInsideFurnace)]
            ,Room (fromEnum RFurnace)[Edge bombs (fromEnum RFurnaceFront)
                                    ,Edge morph (fromEnum RCrosswayAccessWest)
                                    ,Edge ice (fromEnum REastFurnaceAccess)]
                                    [IEdge (furnaceItem diff) (fromEnum FurnaceSpiderTracks)]
            ,Room (fromEnum REastFurnaceAccess)[Edge ice (fromEnum RFurnace)
                                    ,Edge ice (fromEnum RHalloftheElders)] []
            ,Room (fromEnum RCrosswayAccessWest)[Edge morph (fromEnum RFurnace)
                                    ,Edge wave (fromEnum RCrossway)] []
            ,Room (fromEnum RCrossway)[Edge noReq (fromEnum RCrosswayAccessWest)
                                    ,Edge (crosswayTraverse diff) (fromEnum RElderHallAccess)
                                    ,Edge ice (fromEnum RCrosswayAccessSouth)]
                                    [IEdge (crosswayItem diff) (fromEnum Crossway)]
            ,Room (fromEnum RElderHallAccess)[Edge missile (fromEnum RCrossway)
                                    ,Edge noReq (fromEnum RHalloftheElders)] []
            ,Room (fromEnum RCrosswayAccessSouth)[Edge ice (fromEnum RCrossway)
                                    ,Edge ice (fromEnum RHalloftheElders)] []
            ,Room (fromEnum RHalloftheElders)[Edge ice (fromEnum RCrosswayAccessSouth)
                                    ,Edge ice (fromEnum REastFurnaceAccess)
                                    ,Edge sjOrBombs (fromEnum RElderHallAccess)
                                    ,Edge (hoteWave diff) (fromEnum RReflectingPoolAccess)
                                    ,Edge (hotePlasma diff) (fromEnum RElderChamber)]
                                    [IEdge (hoteIce diff) (fromEnum HalloftheElders)]
            ,Room (fromEnum RElderChamber)[Edge (elderChamberExit diff) (fromEnum RHalloftheElders)]
                                    [IEdge noReq (fromEnum ElderChamber)]
            ,Room (fromEnum RReflectingPoolAccess)[Edge noReq (fromEnum RHalloftheElders)
                                    ,Edge noReq (fromEnum RReflectingPool)] []
            ,Room (fromEnum RReflectingPool)[Edge noReq (fromEnum RReflectingPoolAccess)
                                    ,Edge (reflectPoolSave diff) (fromEnum RSaveStation3)
                                    ,Edge (reflectPoolAntechamber diff) (fromEnum RAntechamber)
                                    ,Edge (reflectPoolIceDoor diff) (fromEnum RTransportAccessSouth)] []
            ,Room (fromEnum RAntechamber)[Edge ice (fromEnum RReflectingPool)]
                                    [IEdge noReq (fromEnum Antechamber)]
            ,Room (fromEnum RTransportAccessSouth)[Edge ice (fromEnum RReflectingPool)
                                    ,Edge noReq (fromEnum RTransporttoTallonOverworldSouth)] []
            ,Room (fromEnum RTransporttoTallonOverworldSouth)[Edge noReq (fromEnum RTransportAccessSouth)
                                    ,Edge noReq (fromEnum OTransporttoChozoRuinsSouth)] []
            ,Room (fromEnum RSaveStation3)[Edge missile (fromEnum RReflectingPool)
                                    ,Edge bombs (fromEnum RTransporttoTallonOverworldEast)] []
            ,Room (fromEnum RTransporttoTallonOverworldEast)[Edge bombs (fromEnum RSaveStation3)
                                    ,Edge noReq (fromEnum OTransporttoChozoRuinsEast)] []
            ,Room (fromEnum RChozoBackSw)[Edge bombs (fromEnum RReflectingPool)
                                    ,Edge (longWallcrawl diff) (fromEnum RChozoFrontSw)]
                                    [IEdge bombs (fromEnum HalloftheElders)
                                    ,IEdge (crosswayInfiniteSpeed diff) (fromEnum ElderChamber)]
            ,Room (fromEnum RChozoFrontSw)[] [IEdge bombs (fromEnum TrainingChamber)
                                    ,IEdge bombs (fromEnum MainPlazaGrappleLedge)
                                    ,IEdge bombs (fromEnum TrainingChamberAccess)
                                    ,IEdge bombs (fromEnum TowerofLight)]
            
            --Magmoor Caverns Rooms
            ,Room (fromEnum CTransporttoChozoRuinsNorth)[Edge noReq (fromEnum RTransporttoMagmoorCavernsNorth)
                                    ,Edge noReq (fromEnum CBurningTrail)] []
            ,Room (fromEnum CBurningTrail)[Edge noReq (fromEnum CTransporttoChozoRuinsNorth)
                                    ,Edge missile (fromEnum CSaveStationMagmoorA)
                                    ,Edge noReq (fromEnum CLakeTunnel)
                                    ,Edge (burningTrailSw diff) (fromEnum CMagmoorFrontSw)] []
            ,Room (fromEnum CSaveStationMagmoorA)[Edge missile (fromEnum CBurningTrail)] []
            ,Room (fromEnum CLakeTunnel)[Edge noReq (fromEnum CBurningTrail)
                                    ,Edge noReq (fromEnum CLavaLake)] []
            ,Room (fromEnum CLavaLake)[Edge noReq (fromEnum CLakeTunnel)
                                    ,Edge (lavaLakeTraversal diff) (fromEnum CPitTunnel)]
                                    [IEdge (lavaLakeItem diff) (fromEnum LavaLake)]
            ,Room (fromEnum CPitTunnel)[Edge (lavaLakeReverseTraversal diff) (fromEnum CLavaLake)
                                    ,Edge (pitTunnel diff) (fromEnum CTriclopsPit)] []
            ,Room (fromEnum CTriclopsPit)[Edge (pitTunnelReverse diff) (fromEnum CPitTunnel)
                                    ,Edge (storageCavern diff) (fromEnum CStorageCavern)
                                    ,Edge (heatResistOr8Etanks diff) (fromEnum CMonitorTunnel)] -- This has a high requirement to discourage this path to get to phendrana
                                    [IEdge (triclopsPitItem diff) (fromEnum TriclopsPit)]
            ,Room (fromEnum CStorageCavern)[Edge (vmr2Tank diff) (fromEnum CTriclopsPit)]
                                    [IEdge noReq (fromEnum StorageCavern)]
            ,Room (fromEnum CMonitorTunnel)[Edge (vmr2Tank diff) (fromEnum CTriclopsPit)
                                    ,Edge (vmr2Tank diff) (fromEnum CMonitorStation)] []
            ,Room (fromEnum CMonitorStation)[Edge (vmr4Tank diff) (fromEnum CMonitorTunnel) -- This requirement is excessive if warped to MonitorStation, going to storage cavern
                                    ,Edge (vmr3Tank diff) (fromEnum CShoreTunnel)
                                    ,Edge (toTransportTunnelA diff) (fromEnum CTransportTunnelA)
                                    ,Edge (monitorStationClimb diff) (fromEnum CWarriorShrine)] []
            ,Room (fromEnum CTransportTunnelA)[Edge bombs (fromEnum CMonitorStation)
                                    ,Edge noReq (fromEnum CTransporttoPhendranaDriftsNorth)]
                                    [IEdge bombs (fromEnum TransportTunnelA)]
            ,Room (fromEnum CTransporttoPhendranaDriftsNorth)[Edge noReq (fromEnum CTransportTunnelA)
                                    ,Edge noReq (fromEnum DTransporttoMagmoorCavernsWest)] []
            ,Room (fromEnum CWarriorShrine)[Edge (vmr2Tank diff) (fromEnum CMonitorStation)
                                    ,Edge (warriorShrineTunnel diff) (fromEnum CFieryShores)]
                                    [IEdge noReq (fromEnum WarriorShrine)
                                    ,IEdge (warriorShrineTunnel diff) (fromEnum FieryShoresWarriorShrineTunnel)]
            ,Room (fromEnum CShoreTunnel)[Edge (vmr2Tank diff) (fromEnum CMonitorStation)
                                    ,Edge (vmr2Tank diff) (fromEnum CFieryShores)]
                                    [IEdge pb (fromEnum ShoreTunnel)]
            ,Room (fromEnum CFieryShores)[Edge (vmr3Tank diff) (fromEnum CShoreTunnel)
                                    ,Edge (vmr1Tank diff) (fromEnum CTransportTunnelB)]
                                    [IEdge bombs (fromEnum FieryShoresMorphTrack)]
            ,Room (fromEnum CTransportTunnelB)[Edge (vmr4Tank diff) (fromEnum CFieryShores)
                                    ,Edge noReq (fromEnum CTransporttoTallonOverworldWest)] []
            ,Room (fromEnum CTransporttoTallonOverworldWest)[Edge (vmr4Tank diff) (fromEnum CTransportTunnelB)
                                    ,Edge noReq (fromEnum OTransporttoMagmoorCavernsEast)
                                    ,Edge (crossTft diff) (fromEnum CTwinFiresTunnel)] []
            ,Room (fromEnum CTwinFiresTunnel)[Edge (crossTftReverse diff) (fromEnum CTransporttoTallonOverworldWest)
                                    ,Edge noReq (fromEnum CTwinFires)] []
            ,Room (fromEnum CTwinFires)[Edge noReq (fromEnum CTwinFiresTunnel)
                                    ,Edge (crossTwinFires diff) (fromEnum CNorthCoreTunnel)] []
            ,Room (fromEnum CNorthCoreTunnel)[Edge (crossTwinFires diff) (fromEnum CTwinFires)
                                    ,Edge (crossNorthCoreTunnel diff) (fromEnum CGeothermalCore)] []
            ,Room (fromEnum CGeothermalCore)[Edge (crossNorthCoreTunnel diff) (fromEnum CNorthCoreTunnel)
                                    ,Edge noReq (fromEnum CSouthCoreTunnel)
                                    ,Edge (geoCore diff) (fromEnum CPlasmaProcessing)] []
            ,Room (fromEnum CPlasmaProcessing)[Edge plasma (fromEnum CGeothermalCore)]
                                    [IEdge noReq (fromEnum PlasmaProcessing)]
            ,Room (fromEnum CSouthCoreTunnel)[Edge wave (fromEnum CGeothermalCore)
                                    ,Edge wave (fromEnum CMagmoorWorkstation)] []
            ,Room (fromEnum CMagmoorWorkstation)[Edge noReq (fromEnum CSouthCoreTunnel)
                                    ,Edge sjOrBombs (fromEnum CWorkstationTunnel)
                                    ,Edge (workstationWaveDoor diff) (fromEnum CTransportTunnelC)
                                    ,Edge (workstationSw diff) (fromEnum CMagmoorBackSw)]
                                    [IEdge (workstationItem diff) (fromEnum MagmoorWorkstation)]
            ,Room (fromEnum CTransportTunnelC)[Edge wave (fromEnum CMagmoorWorkstation)
                                    ,Edge wave (fromEnum CTransporttoPhendranaDriftsSouth)] []
            ,Room (fromEnum CTransporttoPhendranaDriftsSouth)[Edge wave (fromEnum CTransportTunnelC)
                                    ,Edge missile (fromEnum CSaveStationMagmoorB)
                                    ,Edge noReq (fromEnum DTransporttoMagmoorCavernsSouth)] []
            ,Room (fromEnum CSaveStationMagmoorB)[Edge missile (fromEnum CTransporttoPhendranaDriftsSouth)] []
            ,Room (fromEnum CWorkstationTunnel)[Edge noReq (fromEnum CMagmoorWorkstation)
                                    ,Edge (workstationTunnel diff) (fromEnum CTransporttoPhazonMinesWest)] []
            ,Room (fromEnum CTransporttoPhazonMinesWest)[Edge (workstationTunnel diff) (fromEnum CWorkstationTunnel)
                                    ,Edge noReq (fromEnum MTransporttoMagmoorCavernsSouth)] []
            ,Room (fromEnum CMagmoorBackSw)[Edge bombs (fromEnum CTransporttoPhazonMinesWest)
                                    ,Edge (longWallcrawl diff) (fromEnum CMagmoorFrontSw)]
                                    [IEdge bombs (fromEnum PlasmaProcessing)]
            ,Room (fromEnum CMagmoorFrontSw)[Edge (magmoorFrontWallcrawl diff) (fromEnum CMagmoorBackSw)] []

            -- Phendrana Drifts Rooms
            ,Room (fromEnum DTransporttoMagmoorCavernsWest)[Edge noReq (fromEnum CTransporttoPhendranaDriftsNorth)
                                    ,Edge noReq (fromEnum DShorelineEntrance)] []
            ,Room (fromEnum DShorelineEntrance)[Edge noReq (fromEnum DTransporttoMagmoorCavernsWest)
                                    ,Edge (iceBarrier diff) (fromEnum DPhendranaShorelines)] []
            ,Room (fromEnum DPhendranaShorelines)[Edge (iceBarrier diff) (fromEnum DShorelineEntrance)
                                    ,Edge noReq (fromEnum DSaveStationB)
                                    ,Edge noReq (fromEnum DIceRuinsAccess)
                                    ,Edge (climbShorelines diff)  (fromEnum DPhendranaShorelinesUpper)
                                    ,Edge (climbShorelines diff) (fromEnum DTempleEntryway)]
                                    [IEdge plasma (fromEnum PhendranaShorelinesBehindIce)
                                    ,IEdge (shorelinesTower diff) (fromEnum PhendranaShorelinesSpiderTrack)]
            ,Room (fromEnum DPhendranaShorelinesUpper)[Edge noReq (fromEnum DPlazaWalkway)
                                    ,Edge noReq (fromEnum DRuinsEntryway)
                                    ,Edge noReq (fromEnum DPhendranaShorelines)] []
            ,Room (fromEnum DSaveStationB)[Edge noReq (fromEnum DPhendranaShorelines)] []
            ,Room (fromEnum DTempleEntryway)[Edge noReq (fromEnum DPhendranaShorelines)
                                    ,Edge (iceBarrier diff) (fromEnum DChozoIceTemple)] []
            ,Room (fromEnum DChozoIceTemple)[Edge (iceBarrier diff) (fromEnum DTempleEntryway)
                                    ,Edge (iceTempleClimb diff) (fromEnum DChapelTunnel)]
                                    [IEdge (iceTempleClimb diff) (fromEnum ChozoIceTempleTrigger)
                                    ,IEdge (iceTempleItem diff) (fromEnum ChozoIceTemple)]
            ,Room (fromEnum DChapelTunnel)[Edge chozoIceTempleBarrier (fromEnum DChozoIceTemple)
                                    ,Edge noReq (fromEnum DChapeloftheElders)] []-- Warp point is near Chapel of the Elders
            ,Room (fromEnum DChapeloftheElders)[Edge wave (fromEnum DChapelTunnel)]
                                    [IEdge missile (fromEnum ChapeloftheElders)]
            ,Room (fromEnum DIceRuinsAccess)[Edge noReq (fromEnum DPhendranaShorelines)
                                    ,Edge (iceBarrier diff) (fromEnum DIceRuinsEast)] []
            ,Room (fromEnum DIceRuinsEast)[Edge (iceBarrier diff) (fromEnum DIceRuinsAccess)
                                    ,Edge noReq (fromEnum DPlazaWalkway)]
                                    [IEdge (ireSpiderTrack diff) (fromEnum IceRuinsEastSpiderTrack)
                                    ,IEdge plasma (fromEnum IceRuinsEastBehindIce)]
            ,Room (fromEnum DPlazaWalkway)[Edge noReq (fromEnum DIceRuinsEast)
                                    ,Edge noReq (fromEnum DPhendranaShorelinesUpper)] []
            ,Room (fromEnum DRuinsEntryway)[Edge noReq (fromEnum DPhendranaShorelinesUpper)
                                    ,Edge noReq (fromEnum DIceRuinsWest)] []
            ,Room (fromEnum DIceRuinsWest)[Edge noReq (fromEnum DRuinsEntryway)
                                    ,Edge missile (fromEnum DCanyonEntryway)
                                    ,Edge (irwSw diff) (fromEnum DPhendranaFrontSw)
                                    ,Edge (irwDoor diff) (fromEnum DCourtyardEntryway)]
                                    [IEdge (irwItem diff) (fromEnum IceRuinsWest)]
            ,Room (fromEnum DCanyonEntryway)[Edge noReq (fromEnum DIceRuinsWest)
                                    ,Edge noReq (fromEnum DPhendranaCanyon)] []
            ,Room (fromEnum DPhendranaCanyon)[Edge noReq (fromEnum DCanyonEntryway)]
                                    [IEdge noReq (fromEnum PhendranaCanyon)]
            ,Room (fromEnum DCourtyardEntryway)[Edge noReq (fromEnum DIceRuinsWest)
                                    ,Edge (ruinedCourtyardClimb diff) (fromEnum DRuinedCourtyard)] []-- Ruined courtyard spawn is at the top of the room
            ,Room (fromEnum DRuinedCourtyard)[Edge noReq (fromEnum DCourtyardEntryway)
                                    ,Edge (ruinedCourtyardSave diff) (fromEnum DSaveStationA)
                                    ,Edge wave (fromEnum DSpecimenStorage)
                                    ,Edge (ruinedCourtyardConduit diff) (fromEnum DQuarantineAccess) 
                                    ,Edge (ruinedCourtyardSw diff) (fromEnum DPhendranaFrontSw)]
                                    [IEdge morph (fromEnum RuinedCourtyard)]
            ,Room (fromEnum DSaveStationA)[Edge missile (fromEnum DCourtyardEntryway) -- If you fall
                                    ,Edge (ruinedCourtyardSave diff) (fromEnum DRuinedCourtyard)] -- If can make it to the spawn point
                                    [IEdge morph (fromEnum RuinedCourtyard)]-- You can grab the item by falling here, without reaching the warp
            ,Room (fromEnum DQuarantineAccess)[Edge noReq (fromEnum DRuinedCourtyard)
                                    ,Edge noReq (fromEnum DNorthQuarantineTunnel)] []
            ,Room (fromEnum DNorthQuarantineTunnel)[Edge wave (fromEnum DQuarantineAccess)
                                    ,Edge (quarantineTunnel diff) (fromEnum DQuarantineCave)] []
            ,Room (fromEnum DQuarantineCave)[Edge (quarantineTunnel diff) (fromEnum DNorthQuarantineTunnel)
                                    ,Edge (climbQuarantineCaveBack diff) (fromEnum DQuarantineCaveBack)]
                                    [IEdge noReq (fromEnum QuarantineCave)]
            -- Added a new "room" representing the other door in quarantine cave
            ,Room (fromEnum DQuarantineCaveBack)[Edge (quarantineMonitor diff) (fromEnum DQuarantineMonitor)
                                    ,Edge (quarantineTunnel diff) (fromEnum DSouthQuarantineTunnel)
                                    ,Edge (climbQuarantineCaveEntrance diff) (fromEnum DQuarantineCave)]
                                    [IEdge noReq (fromEnum QuarantineCave) -- Can drop into thardus fight
                                    ]
            ,Room (fromEnum DQuarantineMonitor)[Edge (climbQuarantineCaveBack diff) (fromEnum DQuarantineCaveBack)
                                    ,Edge (climbQuarantineCaveEntrance diff) (fromEnum DQuarantineCave)]
                                    [IEdge noReq (fromEnum QuarantineCave) -- Can drop into thardus fight
                                    ,IEdge noReq (fromEnum QuarantineMonitor)]
            ,Room (fromEnum DSouthQuarantineTunnel)[Edge (quarantineTunnel diff) (fromEnum DQuarantineCaveBack)
                                    ,Edge wave (fromEnum DTransporttoMagmoorCavernsSouth)] []
            ,Room (fromEnum DTransporttoMagmoorCavernsSouth)[Edge wave (fromEnum DSouthQuarantineTunnel)
                                    ,Edge noReq (fromEnum CTransporttoPhendranaDriftsSouth)
                                    ,Edge (phenElevatorClimb diff) (fromEnum DTransportAccess)] []
            ,Room (fromEnum DTransportAccess)[Edge ice (fromEnum DTransporttoMagmoorCavernsSouth)
                                    ,Edge wave (fromEnum DFrozenPike)]
                                    [IEdge plasma (fromEnum TransportAccess)]
            ,Room (fromEnum DSpecimenStorage)[Edge wave (fromEnum DRuinedCourtyard)
                                    ,Edge wave (fromEnum DResearchEntrance)] []
            ,Room (fromEnum DResearchEntrance)[Edge wave (fromEnum DSpecimenStorage)
                                    ,Edge noReq (fromEnum DMapStation)
                                    ,Edge wave (fromEnum DHydraLabEntryway)] []
            ,Room (fromEnum DMapStation)[Edge noReq (fromEnum DResearchEntrance)] []
            ,Room (fromEnum DHydraLabEntryway)[Edge wave (fromEnum DResearchEntrance)
                                    ,Edge wave (fromEnum DResearchLabHydra)] []
            ,Room (fromEnum DResearchLabHydra)[Edge wave (fromEnum DHydraLabEntryway)
                                    ,Edge noReq (fromEnum DResearchLabHydraBack)]
                                    [IEdge noReq (fromEnum ResearchLabHydraTrigger)]
            ,Room (fromEnum DResearchLabHydraBack)[Edge researchLabHydraBarrier (fromEnum DResearchLabHydra)
                                    ,Edge wave (fromEnum DObservatoryAccess)]
                                    [IEdge supers (fromEnum ResearchLabHydra)]
            ,Room (fromEnum DObservatoryAccess)[Edge wave (fromEnum DResearchLabHydra)
                                    ,Edge wave (fromEnum DObservatory)] []
            ,Room (fromEnum DObservatory)[Edge wave (fromEnum DObservatoryAccess)
                                    ,Edge (observatoryClimb diff) (fromEnum DObservatoryTop)] []
            ,Room (fromEnum DObservatoryTop)[Edge (observatorySave diff) (fromEnum DSaveStationD)
                                    ,Edge wave (fromEnum DWestTowerEntrance)
                                    ,Edge noReq (fromEnum DObservatory)]
                                    [IEdge (observatoryItem diff) (fromEnum Observatory)]
            ,Room (fromEnum DSaveStationD)[Edge (observatorySave diff) (fromEnum DObservatoryTop)] []
            ,Room (fromEnum DWestTowerEntrance)[Edge wave (fromEnum DObservatoryTop)
                                    ,Edge missile (fromEnum DWestTower)] []
            ,Room (fromEnum DWestTower)[Edge missile (fromEnum DWestTowerEntrance)
                                    ,Edge wave (fromEnum DControlTower)] []
            ,Room (fromEnum DControlTower)[Edge wave (fromEnum DWestTower)
                                    ,Edge wave (fromEnum DEastTower)]
                                    [IEdge (controlTowerItem diff) (fromEnum ControlTower)]
            ,Room (fromEnum DEastTower)[Edge wave (fromEnum DControlTower)
                                    ,Edge wave (fromEnum DAetherLabEntryway)] []
            ,Room (fromEnum DAetherLabEntryway)[Edge wave (fromEnum DEastTower)
                                    ,Edge wave (fromEnum DResearchLabAether)] []
            ,Room (fromEnum DResearchLabAether)[Edge wave (fromEnum DAetherLabEntryway)
                                    ,Edge wave (fromEnum DResearchCoreAccess)]
                                    [IEdge missile (fromEnum ResearchLabAetherTank)
                                    ,IEdge (rlaTrack diff) (fromEnum ResearchLabAetherMorphTrack)]
            ,Room (fromEnum DResearchCoreAccess)[Edge wave (fromEnum DResearchLabAether)
                                    ,Edge wave (fromEnum DResearchCore)] []
            ,Room (fromEnum DResearchCore)[Edge wave (fromEnum DResearchCoreAccess)
                                    ,Edge ice (fromEnum DPikeAccess)]
                                    [IEdge noReq (fromEnum ResearchCore)]
            ,Room (fromEnum DPikeAccess)[Edge ice (fromEnum DResearchCore)
                                    ,Edge wave (fromEnum DFrozenPike)] []
            ,Room (fromEnum DFrozenPike)[Edge (frozenPikeClimb diff) (fromEnum DTransportAccess)
                                    ,Edge wave (fromEnum DPikeAccess)
                                    ,Edge wave (fromEnum DFrostCaveAccess)
                                    ,Edge (frozenPikeBottom diff) (fromEnum DHunterCaveAccess)] []
            ,Room (fromEnum DFrostCaveAccess)[Edge wave (fromEnum DFrozenPike)
                                    ,Edge (frostCaveAccess diff) (fromEnum DFrostCave)] []
            ,Room (fromEnum DFrostCave)[Edge (frostCaveAccess diff) (fromEnum DFrostCaveAccess)
                                    ,Edge (frostCaveDoor diff) (fromEnum DSaveStationC)
                                    ,Edge (frostCaveToTunnel diff) (fromEnum DUpperEdgeTunnel)
                                    ,Edge (frostCaveSw diff) (fromEnum DPhendranaBackSw)]
                                    [IEdge (frostCaveItem diff) (fromEnum FrostCave)]
            ,Room (fromEnum DSaveStationC)[Edge (frostCaveDoor diff) (fromEnum DFrostCave)] []
            ,Room (fromEnum DUpperEdgeTunnel)[Edge (frostCaveAccess diff) (fromEnum DFrostCave)
                                    ,Edge wave (fromEnum DPhendranasEdge)] []
            ,Room (fromEnum DPhendranasEdge)[Edge wave (fromEnum DUpperEdgeTunnel)
                                    ,Edge (toStorageCave diff) (fromEnum DStorageCave)
                                    ,Edge (toSecurityCave diff) (fromEnum DSecurityCave)
                                    ,Edge noReq (fromEnum DLowerEdgeTunnel)] []
            ,Room (fromEnum DStorageCave)[Edge (fromStorageCave diff) (fromEnum DPhendranasEdge)]
                                    [IEdge noReq (fromEnum StorageCave)]
            ,Room (fromEnum DSecurityCave)[Edge morph (fromEnum DPhendranasEdge)]
                                    [IEdge noReq (fromEnum SecurityCave)]
            ,Room (fromEnum DLowerEdgeTunnel)[Edge (phenEdgeLower diff) (fromEnum DPhendranasEdge)
                                    ,Edge wave (fromEnum DHunterCave)] []
            ,Room (fromEnum DHunterCave)[Edge wave (fromEnum DLowerEdgeTunnel)
                                    ,Edge (hunterCaveLower diff) (fromEnum DLakeTunnel)
                                    ,Edge (hunterCaveUpper diff) (fromEnum DHunterCaveFar)] []
            ,Room (fromEnum DHunterCaveFar)[Edge sjOrBombs (fromEnum DHunterCave)
                                    ,Edge wave (fromEnum DChamberAccess)
                                    ,Edge wave (fromEnum DHunterCaveAccess)] []
            ,Room (fromEnum DLakeTunnel)[Edge (hunterCaveClimb diff) (fromEnum DHunterCave)
                                    ,Edge wave (fromEnum DGravityChamber)] []
            ,Room (fromEnum DGravityChamber)[Edge (gravityChamberToLakeTunnel diff) (fromEnum DLakeTunnel)
                                    ,Edge (climbGravityChamber diff) (fromEnum DGravityChamberTop)]
                                    [IEdge noReq (fromEnum GravityChamberUnderwater)]
            ,Room (fromEnum DGravityChamberTop)[Edge noReq (fromEnum DGravityChamber)
                                    ,Edge wave (fromEnum DChamberAccess)]
                                    [IEdge (gravLedge diff) (fromEnum GravityChamberGrappleLedge)]
            ,Room (fromEnum DChamberAccess)[Edge wave (fromEnum DGravityChamberTop)
                                    ,Edge wave (fromEnum DHunterCaveFar)] []
            ,Room (fromEnum DHunterCaveAccess)[Edge wave (fromEnum DHunterCaveFar)
                                    ,Edge (frozenPikeBottom diff) (fromEnum DFrozenPike)] []
            ,Room (fromEnum DPhendranaFrontSw)[Edge bombs (fromEnum DRuinedCourtyard)
                                    ,Edge (longWallcrawl diff) (fromEnum DPhendranaBackSw)]
                                    [IEdge bombs (fromEnum QuarantineMonitor)
                                    ,IEdge bombs (fromEnum QuarantineCave)]
            ,Room (fromEnum DPhendranaBackSw)[Edge (longWallcrawl diff) (fromEnum DPhendranaFrontSw)
                                    ,Edge bombs (fromEnum DFrostCave)
                                    ,Edge bombs (fromEnum DGravityChamber)
                                    ,Edge bombs (fromEnum DTransportAccess)
                                    ,Edge bombs (fromEnum DFrozenPike)]
                                    [IEdge bombs (fromEnum GravityChamberGrappleLedge)
                                    ,IEdge (transportAccessItemOob diff) (fromEnum TransportAccess)
                                    ,IEdge bombs (fromEnum SecurityCave)
                                    ,IEdge bombs (fromEnum StorageCave)]

            -- Phazon Mines Rooms
            ,Room (fromEnum MTransporttoTallonOverworldSouth)[Edge noReq (fromEnum OTransporttoPhazonMinesEast)
                                    ,Edge wave (fromEnum MQuarryAccess)] []
            ,Room (fromEnum MQuarryAccess)[Edge wave (fromEnum MTransporttoTallonOverworldSouth)
                                    ,Edge wave (fromEnum MMainQuarry)] []
            ,Room (fromEnum MMainQuarry)[Edge wave (fromEnum MQuarryAccess)
                                    ,Edge (quarrySave diff) (fromEnum MSaveStationMinesA)
                                    ,Edge (reachWasteDisposal diff) (fromEnum MWasteDisposal)
                                    ,Edge ice (fromEnum MSecurityAccessA)]
                                    [IEdge noReq (fromEnum MainQuarryBarrierTrigger)
                                    ,IEdge (quarrySave diff) (fromEnum MainQuarrySaveTrigger)
                                    ,IEdge (quarryItem diff) (fromEnum MainQuarry)]
            ,Room (fromEnum MSaveStationMinesA)[Edge mainQuarryBarrierWave (fromEnum MMainQuarry)] []
            ,Room (fromEnum MSecurityAccessA)[Edge mainQuarryBarrierIce (fromEnum MMainQuarry)
                                    ,Edge ice (fromEnum MMineSecurityStation)]
                                    [IEdge pb (fromEnum SecurityAccessA)]
            ,Room (fromEnum MMineSecurityStation)[Edge waveIce (fromEnum MSecurityAccessA)
                                    ,Edge (toStorageDepotA diff) (fromEnum MStorageDepotA)
                                    ,Edge wave (fromEnum MSecurityAccessB)]
                                    [IEdge pb (fromEnum StorageDepotATrigger)]
            ,Room (fromEnum MStorageDepotA)[Edge (storageDepotABarrier diff) (fromEnum MMineSecurityStation)]
                                    [IEdge noReq (fromEnum StorageDepotA)]
            ,Room (fromEnum MSecurityAccessB)[Edge wave (fromEnum MMineSecurityStation)
                                    ,Edge (securityAccessBSw diff) (fromEnum MMinesFrontSw)
                                    ,Edge ice (fromEnum MEliteResearch)] []
            ,Room (fromEnum MEliteResearch)[Edge ice (fromEnum MSecurityAccessB)
                                    ,Edge (eliteResearchDoor diff) (fromEnum MResearchAccess)]
                                    [IEdge (eliteResearchTopItem diff) (fromEnum EliteResearchLaser)
                                    ,IEdge (eliteResearchPirate diff) (fromEnum EliteResearchPhazonElite)]
            -- Currently require boosting through wall even if you can laser it
            ,Room (fromEnum MResearchAccess)[Edge (shaftClimb2 diff) (fromEnum MEliteResearch)
                                    ,Edge ice (fromEnum MOreProcessingBottom)] []
            ,Room (fromEnum MOreProcessingBottom)[Edge ice (fromEnum MResearchAccess)
                                    ,Edge (oreProcessingClimb diff) (fromEnum MElevatorAccessA)
                                    ,Edge (oreProcessingTop diff) (fromEnum MWasteDisposal)
                                    ,Edge (oreProcessingTop diff) (fromEnum MStorageDepotB)] []
            -- Spawn point is next to the pb rocks
            ,Room (fromEnum MOreProcessing)[Edge noReq (fromEnum MOreProcessingBottom)
                                    ,Edge (dashFromPbRocks diff) (fromEnum MElevatorAccessA)
                                    ,Edge (oreProcessingTopFromRocks diff) (fromEnum MOreProcessingTop)] []
            -- This fake room is considered to be at the top on the waste disposal side
            ,Room (fromEnum MOreProcessingTop)[Edge noReq (fromEnum MOreProcessing)
                                    ,Edge ice (fromEnum MWasteDisposal)
                                    ,Edge (oreProcessingCrossTop diff) (fromEnum MStorageDepotB)] []
            ,Room (fromEnum MWasteDisposal)[Edge ice (fromEnum MOreProcessingTop)
                                    ,Edge (wasteDisposalTraversal diff) (fromEnum MMainQuarry)] []
            ,Room (fromEnum MStorageDepotB)[Edge (oreProcessingCrossTop diff) (fromEnum MOreProcessingTop)
                                    ,Edge ice (fromEnum MOreProcessing)]
                                    [IEdge noReq (fromEnum StorageDepotB)]
            ,Room (fromEnum MElevatorAccessA)[Edge ice (fromEnum MOreProcessing)
                                    ,Edge (oreProcessingTopFromEaa diff) (fromEnum MOreProcessingTop)
                                    ,Edge ice (fromEnum MElevatorA)] []
            ,Room (fromEnum MElevatorA)[Edge (shaftClimb1 diff) (fromEnum MElevatorAccessA)
                                    ,Edge ice (fromEnum MEliteControlAccess)] []
            ,Room (fromEnum MEliteControlAccess)[Edge ice (fromEnum MElevatorA)
                                    ,Edge wave (fromEnum MEliteControl)]
                                    [IEdge (ecaItem diff) (fromEnum EliteControlAccess)]
            ,Room (fromEnum MEliteControl)[Edge wave (fromEnum MEliteControlAccess)
                                    ,Edge ice (fromEnum MMaintenanceTunnel)
                                    ,Edge ice (fromEnum MVentilationShaft)]
                                    [IEdge noReq (fromEnum EliteControlTrigger)]
            ,Room (fromEnum MMaintenanceTunnel)[Edge ice (fromEnum MEliteControl)
                                    ,Edge (maintTunnel diff) (fromEnum MPhazonProcessingCenter)] []
            ,Room (fromEnum MPhazonProcessingCenter)[Edge (maintTunnel diff) (fromEnum MMaintenanceTunnel)
                                    ,Edge blocked (fromEnum MProcessingCenterAccess) -- This barrier is considered to be one-way
                                    ,Edge (ppcClimb diff) (fromEnum MTransportAccess)]
                                    [IEdge pb (fromEnum PhazonProcessingCenter)]
            ,Room (fromEnum MTransportAccess)[Edge ice (fromEnum MPhazonProcessingCenter)
                                    ,Edge (toMinesElevator diff) (fromEnum MTransporttoMagmoorCavernsSouth)] []
            ,Room (fromEnum MTransporttoMagmoorCavernsSouth)[Edge (toMinesElevator diff) (fromEnum MTransportAccess)
                                    ,Edge noReq (fromEnum CTransporttoPhazonMinesWest)] []
            -- Warp is at the top
            ,Room (fromEnum MVentilationShaft)[Edge eliteControlBarrier (fromEnum MEliteControl)
                                    ,Edge ice (fromEnum MOmegaResearch)]
                                    [IEdge pb (fromEnum VentilationShaft)]
            ,Room (fromEnum MOmegaResearch)[Edge ice (fromEnum MVentilationShaft)
                                    ,Edge (maintTunnel diff) (fromEnum MMapStationMines)
                                    ,Edge ice (fromEnum MDynamoAccess)] []
            ,Room (fromEnum MMapStationMines)[Edge (maintTunnel diff) (fromEnum MOmegaResearch)] []
            ,Room (fromEnum MDynamoAccess)[Edge ice (fromEnum MOmegaResearch)
                                    ,Edge ice (fromEnum MCentralDynamo)] []
            -- Warp is the top, but treating as the bottom. It's slightly inaccurate
            ,Room (fromEnum MCentralDynamo)[Edge (centralDynamoClimb diff) (fromEnum MDynamoAccess)
                                    ,Edge ice (fromEnum MSaveStationMinesB)
                                    ,Edge (maintTunnel diff) (fromEnum MQuarantineAccessA)]
                                    [IEdge morph (fromEnum CentralDynamo)]
            ,Room (fromEnum MSaveStationMinesB)[Edge ice (fromEnum MCentralDynamo)] []
            ,Room (fromEnum MQuarantineAccessA)[Edge (maintTunnel diff) (fromEnum MCentralDynamo)
                                    ,Edge wave (fromEnum MMetroidQuarantineA)] []
            ,Room (fromEnum MMetroidQuarantineA)[Edge wave (fromEnum MQuarantineAccessA)
                                    ,Edge noReq (fromEnum MMetroidQuarantineABack)]
                                    [IEdge noReq (fromEnum MetroidQuarantineATrigger)]
            ,Room (fromEnum MMetroidQuarantineABack)[Edge mqaBarrier (fromEnum MMetroidQuarantineA)
                                    ,Edge (mqaTraversal diff) (fromEnum MElevatorAccessB)]
                                    [IEdge (mqaItem diff) (fromEnum MetroidQuarantineA)]
            ,Room (fromEnum MElevatorAccessB)[Edge ice (fromEnum MMetroidQuarantineABack)
                                    ,Edge plasma (fromEnum MElevatorB)] []
            ,Room (fromEnum MElevatorB)[Edge plasma (fromEnum MFungalHallAccess)
                                    ,Edge plasma (fromEnum MElevatorAccessB)] []
            ,Room (fromEnum MFungalHallAccess)[Edge plasma (fromEnum MElevatorB)
                                    ,Edge plasma (fromEnum MFungalHallA)]
                                    [IEdge morph (fromEnum FungalHallAccess)]
            ,Room (fromEnum MFungalHallA)[Edge (climbFungalHallAccess diff) (fromEnum MFungalHallAccess)
                                    ,Edge (fungalHallATraversal diff) (fromEnum MPhazonMiningTunnel)] []
            ,Room (fromEnum MPhazonMiningTunnel)[Edge plasma (fromEnum MFungalHallA)
                                    ,Edge (miningTunnelTraversal diff) (fromEnum MFungalHallB)]
                                    [IEdge (miningTunnelItem diff) (fromEnum PhazonMiningTunnel)]
            ,Room (fromEnum MFungalHallB)[Edge (miningTunnelTraversal diff) (fromEnum MPhazonMiningTunnel)
                                    ,Edge (fungalHallBTraversal diff) (fromEnum MMissileStationMinesInbounds)
                                    ,Edge (fungalHallBTraversal diff) (fromEnum MQuarantineAccessB)]
                                    [IEdge bombs (fromEnum FungalHallB)]
            ,Room (fromEnum MMissileStationMinesInbounds)[Edge plasma (fromEnum MFungalHallB)] []
            ,Room (fromEnum MMissileStationMines)[Edge morph (fromEnum MMissileStationMinesInbounds) -- You get warped out of bounds and need morph
                                    ,Edge (wallcrawl diff) (fromEnum MMinesBackSw)] []
            ,Room (fromEnum MQuarantineAccessB)[Edge plasma (fromEnum MFungalHallB)
                                    ,Edge (quarantineAccessBTraversal diff) (fromEnum MMetroidQuarantineB)] []
            ,Room (fromEnum MMetroidQuarantineB)[Edge (quarantineAccessBTraversal diff) (fromEnum MQuarantineAccessB)
                                    ,Edge (mqbTraversal diff) (fromEnum MMetroidQuarantineBBack)]
                                    [IEdge noReq (fromEnum MetroidQuarantineBTrigger)]
            ,Room (fromEnum MMetroidQuarantineBBack)[Edge mqbBarrier (fromEnum MMetroidQuarantineB)
                                    ,Edge plasma (fromEnum MSaveStationMinesC)
                                    ,Edge (mqbBackClimb diff) (fromEnum MEliteQuartersAccess)
                                    ,Edge (mqbSw diff) (fromEnum MMinesBackSw)]
                                    [IEdge supers (fromEnum MetroidQuarantineB)]
            ,Room (fromEnum MSaveStationMinesC)[Edge plasma (fromEnum MMetroidQuarantineBBack)] []
            ,Room (fromEnum MEliteQuartersAccess)[Edge plasma (fromEnum MMetroidQuarantineBBack)
                                    ,Edge plasma (fromEnum MEliteQuarters)]
                                    [IEdge plasma (fromEnum OmegaPirateEntranceTrigger)]
            ,Room (fromEnum MEliteQuarters)[Edge (eliteQuartersExit diff) (fromEnum MEliteQuartersAccess)
                                    ,Edge (eliteQuartersTop diff) (fromEnum MProcessingCenterAccess)]
                                    [IEdge (eliteQuarters diff) (fromEnum EliteQuarters)]
            ,Room (fromEnum MProcessingCenterAccess)[Edge plasma (fromEnum MEliteQuarters)
                                    ,Edge (ppcBottomClimb diff) (fromEnum MPhazonProcessingCenter)]
                                    [IEdge noReq (fromEnum ProcessingCenterAccess)]
            ,Room (fromEnum MMinesFrontSw)[Edge ice (fromEnum MMainQuarry)]
                                    [IEdge bombs (fromEnum StorageDepotA)
                                    ,IEdge ice (fromEnum SecurityAccessA)]
            ,Room (fromEnum MMinesBackSw)[Edge bombs (fromEnum MFungalHallB)
                                    ,Edge bombs (fromEnum MPhazonProcessingCenter)
                                    ,Edge bombs (fromEnum MMetroidQuarantineB)]
                                    [IEdge (longWallcrawl diff) (fromEnum FungalHallAccess)]
            ]