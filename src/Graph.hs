module Graph where

import Data.Map (Map)
import qualified Data.Map as Map

import Node
import Predicates
import Util

{-- 
A graph is represented as a collection of nodes. Specifically, it's a Map of the Node ID to the Node itself.

Note that Rooms and Items are both nodes in this graph, with edges representing the requirements (specified as a function) to move from node to node.
Items are nodes that force you to traverse to their designated "warp" room.

We calculate if a seed is completable by constructing a graph representing it and calling isCompletable.
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

buildMap :: (Ord b) => (a -> b) -> [a] -> Map b a
buildMap idFunc vals = Map.fromList (map (\x -> (idFunc x, x)) vals)

replaceElevators :: Map RoomId Room -> [(RoomId, RoomId)] -> Map RoomId Room
replaceElevators graph [] = graph
replaceElevators graph ((a, b):rest) =
    let oldNode = getVal (Map.lookup a graph) "Missing room"
        newNode = replaceEdgeRoom oldNode a b
     in replaceElevators (Map.insert a newNode graph) rest

replaceEdgeRoom :: Room -> RoomId -> RoomId -> Room
replaceEdgeRoom (Room rId edgeList itemEdges) original replacement =
    let newEdges = f edgeList original replacement
     in Room rId newEdges itemEdges
  where
    f :: [Edge] -> RoomId -> RoomId -> [Edge]
    f [] _ _ = []
    f ((Edge p roomId):rest) orig replace =
        if roomId `elem` elevatorRooms
            then Edge p replace : f rest orig replace
            else Edge p roomId : f rest orig replace

elevatorRooms :: [RoomId]
elevatorRooms = [RTransporttoTallonOverworldNorth ,RTransporttoMagmoorCavernsNorth ,RTransporttoTallonOverworldEast ,RTransporttoTallonOverworldSouth
    ,DTransporttoMagmoorCavernsWest ,DTransporttoMagmoorCavernsSouth ,OTransporttoChozoRuinsWest ,OTransporttoChozoRuinsEast ,OTransporttoMagmoorCavernsEast 
    ,OTransporttoChozoRuinsSouth ,OTransporttoPhazonMinesEast,MTransporttoTallonOverworldSouth ,MTransporttoMagmoorCavernsSouth ,CTransporttoChozoRuinsNorth
    ,CTransporttoPhendranaDriftsNorth ,CTransporttoTallonOverworldWest ,CTransporttoPhazonMinesWest ,CTransporttoPhendranaDriftsSouth]

pseudoItems :: [Item]
pseudoItems = [Item FrigatePowerDoorTrigger FrigatePowerDoor OMainVentilationShaftSectionB
            ,Item MainQuarryBarrierTrigger MainQuarryBarrier MMainQuarry
            ,Item MainQuarrySaveTrigger MainQuarrySaveUnlocked MSaveStationMinesA
            ,Item ChozoIceTempleTrigger ChozoIceTempleBarrier DChozoIceTemple
            ,Item StorageDepotATrigger StorageDepotABarrier MMineSecurityStation
            ,Item ResearchLabHydraTrigger ResearchLabHydraBarrier DResearchLabHydra
            ,Item EliteControlTrigger EliteControlBarrier MEliteControl
            ,Item MetroidQuarantineATrigger MetroidQuarantineABarrier MMetroidQuarantineA
            ,Item MetroidQuarantineBTrigger MetroidQuarantineBBarrier MMetroidQuarantineB
            ,Item OmegaPirateTopTrigger OmegaPirateTopBarrier MProcessingCenterAccess
            ,Item OmegaPirateEntranceTrigger OmegaPirateEntranceBarrier MEliteQuartersAccess]

pseudoItemNames :: [ItemName]
pseudoItemNames = map itemName pseudoItems

{-- This creates all rooms to add to the graph --}
buildNodes :: Difficulty -> [Room]
buildNodes diff = [ -- Tallon Overworld Rooms
            Room OLandingSite [Edge noReq OCanyonCavern
                                    ,Edge noReq OWaterfallCavern
                                    ,Edge (sjf diff) OGully
                                    ,Edge (sjf diff) OAlcove
                                    ,Edge noReq OTempleHall]
                                    [IEdge morph LandingSite]
            ,Room OAlcove [Edge noReq OLandingSite]
                                    [IEdge noReq Alcove]
            ,Room OCanyonCavern [Edge noReq OLandingSite
                                    ,Edge noReq OTallonCanyon
                                    ,Edge (tallonCanyonSw diff) OTallonFrontSw] []
            ,Room OTallonCanyon [Edge noReq OCanyonCavern
                                    ,Edge boostBombs OGully
                                    ,Edge noReq ORootTunnel
                                    ,Edge noReq OTransportTunnelA] []
            ,Room OGully [Edge bombs OTallonCanyon
                                    ,Edge noReq OLandingSite] []
            ,Room ORootTunnel [Edge noReq OTallonCanyon
                                    ,Edge missile ORootCave] []
            ,Room ORootCave [Edge missile ORootTunnel
                                    ,Edge noReq OTransportTunnelB
                                    ,Edge (arbor diff) OArborChamber]
                                    [IEdge (rootCaveItem diff) RootCave]
            ,Room OTransportTunnelB [Edge noReq ORootCave
                                    ,Edge noReq OTransporttoMagmoorCavernsEast]
                                    [IEdge noReq TransportTunnelB]
            ,Room OTransporttoMagmoorCavernsEast [Edge noReq CTransporttoTallonOverworldWest
                                    ,Edge noReq OTransportTunnelB] []
            ,Room OArborChamber [Edge noReq ORootCave]
                                    [IEdge noReq ArborChamber]
            ,Room OTransportTunnelA [Edge noReq OTallonCanyon
                                    ,Edge noReq OTransporttoChozoRuinsWest] []
            ,Room OTransporttoChozoRuinsWest [Edge noReq OTransportTunnelA
                                    ,Edge noReq RTransporttoTallonOverworldNorth] []
            ,Room OWaterfallCavern [Edge noReq OLandingSite
                                    ,Edge morphMissile OFrigateCrashSite] []
            ,Room OFrigateCrashSite [Edge noReq OWaterfallCavern
                                    ,Edge (fcsClimb diff) OOvergrownCavern
                                    ,Edge (fcsEntry diff) OFrigateAccessTunnel]
                                    [IEdge (fcsItem diff) FrigateCrashSite]
            ,Room OOvergrownCavern [Edge ice OFrigateCrashSite
                                    ,Edge ice OTransportTunnelC]
                                    [IEdge morph OvergrownCavern]
            ,Room OTransportTunnelC [Edge ice OOvergrownCavern
                                    ,Edge ice OTransporttoChozoRuinsEast] []
            ,Room OTransporttoChozoRuinsEast [Edge noReq RTransporttoTallonOverworldEast
                                    ,Edge ice OTransportTunnelC] []
            ,Room OFrigateAccessTunnel [Edge ice OFrigateCrashSite
                                    ,Edge noReq OMainVentilationShaftSectionC] []
            ,Room OMainVentilationShaftSectionC [Edge noReq OFrigateAccessTunnel
                                    ,Edge noReq OMainVentilationShaftSectionB] []
            ,Room OMainVentilationShaftSectionB [Edge wave OMainVentilationShaftSectionA
                                    ,Edge (climbFrigateMvs diff) OMainVentilationShaftSectionC]
                                    [IEdge wave FrigatePowerDoorTrigger]
            ,Room OMainVentilationShaftSectionA [Edge (frigatePowerDoor diff) OMainVentilationShaftSectionB
                                    ,Edge noReq OReactorCore] []
            ,Room OReactorCore [Edge (climbReactorCore diff) OMainVentilationShaftSectionA
                                    ,Edge wave OReactorAccess] []
            ,Room OReactorAccess [Edge wave OCargoFreightLifttoDeckGamma
                                    ,Edge noReq OReactorCore
                                    ,Edge noReq OSaveStation] []
            ,Room OSaveStation [Edge noReq OReactorAccess] []
            ,Room OCargoFreightLifttoDeckGamma [Edge (cargoFreightLift diff) ODeckBetaTransitHall
                                    ,Edge noReq OReactorAccess]
                                    [IEdge missile CargoFreightLifttoDeckGamma]
            ,Room ODeckBetaTransitHall [Edge noReq OCargoFreightLifttoDeckGamma
                                    ,Edge noReq OBiohazardContainment] []
            ,Room OBiohazardContainment [Edge noReq ODeckBetaTransitHall
                                    ,Edge (biohazard diff) ODeckBetaSecurityHall]
                                    [IEdge supers BiohazardContainment]
            ,Room ODeckBetaSecurityHall [Edge (climbBiohazard diff) OBiohazardContainment
                                    ,Edge noReq OBiotechResearchArea1]
                                    [IEdge supers BiohazardContainment]
            ,Room OBiotechResearchArea1 [Edge noReq ODeckBetaSecurityHall
                                    ,Edge (biotech diff) ODeckBetaConduitHall] []
            ,Room ODeckBetaConduitHall [Edge (biotechReverse diff) OBiotechResearchArea1
                                    ,Edge noReq OConnectionElevatortoDeckBeta] []
            ,Room OConnectionElevatortoDeckBeta [Edge noReq ODeckBetaConduitHall
                                    ,Edge noReq OHydroAccessTunnel] []
            ,Room OHydroAccessTunnel [Edge gravSpace OConnectionElevatortoDeckBeta
                                    ,Edge (hydroTunnel diff) OGreatTreeHall]
                                    [IEdge morph HydroAccessTunnel]
            --Great Tree Hall is split into two rooms, an upper and lower section
            ,Room OGreatTreeHall [Edge (hydroTunnel diff) OHydroAccessTunnel
                                    ,Edge ice OTransportTunnelE
                                    ,Edge (gthClimb diff) OGreatTreeHallTop] []
            ,Room OGreatTreeHallTop [Edge ice OTransportTunnelD
                                    ,Edge (gtcEnter diff) OGreatTreeChamber
                                    ,Edge (gthSpiderTrack diff) OLifeGroveTunnel
                                    ,Edge (bars diff) OGreatTreeHall] []
            ,Room OTransportTunnelD [Edge ice OGreatTreeHallTop
                                    ,Edge ice OTransporttoChozoRuinsSouth] []
            ,Room OTransporttoChozoRuinsSouth [Edge ice OTransportTunnelD
                                    ,Edge noReq RTransporttoTallonOverworldSouth] []
            ,Room OGreatTreeChamber [Edge noReq OGreatTreeHallTop
                                    ,Edge (gtcSw diff) OTallonBackSw]
                                    [IEdge noReq GreatTreeChamber]
            ,Room OLifeGroveTunnel [Edge noReq OGreatTreeHallTop
                                    ,Edge (lifeGroveTunnel diff) OLifeGrove]
                                    [IEdge (lifeGroveTunnelItem diff) LifeGroveTunnel]
            ,Room OLifeGrove [Edge morph OLifeGroveTunnel]
                                    [IEdge noReq LifeGroveStart
                                    ,IEdge (lgUnderWater diff) LifeGroveUnderwaterSpinner]
            ,Room OTransportTunnelE [Edge ice OTransporttoPhazonMinesEast
                                    ,Edge ice OGreatTreeHall] []
            ,Room OTransporttoPhazonMinesEast [Edge ice OTransportTunnelE
                                    ,Edge noReq MTransporttoTallonOverworldSouth] []
            ,Room OTempleHall [Edge noReq OLandingSite
                                    ,Edge noReq OTempleSecurityStation] []
            ,Room OTempleSecurityStation [Edge missile OTempleLobby
                                    ,Edge noReq OTempleHall] []
            ,Room OTempleLobby [Edge missile OTempleSecurityStation
                                    ,Edge noReq OArtifactTemple] []
            ,Room OArtifactTemple [Edge noReq OTempleLobby]
                                    [IEdge noReq ArtifactTemple]
            ,Room OTallonBackSw [Edge bombs OLifeGrove
                                    ,Edge bombs OGreatTreeHallTop
                                    ,Edge (wallcrawlIntoFrigate diff) ODeckBetaConduitHall]
                                    [IEdge bombs LifeGroveUnderwaterSpinner]
            ,Room OTallonFrontSw [Edge bombs OFrigateCrashSite
                                    ,Edge bombs OTallonCanyon]
                                    [IEdge bombs ArborChamber
                                    ,IEdge bombs RootCave]

            -- Chozo Ruins Rooms
            ,Room RTransporttoTallonOverworldNorth [Edge noReq OTransporttoChozoRuinsWest
                                    ,Edge noReq RRuinsEntrance] []
            ,Room RRuinsEntrance [Edge noReq RTransporttoTallonOverworldNorth
                                    ,Edge noReq RMainPlaza] []
            ,Room RMainPlaza [Edge noReq RRuinsEntrance
                                    ,Edge morph RRuinedFountainAccess
                                    ,Edge missile RRuinedShrineAccess
                                    ,Edge noReq RNurseryAccess
                                    ,Edge (mainPlazaGrappleLedge diff) RPistonTunnelInbounds
                                    ,Edge (mainPlazaLedge diff) RMainPlazaLedge
                                    ,Edge (mainPlazaSw diff) RChozoFrontSw]
                                    [IEdge (mainPipe diff) MainPlazaHalfPipe
                                    ,IEdge (mainPlazaGrappleLedge diff) MainPlazaGrappleLedge
                                    ,IEdge supers MainPlazaTree]
            --Created new room to hold the main plaza ledge item, and allow one-way traversal through Vault
            ,Room RMainPlazaLedge [Edge noReq RMainPlaza]
                                    [IEdge noReq MainPlazaLockedDoor] 
            ,Room RPlazaAccess [Edge noReq RVault
                                    ,Edge noReq RMainPlazaLedge] []
            ,Room RVault [Edge noReq RPlazaAccess
                                    ,Edge morph RVaultAccess]
                                    [IEdge bombs Vault]
            ,Room RVaultAccess [Edge morph RVault
                                    ,Edge noReq RTransporttoMagmoorCavernsNorth] []
            ,Room RTransporttoMagmoorCavernsNorth [Edge noReq RVaultAccess
                                    ,Edge noReq CTransporttoChozoRuinsNorth
                                    ,Edge (climbSunTower diff) RSunTower
                                    ,Edge morph RTransportAccessNorth] []
            ,Room RTransportAccessNorth [Edge morph RTransporttoMagmoorCavernsNorth
                                    ,Edge missile RHiveTotem]
                                    [IEdge noReq TransportAccessNorth]
            ,Room RHiveTotem [Edge missile RTransportAccessNorth
                                    ,Edge noReq RTotemAccess]
                                    [IEdge noReq HiveTotem]
            ,Room RTotemAccess [Edge noReq RHiveTotem
                                    ,Edge noReq RRuinedGallery] []
            ,Room RRuinedGallery [Edge noReq RTotemAccess
                                    ,Edge missile RMapStation
                                    ,Edge noReq RNorthAtrium]
                                    [IEdge missile RuinedGalleryMissileWall
                                    ,IEdge bombs RuinedGalleryTunnel]
            ,Room RMapStation [Edge missile RRuinedGallery] []
            ,Room RNorthAtrium [Edge noReq RRuinedGallery
                                    ,Edge noReq RRuinedNursery] []
            ,Room RRuinedNursery [Edge noReq RNorthAtrium
                                    ,Edge noReq RSaveStation1
                                    ,Edge noReq REyonTunnel]
                                    [IEdge bombs RuinedNursery]
            ,Room RSaveStation1 [Edge noReq RRuinedNursery] []
            ,Room REyonTunnel [Edge noReq RRuinedNursery
                                    ,Edge noReq RNurseryAccess] []
            ,Room RNurseryAccess [Edge noReq REyonTunnel
                                    ,Edge noReq RMainPlaza] []
            ,Room RRuinedShrineAccess [Edge noReq RRuinedShrine
                                    ,Edge missile RMainPlaza] []
            ,Room RRuinedShrine [Edge noReq RRuinedShrineAccess
                                    ,Edge (tolAccess diff) RTowerofLightAccess]
                                    [IEdge bombs RuinedShrineLowerTunnel
                                    ,IEdge (rsHalf diff) RuinedShrineHalfPipe
                                    ,IEdge noReq RuinedShrineBeetleBattle]
            ,Room RTowerofLightAccess [Edge wave RRuinedShrine
                                    ,Edge wave RTowerofLight] []
            ,Room RTowerofLight [Edge wave RTowerofLightAccess
                                    ,Edge (towerChamber diff) RTowerChamber]
                                    [IEdge (towerOfLight diff) TowerofLight]
            ,Room RTowerChamber [Edge wave RTowerofLight]
                                    [IEdge noReq TowerChamber]
            ,Room RRuinedFountainAccess [Edge noReq RRuinedFountainNonWarp
                                    ,Edge morph RMainPlaza] []
            -- The Ruined Fountain Warp puts you on top of the item and forces you to collect it
            ,Room RRuinedFountainNonWarp [Edge (ruinedFountainItem diff) RRuinedFountain
                                    ,Edge noReq RRuinedFountainAccess
                                    ,Edge noReq RMeditationFountain
                                    ,Edge noReq RArboretumAccess] []
            ,Room RRuinedFountain [Edge (leaveRuinedFountainItem diff) RRuinedFountainNonWarp]
                                    [IEdge noReq RuinedFountain]
            ,Room RMeditationFountain [Edge noReq RRuinedFountainNonWarp
                                    ,Edge noReq RMagmaPool] []
            ,Room RMagmaPool [Edge noReq RMeditationFountain
                                    ,Edge (crossMagmaPool diff) RTrainingChamberAccess]
                                    [IEdge (magmaPoolItem diff) MagmaPool]
            ,Room RTrainingChamberAccess [Edge (crossMagmaPool diff) RMagmaPool
                                    ,Edge wave RTrainingChamber]
                                    [IEdge wavePb MagmaPool
                                    ,IEdge morph TrainingChamberAccess]
            ,Room RTrainingChamber [Edge wave RTowerofLightAccess
                                    ,Edge (tcTunnel diff) RPistonTunnelInbounds]
                                    [IEdge (tcItem diff) TrainingChamber]
            ,Room RPistonTunnelInbounds [Edge morph RMainPlaza
                                    ,Edge blocked RTrainingChamber] [] -- Since it is blocked initially, it's simpler to consider it one-way
            ,Room RPistonTunnel [Edge morph RPistonTunnelInbounds -- If you jump after being warped here, you go oob
                                    ,Edge (wallcrawl diff) RChozoFrontSw] []
            ,Room RArboretumAccess [Edge noReq RRuinedFountainNonWarp
                                    ,Edge missile RArboretum] []
            ,Room RArboretum [Edge missile RArboretumAccess
                                    ,Edge bombs RSunchamberLobby
                                    ,Edge missile RGatheringHallAccess] []
            ,Room RSunchamberLobby [Edge missile RArboretum
                                    ,Edge noReq RSunchamberAccess] []
            ,Room RSunchamberAccess [Edge noReq RSunchamberLobby
                                    ,Edge noVines RSunchamber] []
            ,Room RSunchamber [Edge noVines RSunchamberAccess
                                    ,Edge noReq RSunTowerAccess]
                                    [IEdge bombs SunchamberFlaahgra
                                    ,IEdge (sunchamberghost diff) SunchamberGhosts]
            ,Room RSunTowerAccess [Edge noReq RSunchamber
                                    ,Edge noReq RSunTower] []
            -- The spawn point is at the top of the room, so to approximate this, items are required to enter the room from the elevator
            ,Room RSunTower [Edge noReq RSunTowerAccess
                                    ,Edge noReq RTransporttoMagmoorCavernsNorth] []
            ,Room RGatheringHallAccess [Edge missile RArboretum
                                    ,Edge noReq RGatheringHall] []
            ,Room RGatheringHall [Edge noReq RGatheringHallAccess
                                    ,Edge missile RSaveStation2
                                    ,Edge noReq RWateryHallAccess
                                    ,Edge morph REastAtrium
                                    ,Edge (gatheringHallSw diff) RChozoBackSw]
                                    [IEdge bombs GatheringHall]
            ,Room RWateryHallAccess [Edge noReq RGatheringHall
                                    ,Edge missile RWateryHall]
                                    [IEdge missile WateryHallAccess]
            ,Room RWateryHall [Edge missile RWateryHallAccess
                                    ,Edge (wateryHallTraverse diff) RDynamoAccess
                                    ,Edge (wateryHallSw diff) RChozoBackSw]
                                    [IEdge (wateryHallWater diff) WateryHallUnderwater
                                    ,IEdge noReq WateryHallScanPuzzle]
            ,Room RDynamoAccess [Edge missile RWateryHall
                                    ,Edge missile RDynamo] []
            ,Room RDynamo [Edge missile RDynamoAccess]
                                    [IEdge missile DynamoLower
                                    ,IEdge spider DynamoSpiderTrack]
            ,Room RSaveStation2 [Edge missile RGatheringHall] []
            ,Room REastAtrium [Edge noReq RGatheringHall
                                    ,Edge noReq REnergyCoreAccess] []
            ,Room REnergyCoreAccess [Edge noReq REastAtrium
                                    ,Edge noReq REnergyCore] []
            ,Room REnergyCore [Edge noReq REnergyCoreAccess
                                    ,Edge morph RBurnDomeAccess
                                    ,Edge bombs RWestFurnaceAccess] []
            ,Room RBurnDomeAccess [Edge bombs REnergyCore
                                    ,Edge morph RBurnDome] []
            ,Room RBurnDome [Edge noReq RBurnDomeAccess]
                                    [IEdge bombs BurnDomeMissile
                                    ,IEdge noReq BurnDomeIDrone]
            ,Room RWestFurnaceAccess [Edge noReq REnergyCore
                                    ,Edge noReq RFurnaceFront] []
            ,Room RFurnaceFront [Edge noReq RWestFurnaceAccess
                                    ,Edge (furnaceTraverse diff) RFurnace]
                                    [IEdge bombs FurnaceInsideFurnace]
            ,Room RFurnace [Edge bombs RFurnaceFront
                                    ,Edge morph RCrosswayAccessWest
                                    ,Edge ice REastFurnaceAccess]
                                    [IEdge (furnaceItem diff) FurnaceSpiderTracks]
            ,Room REastFurnaceAccess [Edge ice RFurnace
                                    ,Edge ice RHalloftheElders] []
            ,Room RCrosswayAccessWest [Edge morph RFurnace
                                    ,Edge wave RCrossway] []
            ,Room RCrossway [Edge noReq RCrosswayAccessWest
                                    ,Edge (crosswayTraverse diff) RElderHallAccess
                                    ,Edge ice RCrosswayAccessSouth]
                                    [IEdge (crosswayItem diff) Crossway]
            ,Room RElderHallAccess [Edge missile RCrossway
                                    ,Edge noReq RHalloftheElders] []
            ,Room RCrosswayAccessSouth [Edge ice RCrossway
                                    ,Edge ice RHalloftheElders] []
            ,Room RHalloftheElders [Edge ice RCrosswayAccessSouth
                                    ,Edge ice REastFurnaceAccess
                                    ,Edge sjOrBombs RElderHallAccess
                                    ,Edge (hoteWave diff) RReflectingPoolAccess
                                    ,Edge (hotePlasma diff) RElderChamber]
                                    [IEdge (hoteIce diff) HalloftheElders]
            ,Room RElderChamber [Edge (elderChamberExit diff) RHalloftheElders]
                                    [IEdge noReq ElderChamber]
            ,Room RReflectingPoolAccess [Edge noReq RHalloftheElders
                                    ,Edge noReq RReflectingPool] []
            ,Room RReflectingPool [Edge noReq RReflectingPoolAccess
                                    ,Edge (reflectPoolSave diff) RSaveStation3
                                    ,Edge (reflectPoolAntechamber diff) RAntechamber
                                    ,Edge (reflectPoolIceDoor diff) RTransportAccessSouth] []
            ,Room RAntechamber [Edge ice RReflectingPool]
                                    [IEdge noReq Antechamber]
            ,Room RTransportAccessSouth [Edge ice RReflectingPool
                                    ,Edge noReq RTransporttoTallonOverworldSouth] []
            ,Room RTransporttoTallonOverworldSouth [Edge noReq RTransportAccessSouth
                                    ,Edge noReq OTransporttoChozoRuinsSouth] []
            ,Room RSaveStation3 [Edge missile RReflectingPool
                                    ,Edge bombs RTransporttoTallonOverworldEast] []
            ,Room RTransporttoTallonOverworldEast [Edge bombs RSaveStation3
                                    ,Edge noReq OTransporttoChozoRuinsEast] []
            ,Room RChozoBackSw [Edge bombs RReflectingPool
                                    ,Edge (longWallcrawl diff) RChozoFrontSw]
                                    [IEdge bombs HalloftheElders
                                    ,IEdge (crosswayInfiniteSpeed diff) ElderChamber]
            ,Room RChozoFrontSw [] [IEdge bombs TrainingChamber
                                    ,IEdge bombs MainPlazaGrappleLedge
                                    ,IEdge bombs TrainingChamberAccess
                                    ,IEdge bombs TowerofLight]
            
            --Magmoor Caverns Rooms
            ,Room CTransporttoChozoRuinsNorth [Edge noReq RTransporttoMagmoorCavernsNorth
                                    ,Edge noReq CBurningTrail] []
            ,Room CBurningTrail [Edge noReq CTransporttoChozoRuinsNorth
                                    ,Edge missile CSaveStationMagmoorA
                                    ,Edge noReq CLakeTunnel
                                    ,Edge (burningTrailSw diff) CMagmoorFrontSw] []
            ,Room CSaveStationMagmoorA [Edge missile CBurningTrail] []
            ,Room CLakeTunnel [Edge noReq CBurningTrail
                                    ,Edge noReq CLavaLake] []
            ,Room CLavaLake [Edge noReq CLakeTunnel
                                    ,Edge (lavaLakeTraversal diff) CPitTunnel]
                                    [IEdge (lavaLakeItem diff) LavaLake]
            ,Room CPitTunnel [Edge (lavaLakeReverseTraversal diff) CLavaLake
                                    ,Edge (pitTunnel diff) CTriclopsPit] []
            ,Room CTriclopsPit [Edge (pitTunnelReverse diff) CPitTunnel
                                    ,Edge (storageCavern diff) CStorageCavern
                                    ,Edge (heatResistOr8Etanks diff) CMonitorTunnel] -- This has a high requirement to discourage this path to get to phendrana
                                    [IEdge (triclopsPitItem diff) TriclopsPit]
            ,Room CStorageCavern [Edge (vmr2Tank diff) CTriclopsPit]
                                    [IEdge noReq StorageCavern]
            ,Room CMonitorTunnel [Edge (vmr2Tank diff) CTriclopsPit
                                    ,Edge (vmr2Tank diff) CMonitorStation] []
            ,Room CMonitorStation [Edge (vmr4Tank diff) CMonitorTunnel -- This requirement is excessive if warped to MonitorStation, going to storage cavern
                                    ,Edge (vmr3Tank diff) CShoreTunnel
                                    ,Edge (toTransportTunnelA diff) CTransportTunnelA
                                    ,Edge (monitorStationClimb diff) CWarriorShrine] []
            ,Room CTransportTunnelA [Edge bombs CMonitorStation
                                    ,Edge noReq CTransporttoPhendranaDriftsNorth]
                                    [IEdge bombs TransportTunnelA]
            ,Room CTransporttoPhendranaDriftsNorth [Edge noReq CTransportTunnelA
                                    ,Edge noReq DTransporttoMagmoorCavernsWest] []
            ,Room CWarriorShrine [Edge (vmr2Tank diff) CMonitorStation
                                    ,Edge (warriorShrineTunnel diff) CFieryShores]
                                    [IEdge noReq WarriorShrine
                                    ,IEdge (warriorShrineTunnel diff) FieryShoresWarriorShrineTunnel]
            ,Room CShoreTunnel [Edge (vmr2Tank diff) CMonitorStation
                                    ,Edge (vmr2Tank diff) CFieryShores]
                                    [IEdge pb ShoreTunnel]
            ,Room CFieryShores [Edge (vmr3Tank diff) CShoreTunnel
                                    ,Edge (vmr1Tank diff) CTransportTunnelB]
                                    [IEdge bombs FieryShoresMorphTrack]
            ,Room CTransportTunnelB [Edge (vmr4Tank diff) CFieryShores
                                    ,Edge noReq CTransporttoTallonOverworldWest] []
            ,Room CTransporttoTallonOverworldWest [Edge (vmr4Tank diff) CTransportTunnelB
                                    ,Edge noReq OTransporttoMagmoorCavernsEast
                                    ,Edge (crossTft diff) CTwinFiresTunnel] []
            ,Room CTwinFiresTunnel [Edge (crossTftReverse diff) CTransporttoTallonOverworldWest
                                    ,Edge noReq CTwinFires] []
            ,Room CTwinFires [Edge noReq CTwinFiresTunnel
                                    ,Edge (crossTwinFires diff) CNorthCoreTunnel] []
            ,Room CNorthCoreTunnel [Edge (crossTwinFires diff) CTwinFires
                                    ,Edge (crossNorthCoreTunnel diff) CGeothermalCore] []
            ,Room CGeothermalCore [Edge (crossNorthCoreTunnel diff) CNorthCoreTunnel
                                    ,Edge noReq CSouthCoreTunnel
                                    ,Edge (geoCore diff) CPlasmaProcessing] []
            ,Room CPlasmaProcessing [Edge plasma CGeothermalCore]
                                    [IEdge noReq PlasmaProcessing]
            ,Room CSouthCoreTunnel [Edge wave CGeothermalCore
                                    ,Edge wave CMagmoorWorkstation] []
            ,Room CMagmoorWorkstation [Edge noReq CSouthCoreTunnel
                                    ,Edge sjOrBombs CWorkstationTunnel
                                    ,Edge (workstationWaveDoor diff) CTransportTunnelC
                                    ,Edge (workstationSw diff) CMagmoorBackSw]
                                    [IEdge (workstationItem diff) MagmoorWorkstation]
            ,Room CTransportTunnelC [Edge wave CMagmoorWorkstation
                                    ,Edge wave CTransporttoPhendranaDriftsSouth] []
            ,Room CTransporttoPhendranaDriftsSouth [Edge wave CTransportTunnelC
                                    ,Edge missile CSaveStationMagmoorB
                                    ,Edge noReq DTransporttoMagmoorCavernsSouth] []
            ,Room CSaveStationMagmoorB [Edge missile CTransporttoPhendranaDriftsSouth] []
            ,Room CWorkstationTunnel [Edge noReq CMagmoorWorkstation
                                    ,Edge (workstationTunnel diff) CTransporttoPhazonMinesWest] []
            ,Room CTransporttoPhazonMinesWest [Edge (workstationTunnel diff) CWorkstationTunnel
                                    ,Edge noReq MTransporttoMagmoorCavernsSouth] []
            ,Room CMagmoorBackSw [Edge bombs CTransporttoPhazonMinesWest
                                    ,Edge (longWallcrawl diff) CMagmoorFrontSw]
                                    [IEdge bombs PlasmaProcessing]
            ,Room CMagmoorFrontSw [Edge (magmoorFrontWallcrawl diff) CMagmoorBackSw] []

            -- Phendrana Drifts Rooms
            ,Room DTransporttoMagmoorCavernsWest [Edge noReq CTransporttoPhendranaDriftsNorth
                                    ,Edge noReq DShorelineEntrance] []
            ,Room DShorelineEntrance [Edge noReq DTransporttoMagmoorCavernsWest
                                    ,Edge (iceBarrier diff) DPhendranaShorelines] []
            ,Room DPhendranaShorelines [Edge (iceBarrier diff) DShorelineEntrance
                                    ,Edge noReq DSaveStationB
                                    ,Edge noReq DIceRuinsAccess
                                    ,Edge (climbShorelines diff)  DPhendranaShorelinesUpper
                                    ,Edge (climbShorelines diff) DTempleEntryway]
                                    [IEdge plasma PhendranaShorelinesBehindIce
                                    ,IEdge (shorelinesTower diff) PhendranaShorelinesSpiderTrack]
            ,Room DPhendranaShorelinesUpper [Edge noReq DPlazaWalkway
                                    ,Edge noReq DRuinsEntryway
                                    ,Edge noReq DPhendranaShorelines] []
            ,Room DSaveStationB [Edge noReq DPhendranaShorelines] []
            ,Room DTempleEntryway [Edge noReq DPhendranaShorelines
                                    ,Edge (iceBarrier diff) DChozoIceTemple] []
            ,Room DChozoIceTemple [Edge (iceBarrier diff) DTempleEntryway
                                    ,Edge (iceTempleClimb diff) DChapelTunnel]
                                    [IEdge (iceTempleClimb diff) ChozoIceTempleTrigger
                                    ,IEdge (iceTempleItem diff) ChozoIceTemple]
            ,Room DChapelTunnel [Edge chozoIceTempleBarrier DChozoIceTemple
                                    ,Edge noReq DChapeloftheElders] []-- Warp point is near Chapel of the Elders
            ,Room DChapeloftheElders [Edge wave DChapelTunnel]
                                    [IEdge missile ChapeloftheElders]
            ,Room DIceRuinsAccess [Edge noReq DPhendranaShorelines
                                    ,Edge (iceBarrier diff) DIceRuinsEast] []
            ,Room DIceRuinsEast [Edge (iceBarrier diff) DIceRuinsAccess
                                    ,Edge noReq DPlazaWalkway]
                                    [IEdge (ireSpiderTrack diff) IceRuinsEastSpiderTrack
                                    ,IEdge plasma IceRuinsEastBehindIce]
            ,Room DPlazaWalkway [Edge noReq DIceRuinsEast
                                    ,Edge noReq DPhendranaShorelinesUpper] []
            ,Room DRuinsEntryway [Edge noReq DPhendranaShorelinesUpper
                                    ,Edge noReq DIceRuinsWest] []
            ,Room DIceRuinsWest [Edge noReq DRuinsEntryway
                                    ,Edge missile DCanyonEntryway
                                    ,Edge (irwSw diff) DPhendranaFrontSw
                                    ,Edge (irwDoor diff) DCourtyardEntryway]
                                    [IEdge (irwItem diff) IceRuinsWest]
            ,Room DCanyonEntryway [Edge noReq DIceRuinsWest
                                    ,Edge noReq DPhendranaCanyon] []
            ,Room DPhendranaCanyon [Edge noReq DCanyonEntryway]
                                    [IEdge noReq PhendranaCanyon]
            ,Room DCourtyardEntryway [Edge noReq DIceRuinsWest
                                    ,Edge (ruinedCourtyardClimb diff) DRuinedCourtyard] []-- Ruined courtyard spawn is at the top of the room
            ,Room DRuinedCourtyard [Edge noReq DCourtyardEntryway
                                    ,Edge (ruinedCourtyardSave diff) DSaveStationA
                                    ,Edge wave DSpecimenStorage
                                    ,Edge (ruinedCourtyardConduit diff) DQuarantineAccess 
                                    ,Edge (ruinedCourtyardSw diff) DPhendranaFrontSw]
                                    [IEdge morph RuinedCourtyard]
            ,Room DSaveStationA [Edge missile DCourtyardEntryway -- If you fall
                                    ,Edge (ruinedCourtyardSave diff) DRuinedCourtyard] -- If can make it to the spawn point
                                    [IEdge morph RuinedCourtyard]-- You can grab the item by falling here, without reaching the warp
            ,Room DQuarantineAccess [Edge noReq DRuinedCourtyard
                                    ,Edge noReq DNorthQuarantineTunnel] []
            ,Room DNorthQuarantineTunnel [Edge wave DQuarantineAccess
                                    ,Edge (quarantineTunnel diff) DQuarantineCave] []
            ,Room DQuarantineCave [Edge (quarantineTunnel diff) DNorthQuarantineTunnel
                                    ,Edge (climbQuarantineCaveBack diff) DQuarantineCaveBack]
                                    [IEdge noReq QuarantineCave]
            -- Added a new "room" representing the other door in quarantine cave
            ,Room DQuarantineCaveBack [Edge (quarantineMonitor diff) DQuarantineMonitor
                                    ,Edge (quarantineTunnel diff) DSouthQuarantineTunnel
                                    ,Edge (climbQuarantineCaveEntrance diff) DQuarantineCave]
                                    [IEdge noReq QuarantineCave -- Can drop into thardus fight
                                    ]
            ,Room DQuarantineMonitor [Edge (climbQuarantineCaveBack diff) DQuarantineCaveBack
                                    ,Edge (climbQuarantineCaveEntrance diff) DQuarantineCave]
                                    [IEdge noReq QuarantineCave -- Can drop into thardus fight
                                    ,IEdge noReq QuarantineMonitor]
            ,Room DSouthQuarantineTunnel [Edge (quarantineTunnel diff) DQuarantineCaveBack
                                    ,Edge wave DTransporttoMagmoorCavernsSouth] []
            ,Room DTransporttoMagmoorCavernsSouth [Edge wave DSouthQuarantineTunnel
                                    ,Edge noReq CTransporttoPhendranaDriftsSouth
                                    ,Edge (phenElevatorClimb diff) DTransportAccess] []
            ,Room DTransportAccess [Edge ice DTransporttoMagmoorCavernsSouth
                                    ,Edge wave DFrozenPike]
                                    [IEdge plasma TransportAccess]
            ,Room DSpecimenStorage [Edge wave DRuinedCourtyard
                                    ,Edge wave DResearchEntrance] []
            ,Room DResearchEntrance [Edge wave DSpecimenStorage
                                    ,Edge noReq DMapStation
                                    ,Edge wave DHydraLabEntryway] []
            ,Room DMapStation [Edge noReq DResearchEntrance] []
            ,Room DHydraLabEntryway [Edge wave DResearchEntrance
                                    ,Edge wave DResearchLabHydra] []
            ,Room DResearchLabHydra [Edge wave DHydraLabEntryway
                                    ,Edge noReq DResearchLabHydraBack]
                                    [IEdge noReq ResearchLabHydraTrigger]
            ,Room DResearchLabHydraBack [Edge researchLabHydraBarrier DResearchLabHydra
                                    ,Edge wave DObservatoryAccess]
                                    [IEdge supers ResearchLabHydra]
            ,Room DObservatoryAccess [Edge wave DResearchLabHydra
                                    ,Edge wave DObservatory] []
            ,Room DObservatory [Edge wave DObservatoryAccess
                                    ,Edge (observatoryClimb diff) DObservatoryTop] []
            ,Room DObservatoryTop [Edge (observatorySave diff) DSaveStationD
                                    ,Edge wave DWestTowerEntrance
                                    ,Edge noReq DObservatory]
                                    [IEdge (observatoryItem diff) Observatory]
            ,Room DSaveStationD [Edge (observatorySave diff) DObservatoryTop] []
            ,Room DWestTowerEntrance [Edge wave DObservatoryTop
                                    ,Edge missile DWestTower] []
            ,Room DWestTower [Edge missile DWestTowerEntrance
                                    ,Edge wave DControlTower] []
            ,Room DControlTower [Edge wave DWestTower
                                    ,Edge wave DEastTower]
                                    [IEdge (controlTowerItem diff) ControlTower]
            ,Room DEastTower [Edge wave DControlTower
                                    ,Edge wave DAetherLabEntryway] []
            ,Room DAetherLabEntryway [Edge wave DEastTower
                                    ,Edge wave DResearchLabAether] []
            ,Room DResearchLabAether [Edge wave DAetherLabEntryway
                                    ,Edge wave DResearchCoreAccess]
                                    [IEdge missile ResearchLabAetherTank
                                    ,IEdge (rlaTrack diff) ResearchLabAetherMorphTrack]
            ,Room DResearchCoreAccess [Edge wave DResearchLabAether
                                    ,Edge wave DResearchCore] []
            ,Room DResearchCore [Edge wave DResearchCoreAccess
                                    ,Edge ice DPikeAccess]
                                    [IEdge noReq ResearchCore]
            ,Room DPikeAccess [Edge ice DResearchCore
                                    ,Edge wave DFrozenPike] []
            ,Room DFrozenPike [Edge (frozenPikeClimb diff) DTransportAccess
                                    ,Edge wave DPikeAccess
                                    ,Edge wave DFrostCaveAccess
                                    ,Edge (frozenPikeBottom diff) DHunterCaveAccess] []
            ,Room DFrostCaveAccess [Edge wave DFrozenPike
                                    ,Edge (frostCaveAccess diff) DFrostCave] []
            ,Room DFrostCave [Edge (frostCaveAccess diff) DFrostCaveAccess
                                    ,Edge (frostCaveDoor diff) DSaveStationC
                                    ,Edge (frostCaveToTunnel diff) DUpperEdgeTunnel
                                    ,Edge (frostCaveSw diff) DPhendranaBackSw]
                                    [IEdge (frostCaveItem diff) FrostCave]
            ,Room DSaveStationC [Edge (frostCaveDoor diff) DFrostCave] []
            ,Room DUpperEdgeTunnel [Edge (frostCaveAccess diff) DFrostCave
                                    ,Edge wave DPhendranasEdge] []
            ,Room DPhendranasEdge [Edge wave DUpperEdgeTunnel
                                    ,Edge (toStorageCave diff) DStorageCave
                                    ,Edge (toSecurityCave diff) DSecurityCave
                                    ,Edge noReq DLowerEdgeTunnel] []
            ,Room DStorageCave [Edge (fromStorageCave diff) DPhendranasEdge]
                                    [IEdge noReq StorageCave]
            ,Room DSecurityCave [Edge morph DPhendranasEdge]
                                    [IEdge noReq SecurityCave]
            ,Room DLowerEdgeTunnel [Edge (phenEdgeLower diff) DPhendranasEdge
                                    ,Edge wave DHunterCave] []
            ,Room DHunterCave [Edge wave DLowerEdgeTunnel
                                    ,Edge (hunterCaveLower diff) DLakeTunnel
                                    ,Edge (hunterCaveUpper diff) DHunterCaveFar] []
            ,Room DHunterCaveFar [Edge sjOrBombs DHunterCave
                                    ,Edge wave DChamberAccess
                                    ,Edge wave DHunterCaveAccess] []
            ,Room DLakeTunnel [Edge (hunterCaveClimb diff) DHunterCave
                                    ,Edge wave DGravityChamber] []
            ,Room DGravityChamber [Edge (gravityChamberToLakeTunnel diff) DLakeTunnel
                                    ,Edge (climbGravityChamber diff) DGravityChamberTop]
                                    [IEdge noReq GravityChamberUnderwater]
            ,Room DGravityChamberTop [Edge noReq DGravityChamber
                                    ,Edge wave DChamberAccess]
                                    [IEdge (gravLedge diff) GravityChamberGrappleLedge]
            ,Room DChamberAccess [Edge wave DGravityChamberTop
                                    ,Edge wave DHunterCaveFar] []
            ,Room DHunterCaveAccess [Edge wave DHunterCaveFar
                                    ,Edge (frozenPikeBottom diff) DFrozenPike] []
            ,Room DPhendranaFrontSw [Edge bombs DRuinedCourtyard
                                    ,Edge (longWallcrawl diff) DPhendranaBackSw]
                                    [IEdge bombs QuarantineMonitor
                                    ,IEdge bombs QuarantineCave]
            ,Room DPhendranaBackSw [Edge (longWallcrawl diff) DPhendranaFrontSw
                                    ,Edge bombs DFrostCave
                                    ,Edge bombs DGravityChamber
                                    ,Edge bombs DTransportAccess
                                    ,Edge bombs DFrozenPike]
                                    [IEdge bombs GravityChamberGrappleLedge
                                    ,IEdge (transportAccessItemOob diff) TransportAccess
                                    ,IEdge bombs SecurityCave
                                    ,IEdge bombs StorageCave]

            -- Phazon Mines Rooms
            ,Room MTransporttoTallonOverworldSouth [Edge noReq OTransporttoPhazonMinesEast
                                    ,Edge wave MQuarryAccess] []
            ,Room MQuarryAccess [Edge wave MTransporttoTallonOverworldSouth
                                    ,Edge wave MMainQuarry] []
            ,Room MMainQuarry [Edge wave MQuarryAccess
                                    ,Edge (quarrySave diff) MSaveStationMinesA
                                    ,Edge (reachWasteDisposal diff) MWasteDisposal
                                    ,Edge ice MSecurityAccessA]
                                    [IEdge noReq MainQuarryBarrierTrigger
                                    ,IEdge (quarrySave diff) MainQuarrySaveTrigger
                                    ,IEdge (quarryItem diff) MainQuarry]
            ,Room MSaveStationMinesA [Edge mainQuarryBarrierWave MMainQuarry] []
            ,Room MSecurityAccessA [Edge mainQuarryBarrierIce MMainQuarry
                                    ,Edge ice MMineSecurityStation]
                                    [IEdge pb SecurityAccessA]
            ,Room MMineSecurityStation [Edge waveIce MSecurityAccessA
                                    ,Edge (toStorageDepotA diff) MStorageDepotA
                                    ,Edge wave MSecurityAccessB]
                                    [IEdge pb StorageDepotATrigger]
            ,Room MStorageDepotA [Edge (storageDepotABarrier diff) MMineSecurityStation]
                                    [IEdge noReq StorageDepotA]
            ,Room MSecurityAccessB [Edge wave MMineSecurityStation
                                    ,Edge (securityAccessBSw diff) MMinesFrontSw
                                    ,Edge ice MEliteResearch] []
            ,Room MEliteResearch [Edge ice MSecurityAccessB
                                    ,Edge (eliteResearchDoor diff) MResearchAccess]
                                    [IEdge (eliteResearchTopItem diff) EliteResearchLaser
                                    ,IEdge (eliteResearchPirate diff) EliteResearchPhazonElite]
            -- Currently require boosting through wall even if you can laser it
            ,Room MResearchAccess [Edge (shaftClimb2 diff) MEliteResearch
                                    ,Edge ice MOreProcessingBottom] []
            ,Room MOreProcessingBottom [Edge ice MResearchAccess
                                    ,Edge (oreProcessingClimb diff) MElevatorAccessA
                                    ,Edge (oreProcessingTop diff) MWasteDisposal
                                    ,Edge (oreProcessingTop diff) MStorageDepotB] []
            -- Spawn point is next to the pb rocks
            ,Room MOreProcessing [Edge noReq MOreProcessingBottom
                                    ,Edge (dashFromPbRocks diff) MElevatorAccessA
                                    ,Edge (oreProcessingTopFromRocks diff) MOreProcessingTop] []
            -- This fake room is considered to be at the top on the waste disposal side
            ,Room MOreProcessingTop [Edge noReq MOreProcessing
                                    ,Edge ice MWasteDisposal
                                    ,Edge (oreProcessingCrossTop diff) MStorageDepotB] []
            ,Room MWasteDisposal [Edge ice MOreProcessingTop
                                    ,Edge (wasteDisposalTraversal diff) MMainQuarry] []
            ,Room MStorageDepotB [Edge (oreProcessingCrossTop diff) MOreProcessingTop
                                    ,Edge ice MOreProcessing]
                                    [IEdge noReq StorageDepotB]
            ,Room MElevatorAccessA [Edge ice MOreProcessing
                                    ,Edge (oreProcessingTopFromEaa diff) MOreProcessingTop
                                    ,Edge ice MElevatorA] []
            ,Room MElevatorA [Edge (shaftClimb1 diff) MElevatorAccessA
                                    ,Edge ice MEliteControlAccess] []
            ,Room MEliteControlAccess [Edge ice MElevatorA
                                    ,Edge wave MEliteControl]
                                    [IEdge (ecaItem diff) EliteControlAccess]
            ,Room MEliteControl [Edge wave MEliteControlAccess
                                    ,Edge ice MMaintenanceTunnel
                                    ,Edge ice MVentilationShaft]
                                    [IEdge noReq EliteControlTrigger]
            ,Room MMaintenanceTunnel [Edge ice MEliteControl
                                    ,Edge (maintTunnel diff) MPhazonProcessingCenter] []
            ,Room MPhazonProcessingCenter [Edge (maintTunnel diff) MMaintenanceTunnel
                                    ,Edge (omegaPirateTopBarrier diff) MProcessingCenterAccess
                                    ,Edge (ppcClimb diff) MTransportAccess]
                                    [IEdge pb PhazonProcessingCenter]
            ,Room MTransportAccess [Edge ice MPhazonProcessingCenter
                                    ,Edge (toMinesElevator diff) MTransporttoMagmoorCavernsSouth] []
            ,Room MTransporttoMagmoorCavernsSouth [Edge (toMinesElevator diff) MTransportAccess
                                    ,Edge noReq CTransporttoPhazonMinesWest] []
            -- Warp is at the top
            ,Room MVentilationShaft [Edge eliteControlBarrier MEliteControl
                                    ,Edge ice MOmegaResearch]
                                    [IEdge pb VentilationShaft]
            ,Room MOmegaResearch [Edge ice MVentilationShaft
                                    ,Edge (maintTunnel diff) MMapStationMines
                                    ,Edge ice MDynamoAccess] []
            ,Room MMapStationMines [Edge (maintTunnel diff) MOmegaResearch] []
            ,Room MDynamoAccess [Edge ice MOmegaResearch
                                    ,Edge ice MCentralDynamo] []
            -- Warp is the top, but treating as the bottom. It's slightly inaccurate
            ,Room MCentralDynamo [Edge (centralDynamoClimb diff) MDynamoAccess
                                    ,Edge ice MSaveStationMinesB
                                    ,Edge (maintTunnel diff) MQuarantineAccessA]
                                    [IEdge morph CentralDynamo]
            ,Room MSaveStationMinesB [Edge ice MCentralDynamo] []
            ,Room MQuarantineAccessA [Edge (maintTunnel diff) MCentralDynamo
                                    ,Edge wave MMetroidQuarantineA] []
            ,Room MMetroidQuarantineA [Edge wave MQuarantineAccessA
                                    ,Edge noReq MMetroidQuarantineABack]
                                    [IEdge noReq MetroidQuarantineATrigger]
            ,Room MMetroidQuarantineABack [Edge mqaBarrier MMetroidQuarantineA
                                    ,Edge (mqaTraversal diff) MElevatorAccessB]
                                    [IEdge (mqaItem diff) MetroidQuarantineA]
            ,Room MElevatorAccessB [Edge ice MMetroidQuarantineABack
                                    ,Edge plasma MElevatorB] []
            ,Room MElevatorB [Edge plasma MFungalHallAccess
                                    ,Edge plasma MElevatorAccessB] []
            ,Room MFungalHallAccess [Edge plasma MElevatorB
                                    ,Edge plasma MFungalHallA]
                                    [IEdge morph FungalHallAccess]
            ,Room MFungalHallA [Edge (climbFungalHallAccess diff) MFungalHallAccess
                                    ,Edge (fungalHallATraversal diff) MPhazonMiningTunnel] []
            ,Room MPhazonMiningTunnel [Edge plasma MFungalHallA
                                    ,Edge (miningTunnelTraversal diff) MFungalHallB]
                                    [IEdge (miningTunnelItem diff) PhazonMiningTunnel]
            ,Room MFungalHallB [Edge (miningTunnelTraversal diff) MPhazonMiningTunnel
                                    ,Edge (fungalHallBTraversal diff) MMissileStationMinesInbounds
                                    ,Edge (fungalHallBTraversal diff) MQuarantineAccessB]
                                    [IEdge bombs FungalHallB]
            ,Room MMissileStationMinesInbounds [Edge plasma MFungalHallB] []
            ,Room MMissileStationMines [Edge morph MMissileStationMinesInbounds -- You get warped out of bounds and need morph
                                    ,Edge (wallcrawl diff) MMinesBackSw] []
            ,Room MQuarantineAccessB [Edge plasma MFungalHallB
                                    ,Edge (quarantineAccessBTraversal diff) MMetroidQuarantineB] []
            ,Room MMetroidQuarantineB [Edge (quarantineAccessBTraversal diff) MQuarantineAccessB
                                    ,Edge (mqbTraversal diff) MMetroidQuarantineBBack]
                                    [IEdge noReq MetroidQuarantineBTrigger]
            ,Room MMetroidQuarantineBBack [Edge mqbBarrier MMetroidQuarantineB
                                    ,Edge plasma MSaveStationMinesC
                                    ,Edge (mqbBackClimb diff) MEliteQuartersAccess
                                    ,Edge (mqbSw diff) MMinesBackSw]
                                    [IEdge supers MetroidQuarantineB]
            ,Room MSaveStationMinesC [Edge plasma MMetroidQuarantineBBack] []
            ,Room MEliteQuartersAccess [Edge plasma MMetroidQuarantineBBack
                                    ,Edge plasma MEliteQuarters]
                                    [IEdge plasma EliteControlTrigger]
            ,Room MEliteQuarters [Edge (eliteQuartersExit diff) MEliteQuartersAccess
                                    ,Edge (eliteQuartersTop diff) MProcessingCenterAccess]
                                    [IEdge (eliteQuarters diff) EliteQuarters]
            ,Room MProcessingCenterAccess [Edge plasma MEliteQuarters
                                    ,Edge (ppcBottomClimb diff) MPhazonProcessingCenter]
                                    [IEdge noReq OmegaPirateTopTrigger
                                    ,IEdge noReq ProcessingCenterAccess]
            ,Room MMinesFrontSw [Edge ice MMainQuarry]
                                    [IEdge bombs StorageDepotA
                                    ,IEdge ice SecurityAccessA]
            ,Room MMinesBackSw [Edge bombs MFungalHallB
                                    ,Edge bombs MPhazonProcessingCenter
                                    ,Edge bombs MMetroidQuarantineB]
                                    [IEdge (longWallcrawl diff) FungalHallAccess]
            ]