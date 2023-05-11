module Graph (buildItemMap, buildRoomMap, replaceElevators, pseudoItems, pseudoItemNames, buildNodes) where

import Data.Set (Set)
import Data.Map (Map)
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
    let newEdges = replaceEdges edgeList original replacement
     in Room (fromEnum rId) newEdges itemEdges

replaceEdges :: [Edge] -> Int -> Int -> [Edge]
replaceEdges [] _ _ = []
replaceEdges ((Edge p roomId):rest) orig replace =
    if roomId `elem` elevatorRooms
        then Edge p (fromEnum replace) : replaceEdges rest orig replace
        else Edge p (fromEnum roomId) : replaceEdges rest orig replace

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
            room OLandingSite [edge noReq OCanyonCavern
                                    ,edge noReq OWaterfallCavern
                                    ,edge (sjf diff) OGully
                                    ,edge (sjf diff) OAlcove
                                    ,edge noReq OTempleHall]
                                    [itemEdge morph LandingSite]
            ,room OAlcove [edge noReq OLandingSite]
                                    [itemEdge noReq Alcove]
            ,room OCanyonCavern [edge noReq OLandingSite
                                    ,edge noReq OTallonCanyon
                                    ,edge (tallonCanyonSw diff) OTallonFrontSw] []
            ,room OTallonCanyon [edge noReq OCanyonCavern
                                    ,edge boostBombs OGully
                                    ,edge noReq ORootTunnel
                                    ,edge noReq OTransportTunnelA] []
            ,room OGully [edge bombs OTallonCanyon
                                    ,edge noReq OLandingSite] []
            ,room ORootTunnel [edge noReq OTallonCanyon
                                    ,edge missile ORootCave] []
            ,room ORootCave [edge missile ORootTunnel
                                    ,edge noReq OTransportTunnelB
                                    ,edge (arbor diff) OArborChamber]
                                    [itemEdge (rootCaveItem diff) RootCave]
            ,room OTransportTunnelB [edge noReq ORootCave
                                    ,edge noReq OTransporttoMagmoorCavernsEast]
                                    [itemEdge noReq TransportTunnelB]
            ,room OTransporttoMagmoorCavernsEast [edge noReq CTransporttoTallonOverworldWest
                                    ,edge noReq OTransportTunnelB] []
            ,room OArborChamber [edge noReq ORootCave]
                                    [itemEdge noReq ArborChamber]
            ,room OTransportTunnelA [edge noReq OTallonCanyon
                                    ,edge noReq OTransporttoChozoRuinsWest] []
            ,room OTransporttoChozoRuinsWest [edge noReq OTransportTunnelA
                                    ,edge noReq RTransporttoTallonOverworldNorth] []
            ,room OWaterfallCavern [edge noReq OLandingSite
                                    ,edge morphMissile OFrigateCrashSite] []
            ,room OFrigateCrashSite [edge noReq OWaterfallCavern
                                    ,edge (fcsClimb diff) OOvergrownCavern
                                    ,edge (fcsEntry diff) OFrigateAccessTunnel]
                                    [itemEdge (fcsItem diff) FrigateCrashSite]
            ,room OOvergrownCavern [edge ice OFrigateCrashSite
                                    ,edge ice OTransportTunnelC]
                                    [itemEdge morph OvergrownCavern]
            ,room OTransportTunnelC [edge ice OOvergrownCavern
                                    ,edge ice OTransporttoChozoRuinsEast] []
            ,room OTransporttoChozoRuinsEast [edge noReq RTransporttoTallonOverworldEast
                                    ,edge ice OTransportTunnelC] []
            ,room OFrigateAccessTunnel [edge ice OFrigateCrashSite
                                    ,edge noReq OMainVentilationShaftSectionC] []
            ,room OMainVentilationShaftSectionC [edge noReq OFrigateAccessTunnel
                                    ,edge noReq OMainVentilationShaftSectionB] []
            ,room OMainVentilationShaftSectionB [edge wave OMainVentilationShaftSectionA
                                    ,edge (climbFrigateMvs diff) OMainVentilationShaftSectionC]
                                    [itemEdge wave FrigatePowerDoorTrigger]
            ,room OMainVentilationShaftSectionA [edge (frigatePowerDoor diff) OMainVentilationShaftSectionB
                                    ,edge noReq OReactorCore] []
            ,room OReactorCore [edge (climbReactorCore diff) OMainVentilationShaftSectionA
                                    ,edge wave OReactorAccess] []
            ,room OReactorAccess [edge wave OCargoFreightLifttoDeckGamma
                                    ,edge noReq OReactorCore
                                    ,edge noReq OSaveStation] []
            ,room OSaveStation [edge noReq OReactorAccess] []
            ,room OCargoFreightLifttoDeckGamma [edge (cargoFreightLift diff) ODeckBetaTransitHall
                                    ,edge noReq OReactorAccess]
                                    [itemEdge missile CargoFreightLifttoDeckGamma]
            ,room ODeckBetaTransitHall [edge noReq OCargoFreightLifttoDeckGamma
                                    ,edge noReq OBiohazardContainment] []
            ,room OBiohazardContainment [edge noReq ODeckBetaTransitHall
                                    ,edge (biohazard diff) ODeckBetaSecurityHall]
                                    [itemEdge supers BiohazardContainment]
            ,room ODeckBetaSecurityHall [edge (climbBiohazard diff) OBiohazardContainment
                                    ,edge noReq OBiotechResearchArea1]
                                    [itemEdge supers BiohazardContainment]
            ,room OBiotechResearchArea1 [edge noReq ODeckBetaSecurityHall
                                    ,edge (biotech diff) ODeckBetaConduitHall] []
            ,room ODeckBetaConduitHall [edge (biotechReverse diff) OBiotechResearchArea1
                                    ,edge noReq OConnectionElevatortoDeckBeta] []
            ,room OConnectionElevatortoDeckBeta [edge noReq ODeckBetaConduitHall
                                    ,edge noReq OHydroAccessTunnel] []
            ,room OHydroAccessTunnel [edge gravSpace OConnectionElevatortoDeckBeta
                                    ,edge (hydroTunnel diff) OGreatTreeHall]
                                    [itemEdge morph HydroAccessTunnel]
            --Great Tree Hall is split into two rooms, an upper and lower section
            ,room OGreatTreeHall [edge (hydroTunnel diff) OHydroAccessTunnel
                                    ,edge ice OTransportTunnelE
                                    ,edge (gthClimb diff) OGreatTreeHallTop] []
            ,room OGreatTreeHallTop [edge ice OTransportTunnelD
                                    ,edge (gtcEnter diff) OGreatTreeChamber
                                    ,edge (gthSpiderTrack diff) OLifeGroveTunnel
                                    ,edge (bars diff) OGreatTreeHall] []
            ,room OTransportTunnelD [edge ice OGreatTreeHallTop
                                    ,edge ice OTransporttoChozoRuinsSouth] []
            ,room OTransporttoChozoRuinsSouth [edge ice OTransportTunnelD
                                    ,edge noReq RTransporttoTallonOverworldSouth] []
            ,room OGreatTreeChamber [edge noReq OGreatTreeHallTop
                                    ,edge (gtcSw diff) OTallonBackSw]
                                    [itemEdge noReq GreatTreeChamber]
            ,room OLifeGroveTunnel [edge noReq OGreatTreeHallTop
                                    ,edge (lifeGroveTunnel diff) OLifeGrove]
                                    [itemEdge (lifeGroveTunnelItem diff) LifeGroveTunnel]
            ,room OLifeGrove [edge morph OLifeGroveTunnel]
                                    [itemEdge noReq LifeGroveStart
                                    ,itemEdge (lgUnderWater diff) LifeGroveUnderwaterSpinner]
            ,room OTransportTunnelE [edge ice OTransporttoPhazonMinesEast
                                    ,edge ice OGreatTreeHall] []
            ,room OTransporttoPhazonMinesEast [edge ice OTransportTunnelE
                                    ,edge noReq MTransporttoTallonOverworldSouth] []
            ,room OTempleHall [edge noReq OLandingSite
                                    ,edge noReq OTempleSecurityStation] []
            ,room OTempleSecurityStation [edge missile OTempleLobby
                                    ,edge noReq OTempleHall] []
            ,room OTempleLobby [edge missile OTempleSecurityStation
                                    ,edge noReq OArtifactTemple] []
            ,room OArtifactTemple [edge noReq OTempleLobby]
                                    [itemEdge noReq ArtifactTemple]
            ,room OTallonBackSw [edge bombs OLifeGrove
                                    ,edge bombs OGreatTreeHallTop
                                    ,edge (wallcrawlIntoFrigate diff) ODeckBetaConduitHall]
                                    [itemEdge bombs LifeGroveUnderwaterSpinner]
            ,room OTallonFrontSw [edge bombs OFrigateCrashSite
                                    ,edge bombs OTallonCanyon]
                                    [itemEdge bombs ArborChamber
                                    ,itemEdge bombs RootCave]

            -- Chozo Ruins Rooms
            ,room RTransporttoTallonOverworldNorth [edge noReq OTransporttoChozoRuinsWest
                                    ,edge noReq RRuinsEntrance] []
            ,room RRuinsEntrance [edge noReq RTransporttoTallonOverworldNorth
                                    ,edge noReq RMainPlaza] []
            ,room RMainPlaza [edge noReq RRuinsEntrance
                                    ,edge morph RRuinedFountainAccess
                                    ,edge missile RRuinedShrineAccess
                                    ,edge noReq RNurseryAccess
                                    ,edge (mainPlazaGrappleLedge diff) RPistonTunnelInbounds
                                    ,edge (mainPlazaLedge diff) RMainPlazaLedge
                                    ,edge (mainPlazaSw diff) RChozoFrontSw]
                                    [itemEdge (mainPipe diff) MainPlazaHalfPipe
                                    ,itemEdge (mainPlazaGrappleLedge diff) MainPlazaGrappleLedge
                                    ,itemEdge supers MainPlazaTree]
            --Created new room to hold the main plaza ledge item, and allow one-way traversal through Vault
            ,room RMainPlazaLedge [edge noReq RMainPlaza]
                                    [itemEdge noReq MainPlazaLockedDoor] 
            ,room RPlazaAccess [edge noReq RVault
                                    ,edge noReq RMainPlazaLedge] []
            ,room RVault [edge noReq RPlazaAccess
                                    ,edge morph RVaultAccess]
                                    [itemEdge bombs Vault]
            ,room RVaultAccess [edge morph RVault
                                    ,edge noReq RTransporttoMagmoorCavernsNorth] []
            ,room RTransporttoMagmoorCavernsNorth [edge noReq RVaultAccess
                                    ,edge noReq CTransporttoChozoRuinsNorth
                                    ,edge (climbSunTower diff) RSunTower
                                    ,edge morph RTransportAccessNorth] []
            ,room RTransportAccessNorth [edge morph RTransporttoMagmoorCavernsNorth
                                    ,edge missile RHiveTotem]
                                    [itemEdge noReq TransportAccessNorth]
            ,room RHiveTotem [edge missile RTransportAccessNorth
                                    ,edge noReq RTotemAccess]
                                    [itemEdge noReq HiveTotem]
            ,room RTotemAccess [edge noReq RHiveTotem
                                    ,edge noReq RRuinedGallery] []
            ,room RRuinedGallery [edge noReq RTotemAccess
                                    ,edge missile RMapStation
                                    ,edge noReq RNorthAtrium]
                                    [itemEdge missile RuinedGalleryMissileWall
                                    ,itemEdge bombs RuinedGalleryTunnel]
            ,room RMapStation [edge missile RRuinedGallery] []
            ,room RNorthAtrium [edge noReq RRuinedGallery
                                    ,edge noReq RRuinedNursery] []
            ,room RRuinedNursery [edge noReq RNorthAtrium
                                    ,edge noReq RSaveStation1
                                    ,edge noReq REyonTunnel]
                                    [itemEdge bombs RuinedNursery]
            ,room RSaveStation1 [edge noReq RRuinedNursery] []
            ,room REyonTunnel [edge noReq RRuinedNursery
                                    ,edge noReq RNurseryAccess] []
            ,room RNurseryAccess [edge noReq REyonTunnel
                                    ,edge noReq RMainPlaza] []
            ,room RRuinedShrineAccess [edge noReq RRuinedShrine
                                    ,edge missile RMainPlaza] []
            ,room RRuinedShrine [edge noReq RRuinedShrineAccess
                                    ,edge (tolAccess diff) RTowerofLightAccess]
                                    [itemEdge bombs RuinedShrineLowerTunnel
                                    ,itemEdge (rsHalf diff) RuinedShrineHalfPipe
                                    ,itemEdge noReq RuinedShrineBeetleBattle]
            ,room RTowerofLightAccess [edge wave RRuinedShrine
                                    ,edge wave RTowerofLight] []
            ,room RTowerofLight [edge wave RTowerofLightAccess
                                    ,edge (towerChamber diff) RTowerChamber]
                                    [itemEdge (towerOfLight diff) TowerofLight]
            ,room RTowerChamber [edge wave RTowerofLight]
                                    [itemEdge noReq TowerChamber]
            ,room RRuinedFountainAccess [edge noReq RRuinedFountainNonWarp
                                    ,edge morph RMainPlaza] []
            -- The Ruined Fountain Warp puts you on top of the item and forces you to collect it
            ,room RRuinedFountainNonWarp [edge (ruinedFountainItem diff) RRuinedFountain
                                    ,edge noReq RRuinedFountainAccess
                                    ,edge noReq RMeditationFountain
                                    ,edge noReq RArboretumAccess] []
            ,room RRuinedFountain [edge (leaveRuinedFountainItem diff) RRuinedFountainNonWarp]
                                    [itemEdge noReq RuinedFountain]
            ,room RMeditationFountain [edge noReq RRuinedFountainNonWarp
                                    ,edge noReq RMagmaPool] []
            ,room RMagmaPool [edge noReq RMeditationFountain
                                    ,edge (crossMagmaPool diff) RTrainingChamberAccess]
                                    [itemEdge (magmaPoolItem diff) MagmaPool]
            ,room RTrainingChamberAccess [edge (crossMagmaPool diff) RMagmaPool
                                    ,edge wave RTrainingChamber]
                                    [itemEdge wavePb MagmaPool
                                    ,itemEdge morph TrainingChamberAccess]
            ,room RTrainingChamber [edge wave RTowerofLightAccess
                                    ,edge (tcTunnel diff) RPistonTunnelInbounds]
                                    [itemEdge (tcItem diff) TrainingChamber]
            ,room RPistonTunnelInbounds [edge morph RMainPlaza
                                    ,edge blocked RTrainingChamber] [] -- Since it is blocked initially, it's simpler to consider it one-way
            ,room RPistonTunnel [edge morph RPistonTunnelInbounds -- If you jump after being warped here, you go oob
                                    ,edge (wallcrawl diff) RChozoFrontSw] []
            ,room RArboretumAccess [edge noReq RRuinedFountainNonWarp
                                    ,edge missile RArboretum] []
            ,room RArboretum [edge missile RArboretumAccess
                                    ,edge bombs RSunchamberLobby
                                    ,edge missile RGatheringHallAccess] []
            ,room RSunchamberLobby [edge missile RArboretum
                                    ,edge noReq RSunchamberAccess] []
            ,room RSunchamberAccess [edge noReq RSunchamberLobby
                                    ,edge noVines RSunchamber] []
            ,room RSunchamber [edge noVines RSunchamberAccess
                                    ,edge noReq RSunTowerAccess]
                                    [itemEdge bombs SunchamberFlaahgra
                                    ,itemEdge (sunchamberghost diff) SunchamberGhosts]
            ,room RSunTowerAccess [edge noReq RSunchamber
                                    ,edge noReq RSunTower] []
            -- The spawn point is at the top of the room, so to approximate this, items are required to enter the room from the elevator
            ,room RSunTower [edge noReq RSunTowerAccess
                                    ,edge noReq RTransporttoMagmoorCavernsNorth] []
            ,room RGatheringHallAccess [edge missile RArboretum
                                    ,edge noReq RGatheringHall] []
            ,room RGatheringHall [edge noReq RGatheringHallAccess
                                    ,edge missile RSaveStation2
                                    ,edge noReq RWateryHallAccess
                                    ,edge morph REastAtrium
                                    ,edge (gatheringHallSw diff) RChozoBackSw]
                                    [itemEdge bombs GatheringHall]
            ,room RWateryHallAccess [edge noReq RGatheringHall
                                    ,edge missile RWateryHall]
                                    [itemEdge missile WateryHallAccess]
            ,room RWateryHall [edge missile RWateryHallAccess
                                    ,edge (wateryHallTraverse diff) RDynamoAccess
                                    ,edge (wateryHallSw diff) RChozoBackSw]
                                    [itemEdge (wateryHallWater diff) WateryHallUnderwater
                                    ,itemEdge noReq WateryHallScanPuzzle]
            ,room RDynamoAccess [edge missile RWateryHall
                                    ,edge missile RDynamo] []
            ,room RDynamo [edge missile RDynamoAccess]
                                    [itemEdge missile DynamoLower
                                    ,itemEdge spider DynamoSpiderTrack]
            ,room RSaveStation2 [edge missile RGatheringHall] []
            ,room REastAtrium [edge noReq RGatheringHall
                                    ,edge noReq REnergyCoreAccess] []
            ,room REnergyCoreAccess [edge noReq REastAtrium
                                    ,edge noReq REnergyCore] []
            ,room REnergyCore [edge noReq REnergyCoreAccess
                                    ,edge morph RBurnDomeAccess
                                    ,edge bombs RWestFurnaceAccess] []
            ,room RBurnDomeAccess [edge bombs REnergyCore
                                    ,edge morph RBurnDome] []
            ,room RBurnDome [edge noReq RBurnDomeAccess]
                                    [itemEdge bombs BurnDomeMissile
                                    ,itemEdge noReq BurnDomeIDrone]
            ,room RWestFurnaceAccess [edge noReq REnergyCore
                                    ,edge noReq RFurnaceFront] []
            ,room RFurnaceFront [edge noReq RWestFurnaceAccess
                                    ,edge (furnaceTraverse diff) RFurnace]
                                    [itemEdge bombs FurnaceInsideFurnace]
            ,room RFurnace [edge bombs RFurnaceFront
                                    ,edge morph RCrosswayAccessWest
                                    ,edge ice REastFurnaceAccess]
                                    [itemEdge (furnaceItem diff) FurnaceSpiderTracks]
            ,room REastFurnaceAccess [edge ice RFurnace
                                    ,edge ice RHalloftheElders] []
            ,room RCrosswayAccessWest [edge morph RFurnace
                                    ,edge wave RCrossway] []
            ,room RCrossway [edge noReq RCrosswayAccessWest
                                    ,edge (crosswayTraverse diff) RElderHallAccess
                                    ,edge ice RCrosswayAccessSouth]
                                    [itemEdge (crosswayItem diff) Crossway]
            ,room RElderHallAccess [edge missile RCrossway
                                    ,edge noReq RHalloftheElders] []
            ,room RCrosswayAccessSouth [edge ice RCrossway
                                    ,edge ice RHalloftheElders] []
            ,room RHalloftheElders [edge ice RCrosswayAccessSouth
                                    ,edge ice REastFurnaceAccess
                                    ,edge sjOrBombs RElderHallAccess
                                    ,edge (hoteWave diff) RReflectingPoolAccess
                                    ,edge (hotePlasma diff) RElderChamber]
                                    [itemEdge (hoteIce diff) HalloftheElders]
            ,room RElderChamber [edge (elderChamberExit diff) RHalloftheElders]
                                    [itemEdge noReq ElderChamber]
            ,room RReflectingPoolAccess [edge noReq RHalloftheElders
                                    ,edge noReq RReflectingPool] []
            ,room RReflectingPool [edge noReq RReflectingPoolAccess
                                    ,edge (reflectPoolSave diff) RSaveStation3
                                    ,edge (reflectPoolAntechamber diff) RAntechamber
                                    ,edge (reflectPoolIceDoor diff) RTransportAccessSouth] []
            ,room RAntechamber [edge ice RReflectingPool]
                                    [itemEdge noReq Antechamber]
            ,room RTransportAccessSouth [edge ice RReflectingPool
                                    ,edge noReq RTransporttoTallonOverworldSouth] []
            ,room RTransporttoTallonOverworldSouth [edge noReq RTransportAccessSouth
                                    ,edge noReq OTransporttoChozoRuinsSouth] []
            ,room RSaveStation3 [edge missile RReflectingPool
                                    ,edge bombs RTransporttoTallonOverworldEast] []
            ,room RTransporttoTallonOverworldEast [edge bombs RSaveStation3
                                    ,edge noReq OTransporttoChozoRuinsEast] []
            ,room RChozoBackSw [edge bombs RReflectingPool
                                    ,edge (longWallcrawl diff) RChozoFrontSw]
                                    [itemEdge bombs HalloftheElders
                                    ,itemEdge (crosswayInfiniteSpeed diff) ElderChamber]
            ,room RChozoFrontSw [] [itemEdge bombs TrainingChamber
                                    ,itemEdge bombs MainPlazaGrappleLedge
                                    ,itemEdge bombs TrainingChamberAccess
                                    ,itemEdge bombs TowerofLight]
            
            --Magmoor Caverns Rooms
            ,room CTransporttoChozoRuinsNorth [edge noReq RTransporttoMagmoorCavernsNorth
                                    ,edge noReq CBurningTrail] []
            ,room CBurningTrail [edge noReq CTransporttoChozoRuinsNorth
                                    ,edge missile CSaveStationMagmoorA
                                    ,edge noReq CLakeTunnel
                                    ,edge (burningTrailSw diff) CMagmoorFrontSw] []
            ,room CSaveStationMagmoorA [edge missile CBurningTrail] []
            ,room CLakeTunnel [edge noReq CBurningTrail
                                    ,edge noReq CLavaLake] []
            ,room CLavaLake [edge noReq CLakeTunnel
                                    ,edge (lavaLakeTraversal diff) CPitTunnel]
                                    [itemEdge (lavaLakeItem diff) LavaLake]
            ,room CPitTunnel [edge (lavaLakeReverseTraversal diff) CLavaLake
                                    ,edge (pitTunnel diff) CTriclopsPit] []
            ,room CTriclopsPit [edge (pitTunnelReverse diff) CPitTunnel
                                    ,edge (storageCavern diff) CStorageCavern
                                    ,edge (heatResistOr8Etanks diff) CMonitorTunnel] -- This has a high requirement to discourage this path to get to phendrana
                                    [itemEdge (triclopsPitItem diff) TriclopsPit]
            ,room CStorageCavern [edge (vmr2Tank diff) CTriclopsPit]
                                    [itemEdge noReq StorageCavern]
            ,room CMonitorTunnel [edge (vmr2Tank diff) CTriclopsPit
                                    ,edge (vmr2Tank diff) CMonitorStation] []
            ,room CMonitorStation [edge (vmr4Tank diff) CMonitorTunnel -- This requirement is excessive if warped to MonitorStation, going to storage cavern
                                    ,edge (vmr3Tank diff) CShoreTunnel
                                    ,edge (toTransportTunnelA diff) CTransportTunnelA
                                    ,edge (monitorStationClimb diff) CWarriorShrine] []
            ,room CTransportTunnelA [edge bombs CMonitorStation
                                    ,edge noReq CTransporttoPhendranaDriftsNorth]
                                    [itemEdge bombs TransportTunnelA]
            ,room CTransporttoPhendranaDriftsNorth [edge noReq CTransportTunnelA
                                    ,edge noReq DTransporttoMagmoorCavernsWest] []
            ,room CWarriorShrine [edge (vmr2Tank diff) CMonitorStation
                                    ,edge (warriorShrineTunnel diff) CFieryShores]
                                    [itemEdge noReq WarriorShrine
                                    ,itemEdge (warriorShrineTunnel diff) FieryShoresWarriorShrineTunnel]
            ,room CShoreTunnel [edge (vmr2Tank diff) CMonitorStation
                                    ,edge (vmr2Tank diff) CFieryShores]
                                    [itemEdge pb ShoreTunnel]
            ,room CFieryShores [edge (vmr3Tank diff) CShoreTunnel
                                    ,edge (vmr1Tank diff) CTransportTunnelB]
                                    [itemEdge bombs FieryShoresMorphTrack]
            ,room CTransportTunnelB [edge (vmr4Tank diff) CFieryShores
                                    ,edge noReq CTransporttoTallonOverworldWest] []
            ,room CTransporttoTallonOverworldWest [edge (vmr4Tank diff) CTransportTunnelB
                                    ,edge noReq OTransporttoMagmoorCavernsEast
                                    ,edge (crossTft diff) CTwinFiresTunnel] []
            ,room CTwinFiresTunnel [edge (crossTftReverse diff) CTransporttoTallonOverworldWest
                                    ,edge noReq CTwinFires] []
            ,room CTwinFires [edge noReq CTwinFiresTunnel
                                    ,edge (crossTwinFires diff) CNorthCoreTunnel] []
            ,room CNorthCoreTunnel [edge (crossTwinFires diff) CTwinFires
                                    ,edge (crossNorthCoreTunnel diff) CGeothermalCore] []
            ,room CGeothermalCore [edge (crossNorthCoreTunnel diff) CNorthCoreTunnel
                                    ,edge noReq CSouthCoreTunnel
                                    ,edge (geoCore diff) CPlasmaProcessing] []
            ,room CPlasmaProcessing [edge plasma CGeothermalCore]
                                    [itemEdge noReq PlasmaProcessing]
            ,room CSouthCoreTunnel [edge wave CGeothermalCore
                                    ,edge wave CMagmoorWorkstation] []
            ,room CMagmoorWorkstation [edge noReq CSouthCoreTunnel
                                    ,edge sjOrBombs CWorkstationTunnel
                                    ,edge (workstationWaveDoor diff) CTransportTunnelC
                                    ,edge (workstationSw diff) CMagmoorBackSw]
                                    [itemEdge (workstationItem diff) MagmoorWorkstation]
            ,room CTransportTunnelC [edge wave CMagmoorWorkstation
                                    ,edge wave CTransporttoPhendranaDriftsSouth] []
            ,room CTransporttoPhendranaDriftsSouth [edge wave CTransportTunnelC
                                    ,edge missile CSaveStationMagmoorB
                                    ,edge noReq DTransporttoMagmoorCavernsSouth] []
            ,room CSaveStationMagmoorB [edge missile CTransporttoPhendranaDriftsSouth] []
            ,room CWorkstationTunnel [edge noReq CMagmoorWorkstation
                                    ,edge (workstationTunnel diff) CTransporttoPhazonMinesWest] []
            ,room CTransporttoPhazonMinesWest [edge (workstationTunnel diff) CWorkstationTunnel
                                    ,edge noReq MTransporttoMagmoorCavernsSouth] []
            ,room CMagmoorBackSw [edge bombs CTransporttoPhazonMinesWest
                                    ,edge (longWallcrawl diff) CMagmoorFrontSw]
                                    [itemEdge bombs PlasmaProcessing]
            ,room CMagmoorFrontSw [edge (magmoorFrontWallcrawl diff) CMagmoorBackSw] []

            -- Phendrana Drifts Rooms
            ,room DTransporttoMagmoorCavernsWest [edge noReq CTransporttoPhendranaDriftsNorth
                                    ,edge noReq DShorelineEntrance] []
            ,room DShorelineEntrance [edge noReq DTransporttoMagmoorCavernsWest
                                    ,edge (iceBarrier diff) DPhendranaShorelines] []
            ,room DPhendranaShorelines [edge (iceBarrier diff) DShorelineEntrance
                                    ,edge noReq DSaveStationB
                                    ,edge noReq DIceRuinsAccess
                                    ,Edge (climbShorelines diff)  (fromEnum DPhendranaShorelinesUpper)
                                    ,edge (climbShorelines diff) DTempleEntryway]
                                    [itemEdge plasma PhendranaShorelinesBehindIce
                                    ,itemEdge (shorelinesTower diff) PhendranaShorelinesSpiderTrack]
            ,room DPhendranaShorelinesUpper [edge noReq DPlazaWalkway
                                    ,edge noReq DRuinsEntryway
                                    ,edge noReq DPhendranaShorelines] []
            ,room DSaveStationB [edge noReq DPhendranaShorelines] []
            ,room DTempleEntryway [edge noReq DPhendranaShorelines
                                    ,edge (iceBarrier diff) DChozoIceTemple] []
            ,room DChozoIceTemple [edge (iceBarrier diff) DTempleEntryway
                                    ,edge (iceTempleClimb diff) DChapelTunnel]
                                    [itemEdge (iceTempleClimb diff) ChozoIceTempleTrigger
                                    ,itemEdge (iceTempleItem diff) ChozoIceTemple]
            ,room DChapelTunnel [edge chozoIceTempleBarrier DChozoIceTemple
                                    ,edge noReq DChapeloftheElders] []-- Warp point is near Chapel of the Elders
            ,room DChapeloftheElders [edge wave DChapelTunnel]
                                    [itemEdge missile ChapeloftheElders]
            ,room DIceRuinsAccess [edge noReq DPhendranaShorelines
                                    ,edge (iceBarrier diff) DIceRuinsEast] []
            ,room DIceRuinsEast [edge (iceBarrier diff) DIceRuinsAccess
                                    ,edge noReq DPlazaWalkway]
                                    [itemEdge (ireSpiderTrack diff) IceRuinsEastSpiderTrack
                                    ,itemEdge plasma IceRuinsEastBehindIce]
            ,room DPlazaWalkway [edge noReq DIceRuinsEast
                                    ,edge noReq DPhendranaShorelinesUpper] []
            ,room DRuinsEntryway [edge noReq DPhendranaShorelinesUpper
                                    ,edge noReq DIceRuinsWest] []
            ,room DIceRuinsWest [edge noReq DRuinsEntryway
                                    ,edge missile DCanyonEntryway
                                    ,edge (irwSw diff) DPhendranaFrontSw
                                    ,edge (irwDoor diff) DCourtyardEntryway]
                                    [itemEdge (irwItem diff) IceRuinsWest]
            ,room DCanyonEntryway [edge noReq DIceRuinsWest
                                    ,edge noReq DPhendranaCanyon] []
            ,room DPhendranaCanyon [edge noReq DCanyonEntryway]
                                    [itemEdge noReq PhendranaCanyon]
            ,room DCourtyardEntryway [edge noReq DIceRuinsWest
                                    ,edge (ruinedCourtyardClimb diff) DRuinedCourtyard] []-- Ruined courtyard spawn is at the top of the room
            ,room DRuinedCourtyard [edge noReq DCourtyardEntryway
                                    ,edge (ruinedCourtyardSave diff) DSaveStationA
                                    ,edge wave DSpecimenStorage
                                    ,edge (ruinedCourtyardConduit diff) DQuarantineAccess 
                                    ,edge (ruinedCourtyardSw diff) DPhendranaFrontSw]
                                    [itemEdge morph RuinedCourtyard]
            ,room DSaveStationA [edge missile DCourtyardEntryway -- If you fall
                                    ,edge (ruinedCourtyardSave diff) DRuinedCourtyard] -- If can make it to the spawn point
                                    [itemEdge morph RuinedCourtyard]-- You can grab the item by falling here, without reaching the warp
            ,room DQuarantineAccess [edge noReq DRuinedCourtyard
                                    ,edge noReq DNorthQuarantineTunnel] []
            ,room DNorthQuarantineTunnel [edge wave DQuarantineAccess
                                    ,edge (quarantineTunnel diff) DQuarantineCave] []
            ,room DQuarantineCave [edge (quarantineTunnel diff) DNorthQuarantineTunnel
                                    ,edge (climbQuarantineCaveBack diff) DQuarantineCaveBack]
                                    [itemEdge noReq QuarantineCave]
            -- Added a new "room" representing the other door in quarantine cave
            ,room DQuarantineCaveBack [edge (quarantineMonitor diff) DQuarantineMonitor
                                    ,edge (quarantineTunnel diff) DSouthQuarantineTunnel
                                    ,edge (climbQuarantineCaveEntrance diff) DQuarantineCave]
                                    [itemEdge noReq QuarantineCave -- Can drop into thardus fight
                                    ]
            ,room DQuarantineMonitor [edge (climbQuarantineCaveBack diff) DQuarantineCaveBack
                                    ,edge (climbQuarantineCaveEntrance diff) DQuarantineCave]
                                    [itemEdge noReq QuarantineCave -- Can drop into thardus fight
                                    ,itemEdge noReq QuarantineMonitor]
            ,room DSouthQuarantineTunnel [edge (quarantineTunnel diff) DQuarantineCaveBack
                                    ,edge wave DTransporttoMagmoorCavernsSouth] []
            ,room DTransporttoMagmoorCavernsSouth [edge wave DSouthQuarantineTunnel
                                    ,edge noReq CTransporttoPhendranaDriftsSouth
                                    ,edge (phenElevatorClimb diff) DTransportAccess] []
            ,room DTransportAccess [edge ice DTransporttoMagmoorCavernsSouth
                                    ,edge wave DFrozenPike]
                                    [itemEdge plasma TransportAccess]
            ,room DSpecimenStorage [edge wave DRuinedCourtyard
                                    ,edge wave DResearchEntrance] []
            ,room DResearchEntrance [edge wave DSpecimenStorage
                                    ,edge noReq DMapStation
                                    ,edge wave DHydraLabEntryway] []
            ,room DMapStation [edge noReq DResearchEntrance] []
            ,room DHydraLabEntryway [edge wave DResearchEntrance
                                    ,edge wave DResearchLabHydra] []
            ,room DResearchLabHydra [edge wave DHydraLabEntryway
                                    ,edge noReq DResearchLabHydraBack]
                                    [itemEdge noReq ResearchLabHydraTrigger]
            ,room DResearchLabHydraBack [edge researchLabHydraBarrier DResearchLabHydra
                                    ,edge wave DObservatoryAccess]
                                    [itemEdge supers ResearchLabHydra]
            ,room DObservatoryAccess [edge wave DResearchLabHydra
                                    ,edge wave DObservatory] []
            ,room DObservatory [edge wave DObservatoryAccess
                                    ,edge (observatoryClimb diff) DObservatoryTop] []
            ,room DObservatoryTop [edge (observatorySave diff) DSaveStationD
                                    ,edge wave DWestTowerEntrance
                                    ,edge noReq DObservatory]
                                    [itemEdge (observatoryItem diff) Observatory]
            ,room DSaveStationD [edge (observatorySave diff) DObservatoryTop] []
            ,room DWestTowerEntrance [edge wave DObservatoryTop
                                    ,edge missile DWestTower] []
            ,room DWestTower [edge missile DWestTowerEntrance
                                    ,edge wave DControlTower] []
            ,room DControlTower [edge wave DWestTower
                                    ,edge wave DEastTower]
                                    [itemEdge (controlTowerItem diff) ControlTower]
            ,room DEastTower [edge wave DControlTower
                                    ,edge wave DAetherLabEntryway] []
            ,room DAetherLabEntryway [edge wave DEastTower
                                    ,edge wave DResearchLabAether] []
            ,room DResearchLabAether [edge wave DAetherLabEntryway
                                    ,edge wave DResearchCoreAccess]
                                    [itemEdge missile ResearchLabAetherTank
                                    ,itemEdge (rlaTrack diff) ResearchLabAetherMorphTrack]
            ,room DResearchCoreAccess [edge wave DResearchLabAether
                                    ,edge wave DResearchCore] []
            ,room DResearchCore [edge wave DResearchCoreAccess
                                    ,edge ice DPikeAccess]
                                    [itemEdge noReq ResearchCore]
            ,room DPikeAccess [edge ice DResearchCore
                                    ,edge wave DFrozenPike] []
            ,room DFrozenPike [edge (frozenPikeClimb diff) DTransportAccess
                                    ,edge wave DPikeAccess
                                    ,edge wave DFrostCaveAccess
                                    ,edge (frozenPikeBottom diff) DHunterCaveAccess] []
            ,room DFrostCaveAccess [edge wave DFrozenPike
                                    ,edge (frostCaveAccess diff) DFrostCave] []
            ,room DFrostCave [edge (frostCaveAccess diff) DFrostCaveAccess
                                    ,edge (frostCaveDoor diff) DSaveStationC
                                    ,edge (frostCaveToTunnel diff) DUpperEdgeTunnel
                                    ,edge (frostCaveSw diff) DPhendranaBackSw]
                                    [itemEdge (frostCaveItem diff) FrostCave]
            ,room DSaveStationC [edge (frostCaveDoor diff) DFrostCave] []
            ,room DUpperEdgeTunnel [edge (frostCaveAccess diff) DFrostCave
                                    ,edge wave DPhendranasEdge] []
            ,room DPhendranasEdge [edge wave DUpperEdgeTunnel
                                    ,edge (toStorageCave diff) DStorageCave
                                    ,edge (toSecurityCave diff) DSecurityCave
                                    ,edge noReq DLowerEdgeTunnel] []
            ,room DStorageCave [edge (fromStorageCave diff) DPhendranasEdge]
                                    [itemEdge noReq StorageCave]
            ,room DSecurityCave [edge morph DPhendranasEdge]
                                    [itemEdge noReq SecurityCave]
            ,room DLowerEdgeTunnel [edge (phenEdgeLower diff) DPhendranasEdge
                                    ,edge wave DHunterCave] []
            ,room DHunterCave [edge wave DLowerEdgeTunnel
                                    ,edge (hunterCaveLower diff) DLakeTunnel
                                    ,edge (hunterCaveUpper diff) DHunterCaveFar] []
            ,room DHunterCaveFar [edge sjOrBombs DHunterCave
                                    ,edge wave DChamberAccess
                                    ,edge wave DHunterCaveAccess] []
            ,room DLakeTunnel [edge (hunterCaveClimb diff) DHunterCave
                                    ,edge wave DGravityChamber] []
            ,room DGravityChamber [edge (gravityChamberToLakeTunnel diff) DLakeTunnel
                                    ,edge (climbGravityChamber diff) DGravityChamberTop]
                                    [itemEdge noReq GravityChamberUnderwater]
            ,room DGravityChamberTop [edge noReq DGravityChamber
                                    ,edge wave DChamberAccess]
                                    [itemEdge (gravLedge diff) GravityChamberGrappleLedge]
            ,room DChamberAccess [edge wave DGravityChamberTop
                                    ,edge wave DHunterCaveFar] []
            ,room DHunterCaveAccess [edge wave DHunterCaveFar
                                    ,edge (frozenPikeBottom diff) DFrozenPike] []
            ,room DPhendranaFrontSw [edge bombs DRuinedCourtyard
                                    ,edge (longWallcrawl diff) DPhendranaBackSw]
                                    [itemEdge bombs QuarantineMonitor
                                    ,itemEdge bombs QuarantineCave]
            ,room DPhendranaBackSw [edge (longWallcrawl diff) DPhendranaFrontSw
                                    ,edge bombs DFrostCave
                                    ,edge bombs DGravityChamber
                                    ,edge bombs DTransportAccess
                                    ,edge bombs DFrozenPike]
                                    [itemEdge bombs GravityChamberGrappleLedge
                                    ,itemEdge (transportAccessItemOob diff) TransportAccess
                                    ,itemEdge bombs SecurityCave
                                    ,itemEdge bombs StorageCave]

            -- Phazon Mines Rooms
            ,room MTransporttoTallonOverworldSouth [edge noReq OTransporttoPhazonMinesEast
                                    ,edge wave MQuarryAccess] []
            ,room MQuarryAccess [edge wave MTransporttoTallonOverworldSouth
                                    ,edge wave MMainQuarry] []
            ,room MMainQuarry [edge wave MQuarryAccess
                                    ,edge (quarrySave diff) MSaveStationMinesA
                                    ,edge (reachWasteDisposal diff) MWasteDisposal
                                    ,edge ice MSecurityAccessA]
                                    [itemEdge noReq MainQuarryBarrierTrigger
                                    ,itemEdge (quarrySave diff) MainQuarrySaveTrigger
                                    ,itemEdge (quarryItem diff) MainQuarry]
            ,room MSaveStationMinesA [edge mainQuarryBarrierWave MMainQuarry] []
            ,room MSecurityAccessA [edge mainQuarryBarrierIce MMainQuarry
                                    ,edge ice MMineSecurityStation]
                                    [itemEdge pb SecurityAccessA]
            ,room MMineSecurityStation [edge waveIce MSecurityAccessA
                                    ,edge (toStorageDepotA diff) MStorageDepotA
                                    ,edge wave MSecurityAccessB]
                                    [itemEdge pb StorageDepotATrigger]
            ,room MStorageDepotA [edge (storageDepotABarrier diff) MMineSecurityStation]
                                    [itemEdge noReq StorageDepotA]
            ,room MSecurityAccessB [edge wave MMineSecurityStation
                                    ,edge (securityAccessBSw diff) MMinesFrontSw
                                    ,edge ice MEliteResearch] []
            ,room MEliteResearch [edge ice MSecurityAccessB
                                    ,edge (eliteResearchDoor diff) MResearchAccess]
                                    [itemEdge (eliteResearchTopItem diff) EliteResearchLaser
                                    ,itemEdge (eliteResearchPirate diff) EliteResearchPhazonElite]
            -- Currently require boosting through wall even if you can laser it
            ,room MResearchAccess [edge (shaftClimb2 diff) MEliteResearch
                                    ,edge ice MOreProcessingBottom] []
            ,room MOreProcessingBottom [edge ice MResearchAccess
                                    ,edge (oreProcessingClimb diff) MElevatorAccessA
                                    ,edge (oreProcessingTop diff) MWasteDisposal
                                    ,edge (oreProcessingTop diff) MStorageDepotB] []
            -- Spawn point is next to the pb rocks
            ,room MOreProcessing [edge noReq MOreProcessingBottom
                                    ,edge (dashFromPbRocks diff) MElevatorAccessA
                                    ,edge (oreProcessingTopFromRocks diff) MOreProcessingTop] []
            -- This fake room is considered to be at the top on the waste disposal side
            ,room MOreProcessingTop [edge noReq MOreProcessing
                                    ,edge ice MWasteDisposal
                                    ,edge (oreProcessingCrossTop diff) MStorageDepotB] []
            ,room MWasteDisposal [edge ice MOreProcessingTop
                                    ,edge (wasteDisposalTraversal diff) MMainQuarry] []
            ,room MStorageDepotB [edge (oreProcessingCrossTop diff) MOreProcessingTop
                                    ,edge ice MOreProcessing]
                                    [itemEdge noReq StorageDepotB]
            ,room MElevatorAccessA [edge ice MOreProcessing
                                    ,edge (oreProcessingTopFromEaa diff) MOreProcessingTop
                                    ,edge ice MElevatorA] []
            ,room MElevatorA [edge (shaftClimb1 diff) MElevatorAccessA
                                    ,edge ice MEliteControlAccess] []
            ,room MEliteControlAccess [edge ice MElevatorA
                                    ,edge wave MEliteControl]
                                    [itemEdge (ecaItem diff) EliteControlAccess]
            ,room MEliteControl [edge wave MEliteControlAccess
                                    ,edge ice MMaintenanceTunnel
                                    ,edge ice MVentilationShaft]
                                    [itemEdge noReq EliteControlTrigger]
            ,room MMaintenanceTunnel [edge ice MEliteControl
                                    ,edge (maintTunnel diff) MPhazonProcessingCenter] []
            ,room MPhazonProcessingCenter [edge (maintTunnel diff) MMaintenanceTunnel
                                    ,edge blocked MProcessingCenterAccess -- This barrier is considered to be one-way
                                    ,edge (ppcClimb diff) MTransportAccess]
                                    [itemEdge pb PhazonProcessingCenter]
            ,room MTransportAccess [edge ice MPhazonProcessingCenter
                                    ,edge (toMinesElevator diff) MTransporttoMagmoorCavernsSouth] []
            ,room MTransporttoMagmoorCavernsSouth [edge (toMinesElevator diff) MTransportAccess
                                    ,edge noReq CTransporttoPhazonMinesWest] []
            -- Warp is at the top
            ,room MVentilationShaft [edge eliteControlBarrier MEliteControl
                                    ,edge ice MOmegaResearch]
                                    [itemEdge pb VentilationShaft]
            ,room MOmegaResearch [edge ice MVentilationShaft
                                    ,edge (maintTunnel diff) MMapStationMines
                                    ,edge ice MDynamoAccess] []
            ,room MMapStationMines [edge (maintTunnel diff) MOmegaResearch] []
            ,room MDynamoAccess [edge ice MOmegaResearch
                                    ,edge ice MCentralDynamo] []
            -- Warp is the top, but treating as the bottom. It's slightly inaccurate
            ,room MCentralDynamo [edge (centralDynamoClimb diff) MDynamoAccess
                                    ,edge ice MSaveStationMinesB
                                    ,edge (maintTunnel diff) MQuarantineAccessA]
                                    [itemEdge morph CentralDynamo]
            ,room MSaveStationMinesB [edge ice MCentralDynamo] []
            ,room MQuarantineAccessA [edge (maintTunnel diff) MCentralDynamo
                                    ,edge wave MMetroidQuarantineA] []
            ,room MMetroidQuarantineA [edge wave MQuarantineAccessA
                                    ,edge noReq MMetroidQuarantineABack]
                                    [itemEdge noReq MetroidQuarantineATrigger]
            ,room MMetroidQuarantineABack [edge mqaBarrier MMetroidQuarantineA
                                    ,edge (mqaTraversal diff) MElevatorAccessB]
                                    [itemEdge (mqaItem diff) MetroidQuarantineA]
            ,room MElevatorAccessB [edge ice MMetroidQuarantineABack
                                    ,edge plasma MElevatorB] []
            ,room MElevatorB [edge plasma MFungalHallAccess
                                    ,edge plasma MElevatorAccessB] []
            ,room MFungalHallAccess [edge plasma MElevatorB
                                    ,edge plasma MFungalHallA]
                                    [itemEdge morph FungalHallAccess]
            ,room MFungalHallA [edge (climbFungalHallAccess diff) MFungalHallAccess
                                    ,edge (fungalHallATraversal diff) MPhazonMiningTunnel] []
            ,room MPhazonMiningTunnel [edge plasma MFungalHallA
                                    ,edge (miningTunnelTraversal diff) MFungalHallB]
                                    [itemEdge (miningTunnelItem diff) PhazonMiningTunnel]
            ,room MFungalHallB [edge (miningTunnelTraversal diff) MPhazonMiningTunnel
                                    ,edge (fungalHallBTraversal diff) MMissileStationMinesInbounds
                                    ,edge (fungalHallBTraversal diff) MQuarantineAccessB]
                                    [itemEdge bombs FungalHallB]
            ,room MMissileStationMinesInbounds [edge plasma MFungalHallB] []
            ,room MMissileStationMines [edge morph MMissileStationMinesInbounds -- You get warped out of bounds and need morph
                                    ,edge (wallcrawl diff) MMinesBackSw] []
            ,room MQuarantineAccessB [edge plasma MFungalHallB
                                    ,edge (quarantineAccessBTraversal diff) MMetroidQuarantineB] []
            ,room MMetroidQuarantineB [edge (quarantineAccessBTraversal diff) MQuarantineAccessB
                                    ,edge (mqbTraversal diff) MMetroidQuarantineBBack]
                                    [itemEdge noReq MetroidQuarantineBTrigger]
            ,room MMetroidQuarantineBBack [edge mqbBarrier MMetroidQuarantineB
                                    ,edge plasma MSaveStationMinesC
                                    ,edge (mqbBackClimb diff) MEliteQuartersAccess
                                    ,edge (mqbSw diff) MMinesBackSw]
                                    [itemEdge supers MetroidQuarantineB]
            ,room MSaveStationMinesC [edge plasma MMetroidQuarantineBBack] []
            ,room MEliteQuartersAccess [edge plasma MMetroidQuarantineBBack
                                    ,edge plasma MEliteQuarters]
                                    [itemEdge plasma OmegaPirateEntranceTrigger]
            ,room MEliteQuarters [edge (eliteQuartersExit diff) MEliteQuartersAccess
                                    ,edge (eliteQuartersTop diff) MProcessingCenterAccess]
                                    [itemEdge (eliteQuarters diff) EliteQuarters]
            ,room MProcessingCenterAccess [edge plasma MEliteQuarters
                                    ,edge (ppcBottomClimb diff) MPhazonProcessingCenter]
                                    [itemEdge noReq ProcessingCenterAccess]
            ,room MMinesFrontSw [edge ice MMainQuarry]
                                    [itemEdge bombs StorageDepotA
                                    ,itemEdge ice SecurityAccessA]
            ,room MMinesBackSw [edge bombs MFungalHallB
                                    ,edge bombs MPhazonProcessingCenter
                                    ,edge bombs MMetroidQuarantineB]
                                    [itemEdge (longWallcrawl diff) FungalHallAccess]
            ]
        where 
            room :: RoomId -> [Edge] -> [IEdge] -> Room
            room roomId = Room (fromEnum roomId)
            
            edge :: (Map ItemName Int -> Set Int -> Bool) -> RoomId -> Edge
            edge predicate roomId = Edge predicate (fromEnum roomId)

            itemEdge :: (Map ItemName Int -> Set Int -> Bool) -> ItemId -> IEdge
            itemEdge predicate itemId = IEdge predicate (fromEnum itemId)
