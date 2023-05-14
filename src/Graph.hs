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
    3. Tries to find a sequence of warps that collects at least one progression item
    4. If no such sequences can be found, we are done and cannot complete the game. Otherwise, repeat steps 1-3.

Graph Construction:
Much of the graph construction is hard-coded. During construction, the difficulty is passed in to any edge predicate that requires it, 
thereby eliminating the need to pass in the difficulty when solving the graph.

Non-Warping Items:
If the item does not warp, it is treated as if it warps to the room containing the item (Or in some cases, an adjacent room that is more appropriate).
This is mostly useful for non-warp randomizer seeds. 
--}

buildItemMap :: [Item] -> IntMap Item
buildItemMap items = IntMap.fromList (map (\x -> (itemId x, x)) items)

buildRoomMap :: [Room] -> IntMap Room
buildRoomMap rooms = IntMap.fromList (map (\x -> (roomId x, x)) rooms)

replaceElevators :: IntMap Room -> [(Int, Int)] -> IntMap Room
replaceElevators graph [] = graph
replaceElevators graph ((a, b):rest) =
    let oldNode = getVal (IntMap.lookup a graph) "Missing room"
        newNode = replaceEdgeRoom oldNode a b
     in replaceElevators (IntMap.insert a newNode graph) rest

replaceEdgeRoom :: Room -> Int -> Int -> Room
replaceEdgeRoom (Room rId edgeList itemEdges) original replacement =
    let newEdges = replaceEdges edgeList original replacement
     in Room rId newEdges itemEdges

replaceEdges :: [Edge] -> Int -> Int -> [Edge]
replaceEdges [] _ _ = []
replaceEdges ((Edge p roomId):rest) orig replace =
    if roomId `elem` elevatorRooms
        then Edge p replace : replaceEdges rest orig replace
        else Edge p roomId : replaceEdges rest orig replace

elevatorRooms :: [Int]
elevatorRooms = map getRoomMapKey [RTransporttoTallonOverworldNorth ,RTransporttoMagmoorCavernsNorth ,RTransporttoTallonOverworldEast ,RTransporttoTallonOverworldSouth
    ,DTransporttoMagmoorCavernsWest ,DTransporttoMagmoorCavernsSouth ,OTransporttoChozoRuinsWest ,OTransporttoChozoRuinsEast ,OTransporttoMagmoorCavernsEast 
    ,OTransporttoChozoRuinsSouth ,OTransporttoPhazonMinesEast,MTransporttoTallonOverworldSouth ,MTransporttoMagmoorCavernsSouth ,CTransporttoChozoRuinsNorth
    ,CTransporttoPhendranaDriftsNorth ,CTransporttoTallonOverworldWest ,CTransporttoPhazonMinesWest ,CTransporttoPhendranaDriftsSouth]

pseudoItems :: [Item]
pseudoItems = [Item (getItemMapKey FrigatePowerDoorTrigger) FrigatePowerDoor (getRoomMapKey OMainVentilationShaftSectionB)
            ,Item (getItemMapKey MainQuarryBarrierTrigger) MainQuarryBarrier (getRoomMapKey MMainQuarry)
            ,Item (getItemMapKey MainQuarrySaveTrigger) MainQuarrySaveUnlocked (getRoomMapKey MSaveStationMinesA)
            ,Item (getItemMapKey ChozoIceTempleTrigger) ChozoIceTempleBarrier (getRoomMapKey DChozoIceTemple)
            ,Item (getItemMapKey StorageDepotATrigger) StorageDepotABarrier (getRoomMapKey MMineSecurityStation)
            ,Item (getItemMapKey ResearchLabHydraTrigger) ResearchLabHydraBarrier (getRoomMapKey DResearchLabHydra)
            ,Item (getItemMapKey EliteControlTrigger) EliteControlBarrier (getRoomMapKey MEliteControl)
            ,Item (getItemMapKey MetroidQuarantineATrigger) MetroidQuarantineABarrier (getRoomMapKey MMetroidQuarantineA)
            ,Item (getItemMapKey MetroidQuarantineBTrigger) MetroidQuarantineBBarrier (getRoomMapKey MMetroidQuarantineB)
            ,Item (getItemMapKey OmegaPirateEntranceTrigger) OmegaPirateEntranceBarrier (getRoomMapKey MEliteQuartersAccess)]

pseudoItemNames :: [ItemName]
pseudoItemNames = map itemName pseudoItems

{-- This creates all rooms to add to the graph --}
buildNodes :: Difficulty -> [Room]
buildNodes diff = [ -- Tallon Overworld Rooms
            room OLandingSite [edge OCanyonCavern noReq
                                    ,edge OWaterfallCavern noReq
                                    ,edge OGully $! sjf diff
                                    ,edge OAlcove $! sjf diff
                                    ,edge OTempleHall noReq]
                                    [itemEdge LandingSite morph]
            ,room OAlcove [edge OLandingSite noReq]
                                    [itemEdge Alcove noReq]
            ,room OCanyonCavern [edge OLandingSite noReq
                                    ,edge OTallonCanyon noReq
                                    ,edge OTallonFrontSw $! tallonCanyonSw diff] []
            ,room OTallonCanyon [edge OCanyonCavern noReq
                                    ,edge OGully boostBombs
                                    ,edge ORootTunnel noReq
                                    ,edge OTransportTunnelA noReq] []
            ,room OGully [edge OTallonCanyon bombs
                                    ,edge OLandingSite noReq] []
            ,room ORootTunnel [edge OTallonCanyon noReq
                                    ,edge ORootCave missile] []
            ,room ORootCave [edge ORootTunnel missile
                                    ,edge OTransportTunnelB noReq
                                    ,edge OArborChamber $! arbor diff]
                                    [itemEdge RootCave $! rootCaveItem diff]
            ,room OTransportTunnelB [edge ORootCave noReq
                                    ,edge OTransporttoMagmoorCavernsEast noReq]
                                    [itemEdge TransportTunnelB noReq]
            ,room OTransporttoMagmoorCavernsEast [edge CTransporttoTallonOverworldWest noReq
                                    ,edge OTransportTunnelB noReq] []
            ,room OArborChamber [edge ORootCave noReq]
                                    [itemEdge ArborChamber noReq]
            ,room OTransportTunnelA [edge OTallonCanyon noReq
                                    ,edge OTransporttoChozoRuinsWest noReq] []
            ,room OTransporttoChozoRuinsWest [edge OTransportTunnelA noReq
                                    ,edge RTransporttoTallonOverworldNorth noReq] []
            ,room OWaterfallCavern [edge OLandingSite noReq
                                    ,edge OFrigateCrashSite morphMissile] []
            ,room OFrigateCrashSite [edge OWaterfallCavern noReq
                                    ,edge OOvergrownCavern $! fcsClimb diff
                                    ,edge OFrigateAccessTunnel $! fcsEntry diff]
                                    [itemEdge FrigateCrashSite $! fcsItem diff]
            ,room OOvergrownCavern [edge OFrigateCrashSite ice
                                    ,edge OTransportTunnelC ice]
                                    [itemEdge OvergrownCavern morph]
            ,room OTransportTunnelC [edge OOvergrownCavern ice
                                    ,edge OTransporttoChozoRuinsEast ice] []
            ,room OTransporttoChozoRuinsEast [edge RTransporttoTallonOverworldEast noReq
                                    ,edge OTransportTunnelC ice] []
            ,room OFrigateAccessTunnel [edge OFrigateCrashSite ice
                                    ,edge OMainVentilationShaftSectionC noReq] []
            ,room OMainVentilationShaftSectionC [edge OFrigateAccessTunnel noReq
                                    ,edge OMainVentilationShaftSectionB noReq] []
            ,room OMainVentilationShaftSectionB [edge OMainVentilationShaftSectionA wave
                                    ,edge OMainVentilationShaftSectionC $! climbFrigateMvs diff]
                                    [itemEdge FrigatePowerDoorTrigger wave]
            ,room OMainVentilationShaftSectionA [edge OMainVentilationShaftSectionB $! frigatePowerDoor diff
                                    ,edge OReactorCore noReq] []
            ,room OReactorCore [edge OMainVentilationShaftSectionA $! climbReactorCore diff
                                    ,edge OReactorAccess wave] []
            ,room OReactorAccess [edge OCargoFreightLifttoDeckGamma wave
                                    ,edge OReactorCore noReq
                                    ,edge OSaveStation noReq] []
            ,room OSaveStation [edge OReactorAccess noReq] []
            ,room OCargoFreightLifttoDeckGamma [edge ODeckBetaTransitHall $! cargoFreightLift diff
                                    ,edge OReactorAccess noReq]
                                    [itemEdge CargoFreightLifttoDeckGamma missile]
            ,room ODeckBetaTransitHall [edge OCargoFreightLifttoDeckGamma noReq
                                    ,edge OBiohazardContainment noReq] []
            ,room OBiohazardContainment [edge ODeckBetaTransitHall noReq
                                    ,edge ODeckBetaSecurityHall $! biohazard diff]
                                    [itemEdge BiohazardContainment supers]
            ,room ODeckBetaSecurityHall [edge OBiohazardContainment $! climbBiohazard diff
                                    ,edge OBiotechResearchArea1 noReq]
                                    [itemEdge BiohazardContainment supers]
            ,room OBiotechResearchArea1 [edge ODeckBetaSecurityHall noReq
                                    ,edge ODeckBetaConduitHall $! biotech diff] []
            ,room ODeckBetaConduitHall [edge OBiotechResearchArea1 $! biotechReverse diff
                                    ,edge OConnectionElevatortoDeckBeta noReq] []
            ,room OConnectionElevatortoDeckBeta [edge ODeckBetaConduitHall noReq
                                    ,edge OHydroAccessTunnel noReq] []
            ,room OHydroAccessTunnel [edge OConnectionElevatortoDeckBeta gravSpace
                                    ,edge OGreatTreeHall $! hydroTunnel diff]
                                    [itemEdge HydroAccessTunnel morph]
            --Great Tree Hall is split into two rooms, an upper and lower section
            ,room OGreatTreeHall [edge OHydroAccessTunnel $! hydroTunnel diff
                                    ,edge OTransportTunnelE ice
                                    ,edge OGreatTreeHallTop $! gthClimb diff] []
            ,room OGreatTreeHallTop [edge OTransportTunnelD ice
                                    ,edge OGreatTreeChamber $! gtcEnter diff
                                    ,edge OLifeGroveTunnel $! gthSpiderTrack diff
                                    ,edge OGreatTreeHall $! bars diff] []
            ,room OTransportTunnelD [edge OGreatTreeHallTop ice
                                    ,edge OTransporttoChozoRuinsSouth ice] []
            ,room OTransporttoChozoRuinsSouth [edge OTransportTunnelD ice
                                    ,edge RTransporttoTallonOverworldSouth noReq] []
            ,room OGreatTreeChamber [edge OGreatTreeHallTop noReq
                                    ,edge OTallonBackSw $! gtcSw diff]
                                    [itemEdge GreatTreeChamber noReq]
            ,room OLifeGroveTunnel [edge OGreatTreeHallTop noReq
                                    ,edge OLifeGrove $! lifeGroveTunnel diff]
                                    [itemEdge LifeGroveTunnel $! lifeGroveTunnelItem diff]
            ,room OLifeGrove [edge OLifeGroveTunnel morph]
                                    [itemEdge LifeGroveStart noReq
                                    ,itemEdge LifeGroveUnderwaterSpinner $! lgUnderWater diff]
            ,room OTransportTunnelE [edge OTransporttoPhazonMinesEast ice
                                    ,edge OGreatTreeHall ice] []
            ,room OTransporttoPhazonMinesEast [edge OTransportTunnelE ice
                                    ,edge MTransporttoTallonOverworldSouth noReq] []
            ,room OTempleHall [edge OLandingSite noReq
                                    ,edge OTempleSecurityStation noReq] []
            ,room OTempleSecurityStation [edge OTempleLobby missile
                                    ,edge OTempleHall noReq] []
            ,room OTempleLobby [edge OTempleSecurityStation missile
                                    ,edge OArtifactTemple noReq] []
            ,room OArtifactTemple [edge OTempleLobby noReq]
                                    [itemEdge ArtifactTemple noReq]
            ,room OTallonBackSw [edge OLifeGrove bombs
                                    ,edge OGreatTreeHallTop bombs
                                    ,edge ODeckBetaConduitHall $! wallcrawlIntoFrigate diff]
                                    [itemEdge LifeGroveUnderwaterSpinner bombs]
            ,room OTallonFrontSw [edge OFrigateCrashSite bombs
                                    ,edge OTallonCanyon bombs]
                                    [itemEdge ArborChamber bombs
                                    ,itemEdge RootCave bombs]

            -- Chozo Ruins Rooms
            ,room RTransporttoTallonOverworldNorth [edge OTransporttoChozoRuinsWest noReq
                                    ,edge RRuinsEntrance noReq] []
            ,room RRuinsEntrance [edge RTransporttoTallonOverworldNorth noReq
                                    ,edge RMainPlaza noReq] []
            ,room RMainPlaza [edge RRuinsEntrance noReq
                                    ,edge RRuinedFountainAccess morph
                                    ,edge RRuinedShrineAccess missile
                                    ,edge RNurseryAccess noReq
                                    ,edge RPistonTunnelInbounds $! mainPlazaGrappleLedge diff
                                    ,edge RMainPlazaLedge $! mainPlazaLedge diff
                                    ,edge RChozoFrontSw $! mainPlazaSw diff]
                                    [itemEdge MainPlazaHalfPipe $! mainPipe diff
                                    ,itemEdge MainPlazaGrappleLedge $! mainPlazaGrappleLedge diff
                                    ,itemEdge MainPlazaTree supers]
            --Created new room to hold the main plaza ledge item, and allow one-way traversal through Vault
            ,room RMainPlazaLedge [edge RMainPlaza noReq]
                                    [itemEdge MainPlazaLockedDoor noReq] 
            ,room RPlazaAccess [edge RVault noReq
                                    ,edge RMainPlazaLedge noReq] []
            ,room RVault [edge RPlazaAccess noReq
                                    ,edge RVaultAccess morph]
                                    [itemEdge Vault bombs]
            ,room RVaultAccess [edge RVault morph
                                    ,edge RTransporttoMagmoorCavernsNorth noReq] []
            ,room RTransporttoMagmoorCavernsNorth [edge RVaultAccess noReq
                                    ,edge CTransporttoChozoRuinsNorth noReq
                                    ,edge RSunTower $! climbSunTower diff
                                    ,edge RTransportAccessNorth morph] []
            ,room RTransportAccessNorth [edge RTransporttoMagmoorCavernsNorth morph
                                    ,edge RHiveTotem missile]
                                    [itemEdge TransportAccessNorth noReq]
            ,room RHiveTotem [edge RTransportAccessNorth missile
                                    ,edge RTotemAccess noReq]
                                    [itemEdge HiveTotem noReq]
            ,room RTotemAccess [edge RHiveTotem noReq
                                    ,edge RRuinedGallery noReq] []
            ,room RRuinedGallery [edge RTotemAccess noReq
                                    ,edge RMapStation missile
                                    ,edge RNorthAtrium noReq]
                                    [itemEdge RuinedGalleryMissileWall missile
                                    ,itemEdge RuinedGalleryTunnel bombs]
            ,room RMapStation [edge RRuinedGallery missile] []
            ,room RNorthAtrium [edge RRuinedGallery noReq
                                    ,edge RRuinedNursery noReq] []
            ,room RRuinedNursery [edge RNorthAtrium noReq
                                    ,edge RSaveStation1 noReq
                                    ,edge REyonTunnel noReq]
                                    [itemEdge RuinedNursery bombs]
            ,room RSaveStation1 [edge RRuinedNursery noReq] []
            ,room REyonTunnel [edge RRuinedNursery noReq
                                    ,edge RNurseryAccess noReq] []
            ,room RNurseryAccess [edge REyonTunnel noReq
                                    ,edge RMainPlaza noReq] []
            ,room RRuinedShrineAccess [edge RRuinedShrine noReq
                                    ,edge RMainPlaza missile] []
            ,room RRuinedShrine [edge RRuinedShrineAccess noReq
                                    ,edge RTowerofLightAccess $! tolAccess diff]
                                    [itemEdge RuinedShrineLowerTunnel bombs
                                    ,itemEdge RuinedShrineHalfPipe $! rsHalf diff
                                    ,itemEdge RuinedShrineBeetleBattle noReq]
            ,room RTowerofLightAccess [edge RRuinedShrine wave
                                    ,edge RTowerofLight wave] []
            ,room RTowerofLight [edge RTowerofLightAccess wave
                                    ,edge RTowerChamber $! towerChamber diff]
                                    [itemEdge TowerofLight $! towerOfLight diff]
            ,room RTowerChamber [edge RTowerofLight wave]
                                    [itemEdge TowerChamber noReq]
            ,room RRuinedFountainAccess [edge RRuinedFountainNonWarp noReq
                                    ,edge RMainPlaza morph] []
            -- The Ruined Fountain Warp puts you on top of the item and forces you to collect it
            ,room RRuinedFountainNonWarp [edge RRuinedFountain $! ruinedFountainItem diff
                                    ,edge RRuinedFountainAccess noReq
                                    ,edge RMeditationFountain noReq
                                    ,edge RArboretumAccess noReq] []
            ,room RRuinedFountain [edge RRuinedFountainNonWarp $! leaveRuinedFountainItem diff]
                                    [itemEdge RuinedFountain noReq]
            ,room RMeditationFountain [edge RRuinedFountainNonWarp noReq
                                    ,edge RMagmaPool noReq] []
            ,room RMagmaPool [edge RMeditationFountain noReq
                                    ,edge RTrainingChamberAccess $! crossMagmaPool diff]
                                    [itemEdge MagmaPool $! magmaPoolItem diff]
            ,room RTrainingChamberAccess [edge RMagmaPool $! crossMagmaPool diff
                                    ,edge RTrainingChamber wave]
                                    [itemEdge MagmaPool wavePb
                                    ,itemEdge TrainingChamberAccess morph]
            ,room RTrainingChamber [edge RTowerofLightAccess wave
                                    ,edge RPistonTunnelInbounds $! tcTunnel diff]
                                    [itemEdge TrainingChamber $! tcItem diff]
            ,room RPistonTunnelInbounds [edge RMainPlaza morph
                                    ,edge RTrainingChamber blocked] [] -- Since it is blocked initially, it's simpler to consider it one-way
            ,room RPistonTunnel [edge RPistonTunnelInbounds morph -- If you jump after being warped here, you go oob
                                    ,edge RChozoFrontSw $! wallcrawl diff] []
            ,room RArboretumAccess [edge RRuinedFountainNonWarp noReq
                                    ,edge RArboretum missile] []
            ,room RArboretum [edge RArboretumAccess missile
                                    ,edge RSunchamberLobby bombs
                                    ,edge RGatheringHallAccess missile] []
            ,room RSunchamberLobby [edge RArboretum missile
                                    ,edge RSunchamberAccess noReq] []
            ,room RSunchamberAccess [edge RSunchamberLobby noReq
                                    ,edge RSunchamber noVines] []
            ,room RSunchamber [edge RSunchamberAccess noVines
                                    ,edge RSunTowerAccess noReq]
                                    [itemEdge SunchamberFlaahgra bombs
                                    ,itemEdge SunchamberGhosts $! sunchamberghost diff]
            ,room RSunTowerAccess [edge RSunchamber noReq
                                    ,edge RSunTower noReq] []
            -- The spawn point is at the top of the room, so to approximate this, items are required to enter the room from the elevator
            ,room RSunTower [edge RSunTowerAccess noReq
                                    ,edge RTransporttoMagmoorCavernsNorth noReq] []
            ,room RGatheringHallAccess [edge RArboretum missile
                                    ,edge RGatheringHall noReq] []
            ,room RGatheringHall [edge RGatheringHallAccess noReq
                                    ,edge RSaveStation2 missile
                                    ,edge RWateryHallAccess noReq
                                    ,edge REastAtrium morph
                                    ,edge RChozoBackSw $! gatheringHallSw diff]
                                    [itemEdge GatheringHall bombs]
            ,room RWateryHallAccess [edge RGatheringHall noReq
                                    ,edge RWateryHall missile]
                                    [itemEdge WateryHallAccess missile]
            ,room RWateryHall [edge RWateryHallAccess missile
                                    ,edge RDynamoAccess $! wateryHallTraverse diff
                                    ,edge RChozoBackSw $! wateryHallSw diff]
                                    [itemEdge WateryHallUnderwater $! wateryHallWater diff
                                    ,itemEdge WateryHallScanPuzzle noReq]
            ,room RDynamoAccess [edge RWateryHall missile
                                    ,edge RDynamo missile] []
            ,room RDynamo [edge RDynamoAccess missile]
                                    [itemEdge DynamoLower missile
                                    ,itemEdge DynamoSpiderTrack spider]
            ,room RSaveStation2 [edge RGatheringHall missile] []
            ,room REastAtrium [edge RGatheringHall noReq
                                    ,edge REnergyCoreAccess noReq] []
            ,room REnergyCoreAccess [edge REastAtrium noReq
                                    ,edge REnergyCore noReq] []
            ,room REnergyCore [edge REnergyCoreAccess noReq
                                    ,edge RBurnDomeAccess morph
                                    ,edge RWestFurnaceAccess bombs] []
            ,room RBurnDomeAccess [edge REnergyCore bombs
                                    ,edge RBurnDome morph] []
            ,room RBurnDome [edge RBurnDomeAccess noReq]
                                    [itemEdge BurnDomeMissile bombs
                                    ,itemEdge BurnDomeIDrone noReq]
            ,room RWestFurnaceAccess [edge REnergyCore noReq
                                    ,edge RFurnaceFront noReq] []
            ,room RFurnaceFront [edge RWestFurnaceAccess noReq
                                    ,edge RFurnace $! furnaceTraverse diff]
                                    [itemEdge FurnaceInsideFurnace bombs]
            ,room RFurnace [edge RFurnaceFront bombs
                                    ,edge RCrosswayAccessWest morph
                                    ,edge REastFurnaceAccess ice]
                                    [itemEdge FurnaceSpiderTracks $! furnaceItem diff]
            ,room REastFurnaceAccess [edge RFurnace ice
                                    ,edge RHalloftheElders ice] []
            ,room RCrosswayAccessWest [edge RFurnace morph
                                    ,edge RCrossway wave] []
            ,room RCrossway [edge RCrosswayAccessWest noReq
                                    ,edge RElderHallAccess $! crosswayTraverse diff
                                    ,edge RCrosswayAccessSouth ice]
                                    [itemEdge Crossway $! crosswayItem diff]
            ,room RElderHallAccess [edge RCrossway missile
                                    ,edge RHalloftheElders noReq] []
            ,room RCrosswayAccessSouth [edge RCrossway ice
                                    ,edge RHalloftheElders ice] []
            ,room RHalloftheElders [edge RCrosswayAccessSouth ice
                                    ,edge REastFurnaceAccess ice
                                    ,edge RElderHallAccess sjOrBombs
                                    ,edge RReflectingPoolAccess $! hoteWave diff
                                    ,edge RElderChamber $! hotePlasma diff]
                                    [itemEdge HalloftheElders $! hoteIce diff]
            ,room RElderChamber [edge RHalloftheElders $! elderChamberExit diff]
                                    [itemEdge ElderChamber noReq]
            ,room RReflectingPoolAccess [edge RHalloftheElders noReq
                                    ,edge RReflectingPool noReq] []
            ,room RReflectingPool [edge RReflectingPoolAccess noReq
                                    ,edge RSaveStation3 $! reflectPoolSave diff
                                    ,edge RAntechamber $! reflectPoolAntechamber diff
                                    ,edge RTransportAccessSouth $! reflectPoolIceDoor diff] []
            ,room RAntechamber [edge RReflectingPool ice]
                                    [itemEdge Antechamber noReq]
            ,room RTransportAccessSouth [edge RReflectingPool ice
                                    ,edge RTransporttoTallonOverworldSouth noReq] []
            ,room RTransporttoTallonOverworldSouth [edge RTransportAccessSouth noReq
                                    ,edge OTransporttoChozoRuinsSouth noReq] []
            ,room RSaveStation3 [edge RReflectingPool missile
                                    ,edge RTransporttoTallonOverworldEast bombs] []
            ,room RTransporttoTallonOverworldEast [edge RSaveStation3 bombs
                                    ,edge OTransporttoChozoRuinsEast noReq] []
            ,room RChozoBackSw [edge RReflectingPool bombs
                                    ,edge RChozoFrontSw $! longWallcrawl diff]
                                    [itemEdge HalloftheElders bombs
                                    ,itemEdge ElderChamber $! crosswayInfiniteSpeed diff]
            ,room RChozoFrontSw [] [itemEdge TrainingChamber bombs
                                    ,itemEdge MainPlazaGrappleLedge bombs
                                    ,itemEdge TrainingChamberAccess bombs
                                    ,itemEdge TowerofLight bombs]
            
            --Magmoor Caverns Rooms
            ,room CTransporttoChozoRuinsNorth [edge RTransporttoMagmoorCavernsNorth noReq
                                    ,edge CBurningTrail noReq] []
            ,room CBurningTrail [edge CTransporttoChozoRuinsNorth noReq
                                    ,edge CSaveStationMagmoorA missile
                                    ,edge CLakeTunnel noReq
                                    ,edge CMagmoorFrontSw $! burningTrailSw diff] []
            ,room CSaveStationMagmoorA [edge CBurningTrail missile] []
            ,room CLakeTunnel [edge CBurningTrail noReq
                                    ,edge CLavaLake noReq] []
            ,room CLavaLake [edge CLakeTunnel noReq
                                    ,edge CPitTunnel $! lavaLakeTraversal diff]
                                    [itemEdge LavaLake $! lavaLakeItem diff]
            ,room CPitTunnel [edge CLavaLake $! lavaLakeReverseTraversal diff
                                    ,edge CTriclopsPit $! pitTunnel diff] []
            ,room CTriclopsPit [edge CPitTunnel $! pitTunnelReverse diff
                                    ,edge CStorageCavern $! storageCavern diff
                                    ,edge CMonitorTunnel $! heatResistOr8Etanks diff] -- This has a high requirement to discourage this path to get to phendrana
                                    [itemEdge TriclopsPit $! triclopsPitItem diff]
            ,room CStorageCavern [edge CTriclopsPit $! vmr2Tank diff]
                                    [itemEdge StorageCavern noReq]
            ,room CMonitorTunnel [edge CTriclopsPit $! vmr2Tank diff
                                    ,edge CMonitorStation $! vmr2Tank diff] []
            ,room CMonitorStation [edge CMonitorTunnel $! vmr4Tank diff -- This requirement is excessive if warped to MonitorStation, going to storage cavern
                                    ,edge CShoreTunnel $! vmr3Tank diff
                                    ,edge CTransportTunnelA $! toTransportTunnelA diff
                                    ,edge CWarriorShrine $! monitorStationClimb diff] []
            ,room CTransportTunnelA [edge CMonitorStation bombs
                                    ,edge CTransporttoPhendranaDriftsNorth noReq]
                                    [itemEdge TransportTunnelA bombs]
            ,room CTransporttoPhendranaDriftsNorth [edge CTransportTunnelA noReq
                                    ,edge DTransporttoMagmoorCavernsWest noReq] []
            ,room CWarriorShrine [edge CMonitorStation $! vmr2Tank diff
                                    ,edge CFieryShores $! warriorShrineTunnel diff]
                                    [itemEdge WarriorShrine noReq
                                    ,itemEdge FieryShoresWarriorShrineTunnel $! warriorShrineTunnel diff]
            ,room CShoreTunnel [edge CMonitorStation $! vmr2Tank diff
                                    ,edge CFieryShores $! vmr2Tank diff]
                                    [itemEdge ShoreTunnel pb]
            ,room CFieryShores [edge CShoreTunnel $! vmr3Tank diff
                                    ,edge CTransportTunnelB $! vmr1Tank diff]
                                    [itemEdge FieryShoresMorphTrack bombs]
            ,room CTransportTunnelB [edge CFieryShores $! vmr4Tank diff
                                    ,edge CTransporttoTallonOverworldWest noReq] []
            ,room CTransporttoTallonOverworldWest [edge CTransportTunnelB $! vmr4Tank diff
                                    ,edge OTransporttoMagmoorCavernsEast noReq
                                    ,edge CTwinFiresTunnel $! crossTft diff] []
            ,room CTwinFiresTunnel [edge CTransporttoTallonOverworldWest $! crossTftReverse diff
                                    ,edge CTwinFires noReq] []
            ,room CTwinFires [edge CTwinFiresTunnel noReq
                                    ,edge CNorthCoreTunnel $! crossTwinFires diff] []
            ,room CNorthCoreTunnel [edge CTwinFires $! crossTwinFires diff
                                    ,edge CGeothermalCore $! crossNorthCoreTunnel diff] []
            ,room CGeothermalCore [edge CNorthCoreTunnel $! crossNorthCoreTunnel diff
                                    ,edge CSouthCoreTunnel noReq
                                    ,edge CPlasmaProcessing $! geoCore diff] []
            ,room CPlasmaProcessing [edge CGeothermalCore plasma]
                                    [itemEdge PlasmaProcessing noReq]
            ,room CSouthCoreTunnel [edge CGeothermalCore wave
                                    ,edge CMagmoorWorkstation wave] []
            ,room CMagmoorWorkstation [edge CSouthCoreTunnel noReq
                                    ,edge CWorkstationTunnel sjOrBombs
                                    ,edge CTransportTunnelC $! workstationWaveDoor diff
                                    ,edge CMagmoorBackSw $! workstationSw diff]
                                    [itemEdge MagmoorWorkstation $! workstationItem diff]
            ,room CTransportTunnelC [edge CMagmoorWorkstation wave
                                    ,edge CTransporttoPhendranaDriftsSouth wave] []
            ,room CTransporttoPhendranaDriftsSouth [edge CTransportTunnelC wave
                                    ,edge CSaveStationMagmoorB missile
                                    ,edge DTransporttoMagmoorCavernsSouth noReq] []
            ,room CSaveStationMagmoorB [edge CTransporttoPhendranaDriftsSouth missile] []
            ,room CWorkstationTunnel [edge CMagmoorWorkstation noReq
                                    ,edge CTransporttoPhazonMinesWest $! workstationTunnel diff] []
            ,room CTransporttoPhazonMinesWest [edge CWorkstationTunnel $! workstationTunnel diff
                                    ,edge MTransporttoMagmoorCavernsSouth noReq] []
            ,room CMagmoorBackSw [edge CTransporttoPhazonMinesWest bombs
                                    ,edge CMagmoorFrontSw $! longWallcrawl diff]
                                    [itemEdge PlasmaProcessing bombs]
            ,room CMagmoorFrontSw [edge CMagmoorBackSw $! magmoorFrontWallcrawl diff] []

            -- Phendrana Drifts Rooms
            ,room DTransporttoMagmoorCavernsWest [edge CTransporttoPhendranaDriftsNorth noReq
                                    ,edge DShorelineEntrance noReq] []
            ,room DShorelineEntrance [edge DTransporttoMagmoorCavernsWest noReq
                                    ,edge DPhendranaShorelines $! iceBarrier diff] []
            ,room DPhendranaShorelines [edge DShorelineEntrance $! iceBarrier diff
                                    ,edge DSaveStationB noReq
                                    ,edge DIceRuinsAccess noReq
                                    ,edge DPhendranaShorelinesUpper $! climbShorelines diff
                                    ,edge DTempleEntryway $! climbShorelines diff]
                                    [itemEdge PhendranaShorelinesBehindIce $! shorelinesItem diff
                                    ,itemEdge PhendranaShorelinesSpiderTrack $! shorelinesTower diff]
            ,room DPhendranaShorelinesUpper [edge DPlazaWalkway noReq
                                    ,edge DRuinsEntryway noReq
                                    ,edge DPhendranaShorelines noReq] []
            ,room DSaveStationB [edge DPhendranaShorelines noReq] []
            ,room DTempleEntryway [edge DPhendranaShorelines noReq
                                    ,edge DChozoIceTemple $! iceBarrier diff] []
            ,room DChozoIceTemple [edge DTempleEntryway $! iceBarrier diff
                                    ,edge DChapelTunnel $! iceTempleClimb diff]
                                    [itemEdge ChozoIceTempleTrigger $! iceTempleClimb diff
                                    ,itemEdge ChozoIceTemple $! iceTempleItem diff]
            ,room DChapelTunnel [edge DChozoIceTemple chozoIceTempleBarrier
                                    ,edge DChapeloftheElders noReq] []-- Warp point is near Chapel of the Elders
            ,room DChapeloftheElders [edge DChapelTunnel wave]
                                    [itemEdge ChapeloftheElders missile]
            ,room DIceRuinsAccess [edge DPhendranaShorelines noReq
                                    ,edge DIceRuinsEast $! iceBarrier diff] []
            ,room DIceRuinsEast [edge DIceRuinsAccess $! iceBarrier diff
                                    ,edge DPlazaWalkway noReq]
                                    [itemEdge IceRuinsEastSpiderTrack $! ireSpiderTrack diff
                                    ,itemEdge IceRuinsEastBehindIce plasma]
            ,room DPlazaWalkway [edge DIceRuinsEast noReq
                                    ,edge DPhendranaShorelinesUpper noReq] []
            ,room DRuinsEntryway [edge DPhendranaShorelinesUpper noReq
                                    ,edge DIceRuinsWest noReq] []
            ,room DIceRuinsWest [edge DRuinsEntryway noReq
                                    ,edge DCanyonEntryway missile
                                    ,edge DPhendranaFrontSw $! irwSw diff
                                    ,edge DCourtyardEntryway $! irwDoor diff]
                                    [itemEdge IceRuinsWest $! irwItem diff]
            ,room DCanyonEntryway [edge DIceRuinsWest noReq
                                    ,edge DPhendranaCanyon noReq] []
            ,room DPhendranaCanyon [edge DCanyonEntryway noReq]
                                    [itemEdge PhendranaCanyon noReq]
            ,room DCourtyardEntryway [edge DIceRuinsWest noReq
                                    ,edge DRuinedCourtyard $! ruinedCourtyardClimb diff] []-- Ruined courtyard spawn is at the top of the room
            ,room DRuinedCourtyard [edge DCourtyardEntryway noReq
                                    ,edge DSaveStationA $! ruinedCourtyardSave diff
                                    ,edge DSpecimenStorage wave
                                    ,edge DQuarantineAccess $! ruinedCourtyardConduit diff 
                                    ,edge DPhendranaFrontSw $! ruinedCourtyardSw diff]
                                    [itemEdge RuinedCourtyard morph]
            ,room DSaveStationA [edge DCourtyardEntryway missile -- If you fall
                                    ,edge DRuinedCourtyard $! ruinedCourtyardSave diff] -- If can make it to the spawn point
                                    [itemEdge RuinedCourtyard morph]-- You can grab the item by falling here, without reaching the warp
            ,room DQuarantineAccess [edge DRuinedCourtyard noReq
                                    ,edge DNorthQuarantineTunnel noReq] []
            ,room DNorthQuarantineTunnel [edge DQuarantineAccess wave
                                    ,edge DQuarantineCave $! quarantineTunnel diff] []
            ,room DQuarantineCave [edge DNorthQuarantineTunnel $! quarantineTunnel diff
                                    ,edge DQuarantineCaveBack $! climbQuarantineCaveBack diff]
                                    [itemEdge QuarantineCave noReq]
            -- Added a new "room" representing the other door in quarantine cave
            ,room DQuarantineCaveBack [edge DQuarantineMonitor $! quarantineMonitor diff
                                    ,edge DSouthQuarantineTunnel $! quarantineTunnel diff
                                    ,edge DQuarantineCave $! climbQuarantineCaveEntrance diff]
                                    [itemEdge QuarantineCave noReq -- Can drop into thardus fight
                                    ]
            ,room DQuarantineMonitor [edge DQuarantineCaveBack $! climbQuarantineCaveBack diff
                                    ,edge DQuarantineCave $! climbQuarantineCaveEntrance diff]
                                    [itemEdge QuarantineCave noReq -- Can drop into thardus fight
                                    ,itemEdge QuarantineMonitor noReq]
            ,room DSouthQuarantineTunnel [edge DQuarantineCaveBack $! quarantineTunnel diff
                                    ,edge DTransporttoMagmoorCavernsSouth wave] []
            ,room DTransporttoMagmoorCavernsSouth [edge DSouthQuarantineTunnel wave
                                    ,edge CTransporttoPhendranaDriftsSouth noReq
                                    ,edge DTransportAccess $! phenElevatorClimb diff] []
            ,room DTransportAccess [edge DTransporttoMagmoorCavernsSouth ice
                                    ,edge DFrozenPike wave]
                                    [itemEdge TransportAccess plasma]
            ,room DSpecimenStorage [edge DRuinedCourtyard wave
                                    ,edge DResearchEntrance wave] []
            ,room DResearchEntrance [edge DSpecimenStorage wave
                                    ,edge DMapStation noReq
                                    ,edge DHydraLabEntryway wave] []
            ,room DMapStation [edge DResearchEntrance noReq] []
            ,room DHydraLabEntryway [edge DResearchEntrance wave
                                    ,edge DResearchLabHydra wave] []
            ,room DResearchLabHydra [edge DHydraLabEntryway wave
                                    ,edge DResearchLabHydraBack noReq]
                                    [itemEdge ResearchLabHydraTrigger noReq]
            ,room DResearchLabHydraBack [edge DResearchLabHydra researchLabHydraBarrier
                                    ,edge DObservatoryAccess wave]
                                    [itemEdge ResearchLabHydra supers]
            ,room DObservatoryAccess [edge DResearchLabHydraBack wave
                                    ,edge DObservatory wave] []
            ,room DObservatory [edge DObservatoryAccess wave
                                    ,edge DObservatoryTop $! observatoryClimb diff] []
            ,room DObservatoryTop [edge DSaveStationD $! observatorySave diff
                                    ,edge DWestTowerEntrance wave
                                    ,edge DObservatory noReq]
                                    [itemEdge Observatory $! observatoryItem diff]
            ,room DSaveStationD [edge DObservatoryTop $! observatorySave diff] []
            ,room DWestTowerEntrance [edge DObservatoryTop wave
                                    ,edge DWestTower missile] []
            ,room DWestTower [edge DWestTowerEntrance missile
                                    ,edge DControlTower wave] []
            ,room DControlTower [edge DWestTower wave
                                    ,edge DEastTower wave]
                                    [itemEdge ControlTower $! controlTowerItem diff]
            ,room DEastTower [edge DControlTower wave
                                    ,edge DAetherLabEntryway wave] []
            ,room DAetherLabEntryway [edge DEastTower wave
                                    ,edge DResearchLabAether wave] []
            ,room DResearchLabAether [edge DAetherLabEntryway wave
                                    ,edge DResearchCoreAccess wave]
                                    [itemEdge ResearchLabAetherTank missile
                                    ,itemEdge ResearchLabAetherMorphTrack $! rlaTrack diff]
            ,room DResearchCoreAccess [edge DResearchLabAether wave
                                    ,edge DResearchCore wave] []
            ,room DResearchCore [edge DResearchCoreAccess wave
                                    ,edge DPikeAccess ice]
                                    [itemEdge ResearchCore noReq]
            ,room DPikeAccess [edge DResearchCore ice
                                    ,edge DFrozenPike wave] []
            ,room DFrozenPike [edge DTransportAccess $! frozenPikeClimb diff
                                    ,edge DPikeAccess wave
                                    ,edge DFrostCaveAccess wave
                                    ,edge DHunterCaveAccess $! frozenPikeBottom diff] []
            ,room DFrostCaveAccess [edge DFrozenPike wave
                                    ,edge DFrostCave $! frostCaveAccess diff] []
            ,room DFrostCave [edge DFrostCaveAccess $! frostCaveAccess diff
                                    ,edge DSaveStationC $! frostCaveDoor diff
                                    ,edge DUpperEdgeTunnel $! frostCaveToTunnel diff
                                    ,edge DPhendranaBackSw $! frostCaveSw diff]
                                    [itemEdge FrostCave $! frostCaveItem diff]
            ,room DSaveStationC [edge DFrostCave $! frostCaveDoor diff] []
            ,room DUpperEdgeTunnel [edge DFrostCave $! frostCaveAccess diff
                                    ,edge DPhendranasEdge wave] []
            ,room DPhendranasEdge [edge DUpperEdgeTunnel wave
                                    ,edge DStorageCave $! toStorageCave diff
                                    ,edge DSecurityCave $! toSecurityCave diff
                                    ,edge DLowerEdgeTunnel noReq] []
            ,room DStorageCave [edge DPhendranasEdge $! fromStorageCave diff]
                                    [itemEdge StorageCave noReq]
            ,room DSecurityCave [edge DPhendranasEdge morph]
                                    [itemEdge SecurityCave noReq]
            ,room DLowerEdgeTunnel [edge DPhendranasEdge $! phenEdgeLower diff
                                    ,edge DHunterCave wave] []
            ,room DHunterCave [edge DLowerEdgeTunnel wave
                                    ,edge DLakeTunnel $! hunterCaveLower diff
                                    ,edge DHunterCaveFar $! hunterCaveUpper diff] []
            ,room DHunterCaveFar [edge DHunterCave sjOrBombs
                                    ,edge DChamberAccess wave
                                    ,edge DHunterCaveAccess wave] []
            ,room DLakeTunnel [edge DHunterCave $! hunterCaveClimb diff
                                    ,edge DGravityChamber wave] []
            ,room DGravityChamber [edge DLakeTunnel $! gravityChamberToLakeTunnel diff
                                    ,edge DGravityChamberTop $! climbGravityChamber diff]
                                    [itemEdge GravityChamberUnderwater noReq]
            ,room DGravityChamberTop [edge DGravityChamber noReq
                                    ,edge DChamberAccess wave]
                                    [itemEdge GravityChamberGrappleLedge $! gravLedge diff]
            ,room DChamberAccess [edge DGravityChamberTop wave
                                    ,edge DHunterCaveFar wave] []
            ,room DHunterCaveAccess [edge DHunterCaveFar wave
                                    ,edge DFrozenPike $! frozenPikeBottom diff] []
            ,room DPhendranaFrontSw [edge DRuinedCourtyard bombs
                                    ,edge DPhendranaBackSw $! longWallcrawl diff]
                                    [itemEdge QuarantineMonitor bombs
                                    ,itemEdge QuarantineCave bombs]
            ,room DPhendranaBackSw [edge DPhendranaFrontSw $! longWallcrawl diff
                                    ,edge DFrostCave bombs
                                    ,edge DGravityChamber bombs
                                    ,edge DTransportAccess bombs
                                    ,edge DFrozenPike bombs]
                                    [itemEdge GravityChamberGrappleLedge bombs
                                    ,itemEdge TransportAccess $! transportAccessItemOob diff
                                    ,itemEdge SecurityCave bombs
                                    ,itemEdge StorageCave bombs]

            -- Phazon Mines Rooms
            ,room MTransporttoTallonOverworldSouth [edge OTransporttoPhazonMinesEast noReq
                                    ,edge MQuarryAccess wave] []
            ,room MQuarryAccess [edge MTransporttoTallonOverworldSouth wave
                                    ,edge MMainQuarry wave] []
            ,room MMainQuarry [edge MQuarryAccess wave
                                    ,edge MSaveStationMinesA $! quarrySave diff
                                    ,edge MWasteDisposal $! reachWasteDisposal diff
                                    ,edge MSecurityAccessA ice]
                                    [itemEdge MainQuarryBarrierTrigger noReq
                                    ,itemEdge MainQuarrySaveTrigger $! quarrySave diff
                                    ,itemEdge MainQuarry $! quarryItem diff]
            ,room MSaveStationMinesA [edge MMainQuarry mainQuarryBarrierWave] []
            ,room MSecurityAccessA [edge MMainQuarry mainQuarryBarrierIce
                                    ,edge MMineSecurityStation ice]
                                    [itemEdge SecurityAccessA pb]
            ,room MMineSecurityStation [edge MSecurityAccessA waveIce
                                    ,edge MStorageDepotA $! toStorageDepotA diff
                                    ,edge MSecurityAccessB wave]
                                    [itemEdge StorageDepotATrigger pb]
            ,room MStorageDepotA [edge MMineSecurityStation $! storageDepotABarrier diff]
                                    [itemEdge StorageDepotA noReq]
            ,room MSecurityAccessB [edge MMineSecurityStation wave
                                    ,edge MMinesFrontSw $! securityAccessBSw diff
                                    ,edge MEliteResearch ice] []
            ,room MEliteResearch [edge MSecurityAccessB ice
                                    ,edge MResearchAccess $! eliteResearchDoor diff]
                                    [itemEdge EliteResearchLaser $! eliteResearchTopItem diff
                                    ,itemEdge EliteResearchPhazonElite $! eliteResearchPirate diff]
            -- Currently require boosting through wall even if you can laser it
            ,room MResearchAccess [edge MEliteResearch $! shaftClimb2 diff
                                    ,edge MOreProcessingBottom ice] []
            ,room MOreProcessingBottom [edge MResearchAccess ice
                                    ,edge MElevatorAccessA $! oreProcessingClimb diff
                                    ,edge MWasteDisposal $! oreProcessingTop diff
                                    ,edge MStorageDepotB $! oreProcessingTop diff] []
            -- Spawn point is next to the pb rocks
            ,room MOreProcessing [edge MOreProcessingBottom noReq
                                    ,edge MElevatorAccessA $! dashFromPbRocks diff
                                    ,edge MOreProcessingTop $! oreProcessingTopFromRocks diff] []
            -- This fake room is considered to be at the top on the waste disposal side
            ,room MOreProcessingTop [edge MOreProcessing noReq
                                    ,edge MWasteDisposal ice
                                    ,edge MStorageDepotB $! oreProcessingCrossTop diff] []
            ,room MWasteDisposal [edge MOreProcessingTop ice
                                    ,edge MMainQuarry $! wasteDisposalTraversal diff] []
            ,room MStorageDepotB [edge MOreProcessingTop $! oreProcessingCrossTop diff
                                    ,edge MOreProcessing ice]
                                    [itemEdge StorageDepotB noReq]
            ,room MElevatorAccessA [edge MOreProcessing ice
                                    ,edge MOreProcessingTop $! oreProcessingTopFromEaa diff
                                    ,edge MElevatorA ice] []
            ,room MElevatorA [edge MElevatorAccessA $! shaftClimb1 diff
                                    ,edge MEliteControlAccess ice] []
            ,room MEliteControlAccess [edge MElevatorA ice
                                    ,edge MEliteControl wave]
                                    [itemEdge EliteControlAccess $! ecaItem diff]
            ,room MEliteControl [edge MEliteControlAccess wave
                                    ,edge MMaintenanceTunnel ice
                                    ,edge MVentilationShaft ice]
                                    [itemEdge EliteControlTrigger noReq]
            ,room MMaintenanceTunnel [edge MEliteControl ice
                                    ,edge MPhazonProcessingCenter $! maintTunnel diff] []
            ,room MPhazonProcessingCenter [edge MMaintenanceTunnel $! maintTunnel diff
                                    ,edge MProcessingCenterAccess blocked -- This barrier is considered to be one-way
                                    ,edge MTransportAccess $! ppcClimb diff]
                                    [itemEdge PhazonProcessingCenter pb]
            ,room MTransportAccess [edge MPhazonProcessingCenter ice
                                    ,edge MTransporttoMagmoorCavernsSouth $! toMinesElevator diff] []
            ,room MTransporttoMagmoorCavernsSouth [edge MTransportAccess $! toMinesElevator diff
                                    ,edge CTransporttoPhazonMinesWest noReq] []
            -- Warp is at the top
            ,room MVentilationShaft [edge MEliteControl eliteControlBarrier
                                    ,edge MOmegaResearch ice]
                                    [itemEdge VentilationShaft pb]
            ,room MOmegaResearch [edge MVentilationShaft ice
                                    ,edge MMapStationMines $! maintTunnel diff
                                    ,edge MDynamoAccess ice] []
            ,room MMapStationMines [edge MOmegaResearch $! maintTunnel diff] []
            ,room MDynamoAccess [edge MOmegaResearch ice
                                    ,edge MCentralDynamo ice] []
            -- Warp is the top, but treating as the bottom. It's slightly inaccurate
            ,room MCentralDynamo [edge MDynamoAccess $! centralDynamoClimb diff
                                    ,edge MSaveStationMinesB ice
                                    ,edge MQuarantineAccessA $! maintTunnel diff]
                                    [itemEdge CentralDynamo morph]
            ,room MSaveStationMinesB [edge MCentralDynamo ice] []
            ,room MQuarantineAccessA [edge MCentralDynamo $! maintTunnel diff
                                    ,edge MMetroidQuarantineA wave] []
            ,room MMetroidQuarantineA [edge MQuarantineAccessA wave
                                    ,edge MMetroidQuarantineABack noReq]
                                    [itemEdge MetroidQuarantineATrigger noReq]
            ,room MMetroidQuarantineABack [edge MMetroidQuarantineA mqaBarrier
                                    ,edge MElevatorAccessB $! mqaTraversal diff]
                                    [itemEdge MetroidQuarantineA $! mqaItem diff]
            ,room MElevatorAccessB [edge MMetroidQuarantineABack ice
                                    ,edge MElevatorB plasma] []
            ,room MElevatorB [edge MFungalHallAccess plasma
                                    ,edge MElevatorAccessB plasma] []
            ,room MFungalHallAccess [edge MElevatorB plasma
                                    ,edge MFungalHallA plasma]
                                    [itemEdge FungalHallAccess morph]
            ,room MFungalHallA [edge MFungalHallAccess $! climbFungalHallAccess diff
                                    ,edge MPhazonMiningTunnel $! fungalHallATraversal diff] []
            ,room MPhazonMiningTunnel [edge MFungalHallA plasma
                                    ,edge MFungalHallB $! miningTunnelTraversal diff]
                                    [itemEdge PhazonMiningTunnel $! miningTunnelItem diff]
            ,room MFungalHallB [edge MPhazonMiningTunnel $! miningTunnelTraversal diff
                                    ,edge MMissileStationMinesInbounds $! fungalHallBTraversal diff
                                    ,edge MQuarantineAccessB $! fungalHallBTraversal diff]
                                    [itemEdge FungalHallB bombs]
            ,room MMissileStationMinesInbounds [edge MFungalHallB plasma] []
            ,room MMissileStationMines [edge MMissileStationMinesInbounds morph -- You get warped out of bounds and need morph
                                    ,edge MMinesBackSw $! wallcrawl diff] []
            ,room MQuarantineAccessB [edge MFungalHallB plasma
                                    ,edge MMetroidQuarantineB $! quarantineAccessBTraversal diff] []
            ,room MMetroidQuarantineB [edge MQuarantineAccessB $! quarantineAccessBTraversal diff
                                    ,edge MMetroidQuarantineBBack $! mqbTraversal diff]
                                    [itemEdge MetroidQuarantineBTrigger noReq]
            ,room MMetroidQuarantineBBack [edge MMetroidQuarantineB mqbBarrier
                                    ,edge MSaveStationMinesC plasma
                                    ,edge MEliteQuartersAccess $! mqbBackClimb diff
                                    ,edge MMinesBackSw $! mqbSw diff]
                                    [itemEdge MetroidQuarantineB supers]
            ,room MSaveStationMinesC [edge MMetroidQuarantineBBack plasma] []
            ,room MEliteQuartersAccess [edge MMetroidQuarantineBBack plasma
                                    ,edge MEliteQuarters plasma]
                                    [itemEdge OmegaPirateEntranceTrigger plasma]
            ,room MEliteQuarters [edge MEliteQuartersAccess $! eliteQuartersExit diff
                                    ,edge MProcessingCenterAccess $! eliteQuartersTop diff]
                                    [itemEdge EliteQuarters $! eliteQuarters diff]
            ,room MProcessingCenterAccess [edge MEliteQuarters plasma
                                    ,edge MPhazonProcessingCenter $! ppcBottomClimb diff]
                                    [itemEdge ProcessingCenterAccess noReq]
            ,room MMinesFrontSw [edge MMainQuarry ice]
                                    [itemEdge StorageDepotA bombs
                                    ,itemEdge SecurityAccessA ice]
            ,room MMinesBackSw [edge MFungalHallB bombs
                                    ,edge MPhazonProcessingCenter bombs
                                    ,edge MMetroidQuarantineB bombs]
                                    [itemEdge FungalHallAccess $! longWallcrawl diff]
            ]
        where 
            room :: RoomId -> [Edge] -> [IEdge] -> Room
            room roomId = Room $! getRoomMapKey roomId
            
            edge :: RoomId -> (Map ItemName Int -> Set Int -> Bool) -> Edge
            edge roomId predicate = Edge predicate $! getRoomMapKey roomId

            itemEdge :: ItemId -> (Map ItemName Int -> Set Int -> Bool) -> IEdge
            itemEdge itemId predicate = IEdge predicate $! getItemMapKey itemId
