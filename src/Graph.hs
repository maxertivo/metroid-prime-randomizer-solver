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

buildMap :: [Node] -> Map Id Node
buildMap nodes = Map.fromList (map createMapKeys nodes)

createMapKeys :: Node -> (Id, Node)
createMapKeys node =
    case node of
        Item item _ _ -> (I item, node)
        Room room _ -> (R room, node)

replaceElevators :: Map Id Node -> [(RoomId, RoomId)] -> Map Id Node
replaceElevators graph [] = graph
replaceElevators graph ((a, b):rest) =
    let oldNode = getVal (Map.lookup (R a) graph) "Missing room"
        newNode = replaceEdgeRoom oldNode a b
     in replaceElevators (Map.insert (R a) newNode graph) rest

replaceEdgeRoom :: Node -> RoomId -> RoomId -> Node
replaceEdgeRoom (Room rId edgeList) original replacement =
    let newEdges = f edgeList original replacement
     in Room rId newEdges
  where
    f :: [Edge] -> RoomId -> RoomId -> [Edge]
    f [] _ _ = []
    f ((Edge p nodeId):rest) orig replace =
        case nodeId of
            (R roomId) ->
                if roomId `elem` elevatorRooms
                    then Edge p (R replace) : f rest orig replace
                    else Edge p (R roomId) : f rest orig replace
            _ -> Edge p nodeId : f rest orig replace
replaceEdgeRoom node _ _ = node

elevatorRooms :: [RoomId]
elevatorRooms = [RTransporttoTallonOverworldNorth ,RTransporttoMagmoorCavernsNorth ,RTransporttoTallonOverworldEast ,RTransporttoTallonOverworldSouth
    ,DTransporttoMagmoorCavernsWest ,DTransporttoMagmoorCavernsSouth ,OTransporttoChozoRuinsWest ,OTransporttoChozoRuinsEast ,OTransporttoMagmoorCavernsEast 
    ,OTransporttoChozoRuinsSouth ,OTransporttoPhazonMinesEast,MTransporttoTallonOverworldSouth ,MTransporttoMagmoorCavernsSouth ,CTransporttoChozoRuinsNorth
    ,CTransporttoPhendranaDriftsNorth ,CTransporttoTallonOverworldWest ,CTransporttoPhazonMinesWest ,CTransporttoPhendranaDriftsSouth]

{-- This creates all rooms and "pseudo-items" to add to the graph --}
buildNodes :: Difficulty -> [Node]
buildNodes diff = [ -- Tallon Overworld Rooms
            Room OLandingSite [Edge noReq (R OCanyonCavern)
                                    ,Edge noReq (R OWaterfallCavern)
                                    ,Edge (sjf diff) (R OGully)
                                    ,Edge (sjf diff) (R OAlcove)
                                    ,Edge noReq (R OTempleHall)
                                    ,Edge morph (I LandingSite)]
            ,Room OAlcove [Edge noReq (R OLandingSite)
                                    ,Edge noReq (I Alcove)]
            ,Room OCanyonCavern [Edge noReq (R OLandingSite)
                                    ,Edge noReq (R OTallonCanyon)
                                    ,Edge (tallonCanyonSw diff) (R OTallonFrontSw)]
            ,Room OTallonCanyon [Edge noReq (R OCanyonCavern)
                                    ,Edge boostBombs (R OGully)
                                    ,Edge noReq (R ORootTunnel)
                                    ,Edge noReq (R OTransportTunnelA)]
            ,Room OGully [Edge bombs (R OTallonCanyon)
                                    ,Edge noReq (R OLandingSite)]
            ,Room ORootTunnel [Edge noReq (R OTallonCanyon)
                                    ,Edge missile (R ORootCave)]
            ,Room ORootCave [Edge missile (R ORootTunnel)
                                    ,Edge noReq (R OTransportTunnelB)
                                    ,Edge (arbor diff) (R OArborChamber)
                                    ,Edge (rootCaveItem diff) (I RootCave)]
            ,Room OTransportTunnelB [Edge noReq (R ORootCave)
                                    ,Edge noReq (R OTransporttoMagmoorCavernsEast)
                                    ,Edge noReq (I TransportTunnelB)]
            ,Room OTransporttoMagmoorCavernsEast [Edge noReq (R CTransporttoTallonOverworldWest)
                                    ,Edge noReq (R OTransportTunnelB)]
            ,Room OArborChamber [Edge noReq (I ArborChamber)
                                    ,Edge noReq (R ORootCave)]
            ,Room OTransportTunnelA [Edge noReq (R OTallonCanyon)
                                    ,Edge noReq (R OTransporttoChozoRuinsWest)]
            ,Room OTransporttoChozoRuinsWest [Edge noReq (R OTransportTunnelA)
                                    ,Edge noReq (R RTransporttoTallonOverworldNorth)]
            ,Room OWaterfallCavern [Edge noReq (R OLandingSite)
                                    ,Edge morphMissile (R OFrigateCrashSite)]
            ,Room OFrigateCrashSite [Edge noReq (R OWaterfallCavern)
                                    ,Edge (fcsClimb diff) (R OOvergrownCavern)
                                    ,Edge (fcsEntry diff) (R OFrigateAccessTunnel)
                                    ,Edge (fcsItem diff) (I FrigateCrashSite)]
            ,Room OOvergrownCavern [Edge ice (R OFrigateCrashSite)
                                    ,Edge ice (R OTransportTunnelC)
                                    ,Edge morph (I OvergrownCavern)]
            ,Room OTransportTunnelC [Edge ice (R OOvergrownCavern)
                                    ,Edge ice (R OTransporttoChozoRuinsEast)]
            ,Room OTransporttoChozoRuinsEast [Edge noReq (R RTransporttoTallonOverworldEast)
                                    ,Edge ice (R OTransportTunnelC)]
            ,Room OFrigateAccessTunnel [Edge ice (R OFrigateCrashSite)
                                    ,Edge noReq (R OMainVentilationShaftSectionC)]
            ,Room OMainVentilationShaftSectionC [Edge noReq (R OFrigateAccessTunnel)
                                    ,Edge noReq (R OMainVentilationShaftSectionB)]
            ,Room OMainVentilationShaftSectionB [Edge wave (R OMainVentilationShaftSectionA)
                                    ,Edge (climbFrigateMvs diff) (R OMainVentilationShaftSectionC)
                                    ,Edge wave (I FrigatePowerDoorTrigger)]
            ,Room OMainVentilationShaftSectionA [Edge (frigatePowerDoor diff) (R OMainVentilationShaftSectionB)
                                    ,Edge noReq (R OReactorCore)]
            ,Room OReactorCore [Edge (climbReactorCore diff) (R OMainVentilationShaftSectionA)
                                    ,Edge wave (R OReactorAccess)]
            ,Room OReactorAccess [Edge wave (R OCargoFreightLifttoDeckGamma)
                                    ,Edge noReq (R OReactorCore)
                                    ,Edge noReq (R OSaveStation)]
            ,Room OSaveStation [Edge noReq (R OReactorAccess)]
            ,Room OCargoFreightLifttoDeckGamma [Edge (cargoFreightLift diff) (R ODeckBetaTransitHall)
                                    ,Edge noReq (R OReactorAccess)
                                    ,Edge missile (I CargoFreightLifttoDeckGamma)]
            ,Room ODeckBetaTransitHall [Edge noReq (R OCargoFreightLifttoDeckGamma)
                                    ,Edge noReq (R OBiohazardContainment)]
            ,Room OBiohazardContainment [Edge noReq (R ODeckBetaTransitHall)
                                    ,Edge (biohazard diff) (R ODeckBetaSecurityHall)
                                    ,Edge supers (I BiohazardContainment)]
            ,Room ODeckBetaSecurityHall [Edge (climbBiohazard diff) (R OBiohazardContainment)
                                    ,Edge noReq (R OBiotechResearchArea1)
                                    ,Edge supers (I BiohazardContainment)]
            ,Room OBiotechResearchArea1 [Edge noReq (R ODeckBetaSecurityHall)
                                    ,Edge (biotech diff) (R ODeckBetaConduitHall)]
            ,Room ODeckBetaConduitHall [Edge (biotechReverse diff) (R OBiotechResearchArea1)
                                    ,Edge noReq (R OConnectionElevatortoDeckBeta)]
            ,Room OConnectionElevatortoDeckBeta [Edge noReq (R ODeckBetaConduitHall)
                                    ,Edge noReq (R OHydroAccessTunnel)]
            ,Room OHydroAccessTunnel [Edge gravSpace (R OConnectionElevatortoDeckBeta)
                                    ,Edge (hydroTunnel diff) (R OGreatTreeHall)
                                    ,Edge morph (I HydroAccessTunnel)]
            --Great Tree Hall is split into two rooms, an upper and lower section
            ,Room OGreatTreeHall [Edge (hydroTunnel diff) (R OHydroAccessTunnel)
                                    ,Edge ice (R OTransportTunnelE)
                                    ,Edge (gthClimb diff) (R OGreatTreeHallTop)]
            ,Room OGreatTreeHallTop [Edge ice (R OTransportTunnelD)
                                    ,Edge (gtcEnter diff) (R OGreatTreeChamber)
                                    ,Edge (gthSpiderTrack diff) (R OLifeGroveTunnel)
                                    ,Edge (bars diff) (R OGreatTreeHall)]
            ,Room OTransportTunnelD [Edge ice (R OGreatTreeHallTop)
                                    ,Edge ice (R OTransporttoChozoRuinsSouth)]
            ,Room OTransporttoChozoRuinsSouth [Edge ice (R OTransportTunnelD)
                                    ,Edge noReq (R RTransporttoTallonOverworldSouth)]
            ,Room OGreatTreeChamber [Edge noReq (R OGreatTreeHallTop)
                                    ,Edge (gtcSw diff) (R OTallonBackSw)
                                    ,Edge noReq (I GreatTreeChamber)]
            ,Room OLifeGroveTunnel [Edge noReq (R OGreatTreeHallTop)
                                    ,Edge (lifeGroveTunnel diff) (R OLifeGrove)
                                    ,Edge (lifeGroveTunnelItem diff) (I LifeGroveTunnel)]
            ,Room OLifeGrove [Edge morph (R OLifeGroveTunnel)
                                    ,Edge noReq (I LifeGroveStart)
                                    ,Edge (lgUnderWater diff) (I LifeGroveUnderwaterSpinner)]
            ,Room OTransportTunnelE [Edge ice (R OTransporttoPhazonMinesEast)
                                    ,Edge ice (R OGreatTreeHall)]
            ,Room OTransporttoPhazonMinesEast [Edge ice (R OTransportTunnelE)
                                    ,Edge noReq (R MTransporttoTallonOverworldSouth)]
            ,Room OTempleHall [Edge noReq (R OLandingSite)
                                    ,Edge noReq (R OTempleSecurityStation)]
            ,Room OTempleSecurityStation [Edge missile (R OTempleLobby)
                                    ,Edge noReq (R OTempleHall)]
            ,Room OTempleLobby [Edge missile (R OTempleSecurityStation)
                                    ,Edge noReq (R OArtifactTemple)]
            ,Room OArtifactTemple [Edge noReq (R OTempleLobby)
                                    ,Edge noReq (I ArtifactTemple)]
            ,Room OTallonBackSw [Edge bombs (R OLifeGrove)
                                    ,Edge bombs (R OGreatTreeHallTop)
                                    ,Edge (wallcrawlIntoFrigate diff) (R ODeckBetaConduitHall)
                                    ,Edge bombs (I LifeGroveUnderwaterSpinner)]
            ,Room OTallonFrontSw [Edge bombs (R OFrigateCrashSite)
                                    ,Edge bombs (R OTallonCanyon)
                                    ,Edge bombs (I ArborChamber)
                                    ,Edge bombs (I RootCave)]

            -- Chozo Ruins Rooms
            ,Room RTransporttoTallonOverworldNorth [Edge noReq (R OTransporttoChozoRuinsWest)
                                    ,Edge noReq (R RRuinsEntrance)]
            ,Room RRuinsEntrance [Edge noReq (R RTransporttoTallonOverworldNorth)
                                    ,Edge noReq (R RMainPlaza)]
            ,Room RMainPlaza [Edge noReq (R RRuinsEntrance)
                                    ,Edge morph (R RRuinedFountainAccess)
                                    ,Edge missile (R RRuinedShrineAccess)
                                    ,Edge noReq (R RNurseryAccess)
                                    ,Edge (mainPlazaGrappleLedge diff) (R RPistonTunnelInbounds)
                                    ,Edge (mainPlazaLedge diff) (R RMainPlazaLedge)
                                    ,Edge (mainPlazaSw diff) (R RChozoFrontSw)
                                    ,Edge (mainPipe diff) (I MainPlazaHalfPipe)
                                    ,Edge (mainPlazaGrappleLedge diff) (I MainPlazaGrappleLedge)
                                    ,Edge supers (I MainPlazaTree)]
            --Created new room to hold the main plaza ledge item, and allow one-way traversal through Vault
            ,Room RMainPlazaLedge [Edge noReq (R RMainPlaza)
                                    ,Edge noReq (I MainPlazaLockedDoor)] 
            ,Room RPlazaAccess [Edge noReq (R RVault)
                                    ,Edge noReq (R RMainPlazaLedge)]
            ,Room RVault [Edge noReq (R RPlazaAccess)
                                    ,Edge morph (R RVaultAccess)
                                    ,Edge bombs (I Vault)]
            ,Room RVaultAccess [Edge morph (R RVault)
                                    ,Edge noReq (R RTransporttoMagmoorCavernsNorth)]
            ,Room RTransporttoMagmoorCavernsNorth [Edge noReq (R RVaultAccess)
                                    ,Edge noReq (R CTransporttoChozoRuinsNorth)
                                    ,Edge (climbSunTower diff) (R RSunTower)
                                    ,Edge morph (R RTransportAccessNorth)]
            ,Room RTransportAccessNorth [Edge morph (R RTransporttoMagmoorCavernsNorth)
                                    ,Edge missile (R RHiveTotem)
                                    ,Edge noReq (I TransportAccessNorth)]
            ,Room RHiveTotem [Edge missile (R RTransportAccessNorth)
                                    ,Edge noReq (R RTotemAccess)
                                    ,Edge noReq (I HiveTotem)]
            ,Room RTotemAccess [Edge noReq (R RHiveTotem)
                                    ,Edge noReq (R RRuinedGallery)]
            ,Room RRuinedGallery [Edge noReq (R RTotemAccess)
                                    ,Edge missile (R RMapStation)
                                    ,Edge noReq (R RNorthAtrium)
                                    ,Edge missile (I RuinedGalleryMissileWall)
                                    ,Edge bombs (I RuinedGalleryTunnel)]
            ,Room RMapStation [Edge missile (R RRuinedGallery)]
            ,Room RNorthAtrium [Edge noReq (R RRuinedGallery)
                                    ,Edge noReq (R RRuinedNursery)]
            ,Room RRuinedNursery [Edge noReq (R RNorthAtrium)
                                    ,Edge noReq (R RSaveStation1)
                                    ,Edge noReq (R REyonTunnel)
                                    ,Edge bombs (I RuinedNursery)]
            ,Room RSaveStation1 [Edge noReq (R RRuinedNursery)]
            ,Room REyonTunnel [Edge noReq (R RRuinedNursery)
                                    ,Edge noReq (R RNurseryAccess)]
            ,Room RNurseryAccess [Edge noReq (R REyonTunnel)
                                    ,Edge noReq (R RMainPlaza)]
            ,Room RRuinedShrineAccess [Edge noReq (R RRuinedShrine)
                                    ,Edge missile (R RMainPlaza)]
            ,Room RRuinedShrine [Edge noReq (R RRuinedShrineAccess)
                                    ,Edge (tolAccess diff) (R RTowerofLightAccess)
                                    ,Edge bombs (I RuinedShrineLowerTunnel)
                                    ,Edge (rsHalf diff) (I RuinedShrineHalfPipe)
                                    ,Edge noReq (I RuinedShrineBeetleBattle)]
            ,Room RTowerofLightAccess [Edge wave (R RRuinedShrine)
                                    ,Edge wave (R RTowerofLight)]
            ,Room RTowerofLight [Edge wave (R RTowerofLightAccess)
                                    ,Edge (towerChamber diff) (R RTowerChamber)
                                    ,Edge (towerOfLight diff) (I TowerofLight)]
            ,Room RTowerChamber [Edge wave (R RTowerofLight)
                                    ,Edge noReq (I TowerChamber)]
            ,Room RRuinedFountainAccess [Edge noReq (R RRuinedFountainNonWarp)
                                    ,Edge morph (R RMainPlaza)]
            -- The Ruined Fountain Warp puts you on top of the item and forces you to collect it
            ,Room RRuinedFountainNonWarp [Edge (ruinedFountainItem diff) (R RRuinedFountain)
                                    ,Edge noReq (R RRuinedFountainAccess)
                                    ,Edge noReq (R RMeditationFountain)
                                    ,Edge noReq (R RArboretumAccess)]
            ,Room RRuinedFountain [Edge (leaveRuinedFountainItem diff) (R RRuinedFountainNonWarp)
                                    ,Edge noReq (I RuinedFountain)]
            ,Room RMeditationFountain [Edge noReq (R RRuinedFountainNonWarp)
                                    ,Edge noReq (R RMagmaPool)]
            ,Room RMagmaPool [Edge noReq (R RMeditationFountain)
                                    ,Edge (crossMagmaPool diff) (R RTrainingChamberAccess)
                                    ,Edge (magmaPoolItem diff) (I MagmaPool)]
            ,Room RTrainingChamberAccess [Edge (crossMagmaPool diff) (R RMagmaPool)
                                    ,Edge wave (R RTrainingChamber)
                                    ,Edge wavePb (I MagmaPool)
                                    ,Edge morph (I TrainingChamberAccess)]
            ,Room RTrainingChamber [Edge wave (R RTowerofLightAccess)
                                    ,Edge (tcTunnel diff) (R RPistonTunnelInbounds)
                                    ,Edge (tcItem diff) (I TrainingChamber)]
            ,Room RPistonTunnelInbounds [Edge morph (R RMainPlaza)
                                    ,Edge blocked (R RTrainingChamber)] -- Since it is blocked initially, it's simpler to consider it one-way
            ,Room RPistonTunnel [Edge morph (R RPistonTunnelInbounds) -- If you jump after being warped here, you go oob
                                    ,Edge (wallcrawl diff) (R RChozoFrontSw)]
            ,Room RArboretumAccess [Edge noReq (R RRuinedFountainNonWarp)
                                    ,Edge missile (R RArboretum)]
            ,Room RArboretum [Edge missile (R RArboretumAccess)
                                    ,Edge bombs (R RSunchamberLobby)
                                    ,Edge missile (R RGatheringHallAccess)]
            ,Room RSunchamberLobby [Edge missile (R RArboretum)
                                    ,Edge noReq (R RSunchamberAccess)]
            ,Room RSunchamberAccess [Edge noReq (R RSunchamberLobby)
                                    ,Edge noVines (R RSunchamber)]
            ,Room RSunchamber [Edge noVines (R RSunchamberAccess)
                                    ,Edge noReq (R RSunTowerAccess)
                                    ,Edge bombs (I SunchamberFlaahgra)
                                    ,Edge (sunchamberghost diff) (I SunchamberGhosts)]
            ,Room RSunTowerAccess [Edge noReq (R RSunchamber)
                                    ,Edge noReq (R RSunTower)]
            -- The spawn point is at the top of the room, so to approximate this, items are required to enter the room from the elevator
            ,Room RSunTower [Edge noReq (R RSunTowerAccess)
                                    ,Edge noReq (R RTransporttoMagmoorCavernsNorth)]
            ,Room RGatheringHallAccess [Edge missile (R RArboretum)
                                    ,Edge noReq (R RGatheringHall)]
            ,Room RGatheringHall [Edge noReq (R RGatheringHallAccess)
                                    ,Edge missile (R RSaveStation2)
                                    ,Edge noReq (R RWateryHallAccess)
                                    ,Edge morph (R REastAtrium)
                                    ,Edge (gatheringHallSw diff) (R RChozoBackSw)
                                    ,Edge bombs (I GatheringHall)]
            ,Room RWateryHallAccess [Edge noReq (R RGatheringHall)
                                    ,Edge missile (R RWateryHall)
                                    ,Edge missile (I WateryHallAccess)]
            ,Room RWateryHall [Edge missile (R RWateryHallAccess)
                                    ,Edge (wateryHallTraverse diff) (R RDynamoAccess)
                                    ,Edge (wateryHallWater diff) (I WateryHallUnderwater)
                                    ,Edge (wateryHallSw diff) (R RChozoBackSw)
                                    ,Edge noReq (I WateryHallScanPuzzle)]
            ,Room RDynamoAccess [Edge missile (R RWateryHall)
                                    ,Edge missile (R RDynamo)]
            ,Room RDynamo [Edge missile (R RDynamoAccess)
                                    ,Edge missile (I DynamoLower)
                                    ,Edge spider (I DynamoSpiderTrack)]
            ,Room RSaveStation2 [Edge missile (R RGatheringHall)]
            ,Room REastAtrium [Edge noReq (R RGatheringHall)
                                    ,Edge noReq (R REnergyCoreAccess)]
            ,Room REnergyCoreAccess [Edge noReq (R REastAtrium)
                                    ,Edge noReq (R REnergyCore)]
            ,Room REnergyCore [Edge noReq (R REnergyCoreAccess)
                                    ,Edge morph (R RBurnDomeAccess)
                                    ,Edge bombs (R RWestFurnaceAccess)]
            ,Room RBurnDomeAccess [Edge bombs (R REnergyCore)
                                    ,Edge morph (R RBurnDome)]
            ,Room RBurnDome [Edge noReq (R RBurnDomeAccess)
                                    ,Edge bombs (I BurnDomeMissile)
                                    ,Edge noReq (I BurnDomeIDrone)]
            ,Room RWestFurnaceAccess [Edge noReq (R REnergyCore)
                                    ,Edge noReq (R RFurnaceFront)]
            ,Room RFurnaceFront [Edge noReq (R RWestFurnaceAccess)
                                    ,Edge (furnaceTraverse diff) (R RFurnace)
                                    ,Edge bombs (I FurnaceInsideFurnace)]
            ,Room RFurnace [Edge bombs (R RFurnaceFront)
                                    ,Edge morph (R RCrosswayAccessWest)
                                    ,Edge ice (R REastFurnaceAccess)
                                    ,Edge (furnaceItem diff) (I FurnaceSpiderTracks)]
            ,Room REastFurnaceAccess [Edge ice (R RFurnace)
                                    ,Edge ice (R RHalloftheElders)]
            ,Room RCrosswayAccessWest [Edge morph (R RFurnace)
                                    ,Edge wave (R RCrossway)]
            ,Room RCrossway [Edge noReq (R RCrosswayAccessWest)
                                    ,Edge (crosswayTraverse diff) (R RElderHallAccess)
                                    ,Edge ice (R RCrosswayAccessSouth)
                                    ,Edge (crosswayItem diff) (I Crossway)]
            ,Room RElderHallAccess [Edge missile (R RCrossway)
                                    ,Edge noReq (R RHalloftheElders)]
            ,Room RCrosswayAccessSouth [Edge ice (R RCrossway)
                                    ,Edge ice (R RHalloftheElders)]
            ,Room RHalloftheElders [Edge ice (R RCrosswayAccessSouth)
                                    ,Edge ice (R REastFurnaceAccess)
                                    ,Edge sjOrBombs (R RElderHallAccess)
                                    ,Edge (hoteWave diff) (R RReflectingPoolAccess)
                                    ,Edge (hotePlasma diff) (R RElderChamber)
                                    ,Edge (hoteIce diff) (I HalloftheElders)]
            ,Room RElderChamber [Edge noReq (I ElderChamber)
                                    ,Edge blocked (R RHalloftheElders)] -- Statue blocks the way if you get warped here
            ,Room RReflectingPoolAccess [Edge noReq (R RHalloftheElders)
                                    ,Edge noReq (R RReflectingPool)]
            ,Room RReflectingPool [Edge noReq (R RReflectingPoolAccess)
                                    ,Edge (reflectPoolSave diff) (R RSaveStation3)
                                    ,Edge (reflectPoolAntechamber diff) (R RAntechamber)
                                    ,Edge (reflectPoolIceDoor diff) (R RTransportAccessSouth)]
            ,Room RAntechamber [Edge noReq (I Antechamber)
                                    ,Edge ice (R RReflectingPool)]
            ,Room RTransportAccessSouth [Edge ice (R RReflectingPool)
                                    ,Edge noReq (R RTransporttoTallonOverworldSouth)]
            ,Room RTransporttoTallonOverworldSouth [Edge noReq (R RTransportAccessSouth)
                                    ,Edge noReq (R OTransporttoChozoRuinsSouth)]
            ,Room RSaveStation3 [Edge missile (R RReflectingPool)
                                    ,Edge bombs (R RTransporttoTallonOverworldEast)]
            ,Room RTransporttoTallonOverworldEast [Edge bombs (R RSaveStation3)
                                    ,Edge noReq (R OTransporttoChozoRuinsEast)]
            ,Room RChozoBackSw [Edge bombs (R RReflectingPool)
                                    ,Edge (crosswayInfiniteSpeed diff) (I ElderChamber)
                                    ,Edge (longWallcrawl diff) (R RChozoFrontSw)]
            ,Room RChozoFrontSw [Edge bombs (I TrainingChamber)
                                    ,Edge bombs (I MainPlazaGrappleLedge)
                                    ,Edge bombs (I TrainingChamberAccess)
                                    ,Edge bombs (I TowerofLight)]
            
            --Magmoor Caverns Rooms
            ,Room CTransporttoChozoRuinsNorth [Edge noReq (R RTransporttoMagmoorCavernsNorth)
                                    ,Edge noReq (R CBurningTrail)]
            ,Room CBurningTrail [Edge noReq (R CTransporttoChozoRuinsNorth)
                                    ,Edge missile (R CSaveStationMagmoorA)
                                    ,Edge noReq (R CLakeTunnel)
                                    ,Edge (burningTrailSw diff) (R CMagmoorFrontSw)]
            ,Room CSaveStationMagmoorA [Edge missile (R CBurningTrail)]
            ,Room CLakeTunnel [Edge noReq (R CBurningTrail)
                                    ,Edge noReq (R CLavaLake)]
            ,Room CLavaLake [Edge noReq (R CLakeTunnel)
                                    ,Edge (lavaLakeTraversal diff) (R CPitTunnel)
                                    ,Edge (lavaLakeItem diff) (I LavaLake)]
            ,Room CPitTunnel [Edge (lavaLakeReverseTraversal diff) (R CLavaLake)
                                    ,Edge (pitTunnel diff) (R CTriclopsPit)]
            ,Room CTriclopsPit [Edge (pitTunnelReverse diff) (R CPitTunnel)
                                    ,Edge (storageCavern diff) (R CStorageCavern)
                                    ,Edge (heatResistOr8Etanks diff) (R CMonitorTunnel) -- This has a high requirement to discourage this path to get to phendrana
                                    ,Edge (triclopsPitItem diff) (I TriclopsPit)]
            ,Room CStorageCavern [Edge (vmr2Tank diff) (R CTriclopsPit)
                                    ,Edge noReq (I StorageCavern)]
            ,Room CMonitorTunnel [Edge (vmr2Tank diff) (R CTriclopsPit)
                                    ,Edge (vmr2Tank diff) (R CMonitorStation)]
            ,Room CMonitorStation [Edge (vmr4Tank diff) (R CMonitorTunnel) -- This requirement is excessive if warped to MonitorStation, going to storage cavern
                                    ,Edge (vmr3Tank diff) (R CShoreTunnel)
                                    ,Edge (toTransportTunnelA diff) (R CTransportTunnelA)
                                    ,Edge (monitorStationClimb diff) (R CWarriorShrine)]
            ,Room CTransportTunnelA [Edge bombs (R CMonitorStation)
                                    ,Edge noReq (R CTransporttoPhendranaDriftsNorth)
                                    ,Edge bombs (I TransportTunnelA)]
            ,Room CTransporttoPhendranaDriftsNorth [Edge noReq (R CTransportTunnelA)
                                    ,Edge noReq (R DTransporttoMagmoorCavernsWest)]
            ,Room CWarriorShrine [Edge (vmr2Tank diff) (R CMonitorStation)
                                    ,Edge (warriorShrineTunnel diff) (R CFieryShores)
                                    ,Edge noReq (I WarriorShrine)
                                    ,Edge (warriorShrineTunnel diff) (I FieryShoresWarriorShrineTunnel)]
            ,Room CShoreTunnel [Edge (vmr2Tank diff) (R CMonitorStation)
                                    ,Edge (vmr2Tank diff) (R CFieryShores)
                                    ,Edge pb (I ShoreTunnel)]
            ,Room CFieryShores [Edge (vmr3Tank diff) (R CShoreTunnel)
                                    ,Edge (vmr1Tank diff) (R CTransportTunnelB)
                                    ,Edge bombs (I FieryShoresMorphTrack)]
            ,Room CTransportTunnelB [Edge (vmr4Tank diff) (R CFieryShores)
                                    ,Edge noReq (R CTransporttoTallonOverworldWest)]
            ,Room CTransporttoTallonOverworldWest [Edge (vmr4Tank diff) (R CTransportTunnelB)
                                    ,Edge noReq (R OTransporttoMagmoorCavernsEast)
                                    ,Edge (crossTft diff) (R CTwinFiresTunnel)]
            ,Room CTwinFiresTunnel [Edge (crossTftReverse diff) (R CTransporttoTallonOverworldWest)
                                    ,Edge noReq (R CTwinFires)]
            ,Room CTwinFires [Edge noReq (R CTwinFiresTunnel)
                                    ,Edge (crossTwinFires diff) (R CNorthCoreTunnel)]
            ,Room CNorthCoreTunnel [Edge (crossTwinFires diff) (R CTwinFires)
                                    ,Edge (crossNorthCoreTunnel diff) (R CGeothermalCore)]
            ,Room CGeothermalCore [Edge (crossNorthCoreTunnel diff) (R CNorthCoreTunnel)
                                    ,Edge noReq (R CSouthCoreTunnel)
                                    ,Edge (geoCore diff) (R CPlasmaProcessing)]
            ,Room CPlasmaProcessing [Edge plasma (R CGeothermalCore)
                                    ,Edge noReq (I PlasmaProcessing)]
            ,Room CSouthCoreTunnel [Edge wave (R CGeothermalCore)
                                    ,Edge wave (R CMagmoorWorkstation)]
            ,Room CMagmoorWorkstation [Edge noReq (R CSouthCoreTunnel)
                                    ,Edge sjOrBombs (R CWorkstationTunnel)
                                    ,Edge (workstationWaveDoor diff) (R CTransportTunnelC)
                                    ,Edge (workstationSw diff) (R CMagmoorBackSw)
                                    ,Edge (workstationItem diff) (I MagmoorWorkstation)]
            ,Room CTransportTunnelC [Edge wave (R CMagmoorWorkstation)
                                    ,Edge wave (R CTransporttoPhendranaDriftsSouth)]
            ,Room CTransporttoPhendranaDriftsSouth [Edge wave (R CTransportTunnelC)
                                    ,Edge missile (R CSaveStationMagmoorB)
                                    ,Edge noReq (R DTransporttoMagmoorCavernsSouth)]
            ,Room CSaveStationMagmoorB [Edge missile (R CTransporttoPhendranaDriftsSouth)]
            ,Room CWorkstationTunnel [Edge noReq (R CMagmoorWorkstation)
                                    ,Edge (workstationTunnel diff) (R CTransporttoPhazonMinesWest)]
            ,Room CTransporttoPhazonMinesWest [Edge (workstationTunnel diff) (R CWorkstationTunnel)
                                    ,Edge noReq (R MTransporttoMagmoorCavernsSouth)]
            ,Room CMagmoorBackSw [Edge bombs (R CTransporttoPhazonMinesWest)
                                    ,Edge (longWallcrawl diff) (R CMagmoorFrontSw)
                                    ,Edge bombs (I PlasmaProcessing)]
            ,Room CMagmoorFrontSw [Edge (magmoorFrontWallcrawl diff) (R CMagmoorBackSw)]

            -- Phendrana Drifts Rooms
            ,Room DTransporttoMagmoorCavernsWest [Edge noReq (R CTransporttoPhendranaDriftsNorth)
                                    ,Edge noReq (R DShorelineEntrance)]
            ,Room DShorelineEntrance [Edge noReq (R DTransporttoMagmoorCavernsWest)
                                    ,Edge (iceBarrier diff) (R DPhendranaShorelines)]
            ,Room DPhendranaShorelines [Edge (iceBarrier diff) (R DShorelineEntrance)
                                    ,Edge noReq (R DSaveStationB)
                                    ,Edge noReq (R DIceRuinsAccess)
                                    ,Edge (climbShorelines diff)  (R DPlazaWalkway)
                                    ,Edge (climbShorelines diff) (R DRuinsEntryway)
                                    ,Edge (climbShorelines diff) (R DTempleEntryway)
                                    ,Edge plasma (I PhendranaShorelinesBehindIce)
                                    ,Edge (shorelinesTower diff) (I PhendranaShorelinesSpiderTrack)]
            ,Room DSaveStationB [Edge noReq (R DPhendranaShorelines)]
            ,Room DTempleEntryway [Edge noReq (R DPhendranaShorelines)
                                    ,Edge (iceBarrier diff) (R DChozoIceTemple)]
            ,Room DChozoIceTemple [Edge (iceBarrier diff) (R DTempleEntryway)
                                    ,Edge (iceTempleClimb diff) (R DChapelTunnel)
                                    ,Edge (iceTempleClimb diff) (I ChozoIceTempleTrigger)
                                    ,Edge (iceTempleItem diff) (I ChozoIceTemple)]
            ,Room DChapelTunnel [Edge chozoIceTempleBarrier (R DChozoIceTemple)
                                    ,Edge noReq (R DChapeloftheElders)] -- Warp point is near Chapel of the Elders
            ,Room DChapeloftheElders [Edge wave (R DChapelTunnel)
                                    ,Edge missile (I ChapeloftheElders)]
            ,Room DIceRuinsAccess [Edge noReq (R DPhendranaShorelines)
                                    ,Edge (iceBarrier diff) (R DIceRuinsEast)]
            ,Room DIceRuinsEast [Edge (iceBarrier diff) (R DIceRuinsAccess)
                                    ,Edge noReq (R DPlazaWalkway)
                                    ,Edge (ireSpiderTrack diff) (I IceRuinsEastSpiderTrack)
                                    ,Edge plasma (I IceRuinsEastBehindIce)]
            ,Room DPlazaWalkway [Edge noReq (R DIceRuinsEast)
                                    ,Edge noReq (R DPhendranaShorelines)]
            ,Room DRuinsEntryway [Edge noReq (R DPhendranaShorelines)
                                    ,Edge noReq (R DIceRuinsWest)]
            ,Room DIceRuinsWest [Edge noReq (R DRuinsEntryway)
                                    ,Edge missile (R DCanyonEntryway)
                                    ,Edge (irwSw diff) (R DPhendranaFrontSw)
                                    ,Edge (irwDoor diff) (R DCourtyardEntryway)
                                    ,Edge (irwItem diff) (I IceRuinsWest)]
            ,Room DCanyonEntryway [Edge noReq (R DIceRuinsWest)
                                    ,Edge noReq (R DPhendranaCanyon)]
            ,Room DPhendranaCanyon [Edge noReq (R DCanyonEntryway)
                                    ,Edge noReq (I PhendranaCanyon)]
            ,Room DCourtyardEntryway [Edge noReq (R DIceRuinsWest)
                                    ,Edge (ruinedCourtyardClimb diff) (R DRuinedCourtyard)] -- Ruined courtyard spawn is at the top of the room
            ,Room DRuinedCourtyard [Edge noReq (R DCourtyardEntryway)
                                    ,Edge (ruinedCourtyardSave diff) (R DSaveStationA)
                                    ,Edge wave (R DSpecimenStorage)
                                    ,Edge (ruinedCourtyardConduit diff) (R DQuarantineAccess) 
                                    ,Edge (ruinedCourtyardSw diff) (R DPhendranaFrontSw)
                                    ,Edge morph (I RuinedCourtyard)]
            ,Room DSaveStationA [Edge missile (R DCourtyardEntryway) -- If you fall
                                    ,Edge (ruinedCourtyardSave diff) (R DRuinedCourtyard) -- If can make it to the spawn point
                                    ,Edge morph (I RuinedCourtyard)] -- You can grab the item by falling here, without reaching the warp
            ,Room DQuarantineAccess [Edge noReq (R DRuinedCourtyard)
                                    ,Edge noReq (R DNorthQuarantineTunnel)]
            ,Room DNorthQuarantineTunnel [Edge wave (R DQuarantineAccess)
                                    ,Edge (quarantineTunnel diff) (R DQuarantineCave)]
            ,Room DQuarantineCave [Edge (quarantineTunnel diff) (R DNorthQuarantineTunnel)
                                    ,Edge (climbQuarantineCaveBack diff) (R DQuarantineCaveBack)
                                    ,Edge noReq (I QuarantineCave)]
            -- Added a new "room" representing the other door in quarantine cave
            ,Room DQuarantineCaveBack [Edge (quarantineMonitor diff) (R DQuarantineMonitor)
                                    ,Edge (quarantineTunnel diff) (R DSouthQuarantineTunnel)
                                    ,Edge noReq (I QuarantineCave) -- Can drop into thardus fight
                                    ,Edge (climbQuarantineCaveEntrance diff) (R DQuarantineCave)]
            ,Room DQuarantineMonitor [Edge (climbQuarantineCaveBack diff) (R DQuarantineCaveBack)
                                    ,Edge (climbQuarantineCaveEntrance diff) (R DQuarantineCave)
                                    ,Edge noReq (I QuarantineCave) -- Can drop into thardus fight
                                    ,Edge noReq (I QuarantineMonitor)]
            ,Room DSouthQuarantineTunnel [Edge (quarantineTunnel diff) (R DQuarantineCaveBack)
                                    ,Edge wave (R DTransporttoMagmoorCavernsSouth)]
            ,Room DTransporttoMagmoorCavernsSouth [Edge wave (R DSouthQuarantineTunnel)
                                    ,Edge noReq (R CTransporttoPhendranaDriftsSouth)
                                    ,Edge (phenElevatorClimb diff) (R DTransportAccess)]
            ,Room DTransportAccess [Edge ice (R DTransporttoMagmoorCavernsSouth)
                                    ,Edge wave (R DFrozenPike)
                                    ,Edge plasma (I TransportAccess)]
            ,Room DSpecimenStorage [Edge wave (R DRuinedCourtyard)
                                    ,Edge wave (R DResearchEntrance)]
            ,Room DResearchEntrance [Edge wave (R DSpecimenStorage)
                                    ,Edge noReq (R DMapStation)
                                    ,Edge wave (R DHydraLabEntryway)]
            ,Room DMapStation [Edge noReq (R DResearchEntrance)]
            ,Room DHydraLabEntryway [Edge wave (R DResearchEntrance)
                                    ,Edge wave (R DResearchLabHydra)]
            ,Room DResearchLabHydra [Edge wave (R DHydraLabEntryway)
                                    ,Edge noReq (R DResearchLabHydraBack)
                                    ,Edge noReq (I ResearchLabHydraTrigger)]
            ,Room DResearchLabHydraBack [Edge researchLabHydraBarrier (R DResearchLabHydra)
                                    ,Edge wave (R DObservatoryAccess)
                                    ,Edge supers (I ResearchLabHydra)]
            ,Room DObservatoryAccess [Edge wave (R DResearchLabHydra)
                                    ,Edge wave (R DObservatory)]
            ,Room DObservatory [Edge wave (R DObservatoryAccess)
                                    ,Edge (observatoryClimb diff) (R DObservatoryTop)]
            ,Room DObservatoryTop [Edge (observatorySave diff) (R DSaveStationD)
                                    ,Edge wave (R DWestTowerEntrance)
                                    ,Edge noReq (R DObservatory)
                                    ,Edge (observatoryItem diff) (I Observatory)]
            ,Room DSaveStationD [Edge (observatorySave diff) (R DObservatoryTop)]
            ,Room DWestTowerEntrance [Edge wave (R DObservatoryTop)
                                    ,Edge missile (R DWestTower)]
            ,Room DWestTower [Edge missile (R DWestTowerEntrance)
                                    ,Edge wave (R DControlTower)]
            ,Room DControlTower [Edge wave (R DWestTower)
                                    ,Edge wave (R DEastTower)
                                    ,Edge (controlTowerItem diff) (I ControlTower)]
            ,Room DEastTower [Edge wave (R DControlTower)
                                    ,Edge wave (R DAetherLabEntryway)]
            ,Room DAetherLabEntryway [Edge wave (R DEastTower)
                                    ,Edge wave (R DResearchLabAether)]
            ,Room DResearchLabAether [Edge wave (R DAetherLabEntryway)
                                    ,Edge wave (R DResearchCoreAccess)
                                    ,Edge missile (I ResearchLabAetherTank)
                                    ,Edge (rlaTrack diff) (I ResearchLabAetherMorphTrack)]
            ,Room DResearchCoreAccess [Edge wave (R DResearchLabAether)
                                    ,Edge wave (R DResearchCore)]
            ,Room DResearchCore [Edge wave (R DResearchCoreAccess)
                                    ,Edge ice (R DPikeAccess)
                                    ,Edge noReq (I ResearchCore)]
            ,Room DPikeAccess [Edge ice (R DResearchCore)
                                    ,Edge wave (R DFrozenPike)]
            ,Room DFrozenPike [Edge (frozenPikeClimb diff) (R DTransportAccess)
                                    ,Edge wave (R DPikeAccess)
                                    ,Edge wave (R DFrostCaveAccess)
                                    ,Edge (frozenPikeBottom diff) (R DHunterCaveAccess)]
            ,Room DFrostCaveAccess [Edge wave (R DFrozenPike)
                                    ,Edge (frostCaveAccess diff) (R DFrostCave)]
            ,Room DFrostCave [Edge (frostCaveAccess diff) (R DFrostCaveAccess)
                                    ,Edge (frostCaveDoor diff) (R DSaveStationC)
                                    ,Edge (frostCaveToTunnel diff) (R DUpperEdgeTunnel)
                                    ,Edge (frostCaveSw diff) (R DPhendranaBackSw)
                                    ,Edge (frostCaveItem diff) (I FrostCave)]
            ,Room DSaveStationC [Edge (frostCaveDoor diff) (R DFrostCave)]
            ,Room DUpperEdgeTunnel [Edge (frostCaveAccess diff) (R DFrostCave)
                                    ,Edge wave (R DPhendranasEdge)]
            ,Room DPhendranasEdge [Edge wave (R DUpperEdgeTunnel)
                                    ,Edge (toStorageCave diff) (R DStorageCave)
                                    ,Edge (toSecurityCave diff) (R DSecurityCave)
                                    ,Edge noReq (R DLowerEdgeTunnel)]
            ,Room DStorageCave [Edge (fromStorageCave diff) (R DPhendranasEdge)
                                    ,Edge noReq (I StorageCave)]
            ,Room DSecurityCave [Edge morph (R DPhendranasEdge)
                                    ,Edge noReq (I SecurityCave)]
            ,Room DLowerEdgeTunnel [Edge (phenEdgeLower diff) (R DPhendranasEdge)
                                    ,Edge wave (R DHunterCave)]
            ,Room DHunterCave [Edge wave (R DLowerEdgeTunnel)
                                    ,Edge (hunterCaveLower diff) (R DLakeTunnel)
                                    ,Edge (hunterCaveUpper diff) (R DHunterCaveFar)]
            ,Room DHunterCaveFar [Edge sjOrBombs (R DHunterCave)
                                    ,Edge wave (R DChamberAccess)
                                    ,Edge wave (R DHunterCaveAccess)]
            ,Room DLakeTunnel [Edge (hunterCaveClimb diff) (R DHunterCave)
                                    ,Edge wave (R DGravityChamber)]
            ,Room DGravityChamber [Edge (gravityChamberToLakeTunnel diff) (R DLakeTunnel)
                                    ,Edge (climbGravityChamber diff) (R DGravityChamberTop)
                                    ,Edge noReq (I GravityChamberUnderwater)]
            ,Room DGravityChamberTop [Edge noReq (R DGravityChamber)
                                    ,Edge wave (R DChamberAccess)
                                    ,Edge (gravLedge diff) (I GravityChamberGrappleLedge)]
            ,Room DChamberAccess [Edge wave (R DGravityChamberTop)
                                    ,Edge wave (R DHunterCaveFar)]
            ,Room DHunterCaveAccess [Edge wave (R DHunterCaveFar)
                                    ,Edge (frozenPikeBottom diff) (R DFrozenPike)]
            ,Room DPhendranaFrontSw [Edge bombs (R DRuinedCourtyard)
                                    ,Edge bombs (I QuarantineMonitor)
                                    ,Edge bombs (I QuarantineCave)
                                    ,Edge (longWallcrawl diff) (R DPhendranaBackSw)]
            ,Room DPhendranaBackSw [Edge (longWallcrawl diff) (R DPhendranaFrontSw)
                                    ,Edge bombs (R DFrostCave)
                                    ,Edge bombs (R DGravityChamber)
                                    ,Edge bombs (I GravityChamberGrappleLedge)
                                    ,Edge (transportAccessItemOob diff) (I TransportAccess)
                                    ,Edge bombs (R DTransportAccess)
                                    ,Edge bombs (R DFrozenPike)
                                    ,Edge bombs (I SecurityCave)
                                    ,Edge bombs (I StorageCave)]

            -- Phazon Mines Rooms
            ,Room MTransporttoTallonOverworldSouth [Edge noReq (R OTransporttoPhazonMinesEast)
                                    ,Edge wave (R MQuarryAccess)]
            ,Room MQuarryAccess [Edge wave (R MTransporttoTallonOverworldSouth)
                                    ,Edge wave (R MMainQuarry)]
            ,Room MMainQuarry [Edge wave (R MQuarryAccess)
                                    ,Edge (quarrySave diff) (R MSaveStationMinesA)
                                    ,Edge (reachWasteDisposal diff) (R MWasteDisposal)
                                    ,Edge ice (R MSecurityAccessA)
                                    ,Edge (quarrySave diff) (I MainQuarryBarrierTriggers)
                                    ,Edge (quarryItem diff) (I MainQuarry)]
            ,Room MSaveStationMinesA [Edge mainQuarryBarrierWave (R MMainQuarry)]
            ,Room MSecurityAccessA [Edge mainQuarryBarrierIce (R MMainQuarry)
                                    ,Edge ice (R MMineSecurityStation)
                                    ,Edge pb (I SecurityAccessA)]
            ,Room MMineSecurityStation [Edge waveIce (R MSecurityAccessA)
                                    ,Edge (toStorageDepotA diff) (R MStorageDepotA)
                                    ,Edge wave (R MSecurityAccessB)
                                    ,Edge pb (I StorageDepotATrigger)]
            ,Room MStorageDepotA [Edge (storageDepotABarrier diff) (R MMineSecurityStation)
                                    ,Edge noReq (I StorageDepotA)]
            ,Room MSecurityAccessB [Edge wave (R MMineSecurityStation)
                                    ,Edge (securityAccessBSw diff) (R MMinesFrontSw)
                                    ,Edge ice (R MEliteResearch)]
            ,Room MEliteResearch [Edge ice (R MSecurityAccessB)
                                    ,Edge (eliteResearchDoor diff) (R MResearchAccess)
                                    ,Edge (eliteResearchTopItem diff) (I EliteResearchLaser)
                                    ,Edge (eliteResearchPirate diff) (I EliteResearchPhazonElite)]
            -- Currently require boosting through wall even if you can laser it
            ,Room MResearchAccess [Edge (shaftClimb2 diff) (R MEliteResearch)
                                    ,Edge ice (R MOreProcessingBottom)]
            ,Room MOreProcessingBottom [Edge ice (R MResearchAccess)
                                    ,Edge (oreProcessingClimb diff) (R MElevatorAccessA)
                                    ,Edge (oreProcessingTop diff) (R MWasteDisposal)
                                    ,Edge (oreProcessingTop diff) (R MStorageDepotB)]
            -- Spawn point is next to the pb rocks
            ,Room MOreProcessing [Edge noReq (R MOreProcessingBottom)
                                    ,Edge (dashFromPbRocks diff) (R MElevatorAccessA)
                                    ,Edge (oreProcessingTopFromRocks diff) (R MOreProcessingTop)]
            -- This fake room is considered to be at the top on the waste disposal side
            ,Room MOreProcessingTop[Edge noReq (R MOreProcessing)
                                    ,Edge ice (R MWasteDisposal)
                                    ,Edge (oreProcessingCrossTop diff) (R MStorageDepotB)]
            ,Room MWasteDisposal [Edge ice (R MOreProcessingTop)
                                    ,Edge (wasteDisposalTraversal diff) (R MMainQuarry)]
            ,Room MStorageDepotB [Edge (oreProcessingCrossTop diff) (R MOreProcessingTop)
                                    ,Edge ice (R MOreProcessing)
                                    ,Edge noReq (I StorageDepotB)]
            ,Room MElevatorAccessA [Edge ice (R MOreProcessing)
                                    ,Edge (oreProcessingTopFromEaa diff) (R MOreProcessingTop)
                                    ,Edge ice (R MElevatorA)]
            ,Room MElevatorA [Edge (shaftClimb1 diff) (R MElevatorAccessA)
                                    ,Edge ice (R MEliteControlAccess)]
            ,Room MEliteControlAccess [Edge ice (R MElevatorA)
                                    ,Edge wave (R MEliteControl)
                                    ,Edge (ecaItem diff) (I EliteControlAccess)]
            ,Room MEliteControl [Edge wave (R MEliteControlAccess)
                                    ,Edge ice (R MMaintenanceTunnel)
                                    ,Edge ice (R MVentilationShaft)
                                    ,Edge noReq (I EliteControlTrigger)]
            ,Room MMaintenanceTunnel [Edge ice (R MEliteControl)
                                    ,Edge (maintTunnel diff) (R MPhazonProcessingCenter)]
            ,Room MPhazonProcessingCenter [Edge (maintTunnel diff) (R MMaintenanceTunnel)
                                    ,Edge blocked (R MProcessingCenterAccess) -- Not going to deal with this door as it's rarely useful
                                    ,Edge (ppcClimb diff) (R MTransportAccess)
                                    ,Edge pb (I PhazonProcessingCenter)]
            ,Room MTransportAccess [Edge ice (R MPhazonProcessingCenter)
                                    ,Edge (toMinesElevator diff) (R MTransporttoMagmoorCavernsSouth)]
            ,Room MTransporttoMagmoorCavernsSouth [Edge (toMinesElevator diff) (R MTransportAccess)
                                    ,Edge noReq (R CTransporttoPhazonMinesWest)]
            -- Warp is at the top
            ,Room MVentilationShaft [Edge eliteControlBarrier (R MEliteControl)
                                    ,Edge ice (R MOmegaResearch)
                                    ,Edge pb (I VentilationShaft)]
            ,Room MOmegaResearch [Edge ice (R MVentilationShaft)
                                    ,Edge (maintTunnel diff) (R MMapStationMines)
                                    ,Edge ice (R MDynamoAccess)]
            ,Room MMapStationMines [Edge (maintTunnel diff) (R MOmegaResearch)]
            ,Room MDynamoAccess [Edge ice (R MOmegaResearch)
                                    ,Edge ice (R MCentralDynamo)]
            -- Warp is the top, but treating as the bottom. It's slightly inaccurate
            ,Room MCentralDynamo [Edge (centralDynamoClimb diff) (R MDynamoAccess)
                                    ,Edge ice (R MSaveStationMinesB)
                                    ,Edge (maintTunnel diff) (R MQuarantineAccessA)
                                    ,Edge morph (I CentralDynamo)]
            ,Room MSaveStationMinesB [Edge ice (R MCentralDynamo)]
            ,Room MQuarantineAccessA [Edge (maintTunnel diff) (R MCentralDynamo)
                                    ,Edge wave (R MMetroidQuarantineA)]
            ,Room MMetroidQuarantineA [Edge wave (R MQuarantineAccessA)
                                    ,Edge noReq (R MMetroidQuarantineABack)
                                    ,Edge noReq (I MetroidQuarantineATrigger)]
            ,Room MMetroidQuarantineABack [Edge mqaBarrier (R MMetroidQuarantineA)
                                    ,Edge (mqaTraversal diff) (R MElevatorAccessB)
                                    ,Edge (mqaItem diff) (I MetroidQuarantineA)]
            ,Room MElevatorAccessB [Edge ice (R MMetroidQuarantineABack)
                                    ,Edge plasma (R MElevatorB)]
            ,Room MElevatorB [Edge plasma (R MFungalHallAccess)
                                    ,Edge plasma (R MElevatorAccessB)]
            ,Room MFungalHallAccess [Edge plasma (R MElevatorB)
                                    ,Edge plasma (R MFungalHallA)
                                    ,Edge morph (I FungalHallAccess)]
            ,Room MFungalHallA [Edge (climbFungalHallAccess diff) (R MFungalHallAccess)
                                    ,Edge (fungalHallATraversal diff) (R MPhazonMiningTunnel)]
            ,Room MPhazonMiningTunnel [Edge plasma (R MFungalHallA)
                                    ,Edge (miningTunnelTraversal diff) (R MFungalHallB)
                                    ,Edge (miningTunnelItem diff) (I PhazonMiningTunnel)]
            ,Room MFungalHallB [Edge (miningTunnelTraversal diff) (R MPhazonMiningTunnel)
                                    ,Edge (fungalHallBTraversal diff) (R MMissileStationMinesInbounds)
                                    ,Edge (fungalHallBTraversal diff) (R MQuarantineAccessB)
                                    ,Edge bombs (I FungalHallB)]
            ,Room MMissileStationMinesInbounds [Edge plasma (R MFungalHallB)]
            ,Room MMissileStationMines [Edge morph (R MMissileStationMinesInbounds) -- You get warped out of bounds and need morph
                                    ,Edge (wallcrawl diff) (R MMinesBackSw)] 
            ,Room MQuarantineAccessB [Edge plasma (R MFungalHallB)
                                    ,Edge (quarantineAccessBTraversal diff) (R MMetroidQuarantineB)]
            ,Room MMetroidQuarantineB [Edge (quarantineAccessBTraversal diff) (R MQuarantineAccessB)
                                    ,Edge (mqbTraversal diff) (R MMetroidQuarantineBBack)
                                    ,Edge noReq (I MetroidQuarantineBTrigger)]
            ,Room MMetroidQuarantineBBack [Edge mqbBarrier (R MMetroidQuarantineB)
                                    ,Edge plasma (R MSaveStationMinesC)
                                    ,Edge (mqbBackClimb diff) (R MEliteQuartersAccess)
                                    ,Edge (mqbSw diff) (R MMinesBackSw)
                                    ,Edge supers (I MetroidQuarantineB)]
            ,Room MSaveStationMinesC [Edge plasma (R MMetroidQuarantineBBack)]
            ,Room MEliteQuartersAccess [Edge plasma (R MMetroidQuarantineBBack)
                                    ,Edge plasma (R MEliteQuarters)]
            ,Room MEliteQuarters [Edge (eliteQuartersPlasma diff) (R MEliteQuartersAccess)
                                    ,Edge (eliteQuartersTop diff) (R MProcessingCenterAccess)
                                    ,Edge (eliteQuarters diff) (I EliteQuarters)]
            ,Room MProcessingCenterAccess [Edge plasma (R MEliteQuarters)
                                    ,Edge (ppcBottomClimb diff) (R MPhazonProcessingCenter)
                                    ,Edge noReq (I ProcessingCenterAccess)]
            ,Room MMinesFrontSw [Edge bombs (I StorageDepotA)
                                    ,Edge ice (R MMainQuarry)
                                    ,Edge ice (I SecurityAccessA)]
            ,Room MMinesBackSw [Edge bombs (R MFungalHallB)
                                    ,Edge bombs (R MPhazonProcessingCenter)
                                    ,Edge bombs (R MMetroidQuarantineB)
                                    ,Edge (longWallcrawl diff) (I FungalHallAccess)]

            -- Pseudo-items
            ,Item FrigatePowerDoorTrigger FrigatePowerDoor OMainVentilationShaftSectionB
            ,Item MainQuarryBarrierTriggers MainQuarryBarriers MMainQuarry
            ,Item ChozoIceTempleTrigger ChozoIceTempleBarrier DChozoIceTemple
            ,Item StorageDepotATrigger StorageDepotABarrier MMineSecurityStation
            ,Item ResearchLabHydraTrigger ResearchLabHydraBarrier DResearchLabHydra
            ,Item EliteControlTrigger EliteControlBarrier MEliteControl
            ,Item MetroidQuarantineATrigger MetroidQuarantineABarrier MMetroidQuarantineA
            ,Item MetroidQuarantineBTrigger MetroidQuarantineBBarrier MMetroidQuarantineB
            ]
