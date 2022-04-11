module Node where

data Id = R RoomId | I ItemId
        deriving  (Read, Eq, Show)

data Node = Room {roomId :: RoomId, edges :: [Edge]} | Item {itemId :: ItemId, itemName :: ItemName, warp :: RoomId}
        deriving (Show)
data Edge = Edge {canUse :: [ItemName] -> Bool, nodeId :: Id}
        
instance Show Edge where 
    show (Edge canUse nodeId) = show nodeId

data ItemName = Missile | EnergyTank | MorphBall | SpaceJumpBoots | MorphBallBomb | GrappleBeam | WaveBeam | IceBeam | PlasmaBeam | VariaSuit | GravitySuit 
                | PhazonSuit | BoostBall | PowerBomb | SpiderBall | SuperMissile | ChargeBeam | XRayVisor | ThermalVisor | Wavebuster | IceSpreader 
                | Flamethrower | Artifact
                deriving  (Read, Eq, Show, Enum)

-- Room IDs are distinct from Item IDs to make it more difficult to confuse them
data RoomId = OLandingSite | OCanyonCavern | OWaterfallCavern | OGully | OAlcove | OTallonCanyon | ORootTunnel 
                | OTransportTunnelA | ORootCave | OArborChamber | OTransportTunnelB | OTransporttoMagmoorCavernsEast
                | OOvergrownCavern | OFrigateAccessTunnel | OFrigateCrashSite | OTransportTunnelC | OTransporttoChozoRuinsEast | OTransporttoChozoRuinsWest
                | OMainVentilationShaftSectionC | OMainVentilationShaftSectionB | OMainVentilationShaftSectionA | OReactorCore | OReactorAccess
                | OSaveStation | OCargoFreightLifttoDeckGamma | ODeckBetaTransitHall | OBiohazardContainment | ODeckBetaSecurityHall | OBiotechResearchArea1
                | ODeckBetaConduitHall | OConnectionElevatortoDeckBeta | OHydroAccessTunnel | OGreatTreeHall | OGreatTreeHallTop | OTransportTunnelD
                | OTransporttoChozoRuinsSouth | OGreatTreeChamber | OLifeGroveTunnel | OLifeGrove | OTransportTunnelE | OTransporttoPhazonMinesEast
                | OTempleHall | OTempleSecurityStation | OTempleLobby | OArtifactTemple

                | RTransporttoTallonOverworldNorth | RRuinsEntrance | RMainPlaza | RMainPlazaLedge | RNurseryAccess | REyonTunnel | RRuinedNursery | RSaveStation1 
                | RNorthAtrium | RRuinedGallery | RMapStation | RTotemAccess | RHiveTotem | RTransportAccessNorth | RTransporttoMagmoorCavernsNorth 
                | RVaultAccess | RVault | RPlazaAccess | RRuinedShrineAccess | RRuinedShrine | RTowerofLightAccess | RTowerofLight | RTowerChamber 
                | RRuinedFountainAccess | RRuinedFountain | RMeditationFountain | RMagmaPool | RTrainingChamberAccess | RTrainingChamber | RPistonTunnel
                | RArboretumAccess | RSunchamberLobby | RSunchamberAccess | RSunchamber | RSunTowerAccess | RSunTower | RFurnaceFront
                | RArboretum | RGatheringHallAccess | RGatheringHall | RSaveStation2 | RWateryHallAccess | RWateryHall | RDynamoAccess | RDynamo | REastAtrium 
                | REnergyCoreAccess | REnergyCore | RBurnDomeAccess | RBurnDome | RWestFurnaceAccess | RFurnace | REastFurnaceAccess | RCrosswayAccessWest 
                | RCrossway | RCrosswayAccessSouth | RElderHallAccess | RHalloftheElders | RElderChamber | RReflectingPoolAccess | RReflectingPool | RAntechamber 
                | RSaveStation3 | RTransporttoTallonOverworldEast | RTransportAccessSouth | RTransporttoTallonOverworldSouth | RRuinedFountainNonWarp

                | CTransporttoChozoRuinsNorth | CBurningTrail | CSaveStationMagmoorA | CLakeTunnel | CLavaLake | CPitTunnel | CTriclopsPit | CStorageCavern 
                | CMonitorTunnel | CMonitorStation | CTransportTunnelA | CTransporttoPhendranaDriftsNorth | CWarriorShrine | CShoreTunnel | CFieryShores 
                | CTransportTunnelB | CTransporttoTallonOverworldWest | CTwinFiresTunnel | CTwinFires | CNorthCoreTunnel | CGeothermalCore | CPlasmaProcessing 
                | CSouthCoreTunnel | CMagmoorWorkstation | CWorkstationTunnel | CTransportTunnelC | CTransporttoPhendranaDriftsSouth | CSaveStationMagmoorB 
                | CTransporttoPhazonMinesWest
                
                | DTransporttoMagmoorCavernsWest | DShorelineEntrance | DPhendranaShorelines | DSaveStationB | DIceRuinsAccess | DIceRuinsEast | DPlazaWalkway 
                | DRuinsEntryway | DIceRuinsWest | DCanyonEntryway | DPhendranaCanyon | DTempleEntryway | DChozoIceTemple | DChapelTunnel | DChapeloftheElders 
                | DCourtyardEntryway | DRuinedCourtyard | DSaveStationA | DSpecimenStorage | DResearchEntrance | DMapStation | DHydraLabEntryway | DResearchLabHydra 
                | DObservatoryAccess | DObservatory | DSaveStationD | DWestTowerEntrance | DWestTower | DControlTower | DEastTower | DAetherLabEntryway 
                | DResearchLabAether | DResearchCoreAccess | DResearchCore | DQuarantineAccess | DNorthQuarantineTunnel | DQuarantineCave | DQuarantineMonitor 
                | DSouthQuarantineTunnel | DTransporttoMagmoorCavernsSouth | DTransportAccess | DFrozenPike | DPikeAccess | DFrostCaveAccess | DFrostCave 
                | DSaveStationC | DUpperEdgeTunnel | DPhendranasEdge | DStorageCave | DSecurityCave | DLowerEdgeTunnel | DHunterCave | DLakeTunnel 
                | DGravityChamber | DChamberAccess | DHunterCaveAccess | DQuarantineCaveBack | DGravityChamberTop | DHunterCaveFar

                | MTransporttoTallonOverworldSouth | MQuarryAccess | MMainQuarry | MSaveStationMinesA | MSecurityAccessA | MMineSecurityStation | MSecurityAccessB 
                | MStorageDepotA | MEliteResearch | MResearchAccess | MOreProcessing | MElevatorAccessA | MElevatorA | MStorageDepotB | MWasteDisposal 
                | MEliteControlAccess | MEliteControl | MMaintenanceTunnel | MVentilationShaft | MControlRoom | MOmegaResearch | MMapStationMines | MDynamoAccess 
                | MCentralDynamo | MSaveStationMinesB | MQuarantineAccessA | MMetroidQuarantineA | MElevatorAccessB | MElevatorB | MFungalHallAccess | MFungalHallA 
                | MPhazonMiningTunnel | MFungalHallB | MMissileStationMines | MQuarantineAccessB | MMetroidQuarantineB | MSaveStationMinesC | MEliteQuartersAccess 
                | MEliteQuarters | MProcessingCenterAccess | MPhazonProcessingCenter | MTransportAccess | MTransporttoMagmoorCavernsSouth
                deriving  (Read, Eq, Show, Enum)
data ItemId = MainPlazaHalfPipe | MainPlazaGrappleLedge | MainPlazaTree | MainPlazaLockedDoor | RuinedFountain | RuinedShrineBeetleBattle | RuinedShrineHalfPipe 
                | RuinedShrineLowerTunnel | Vault | TrainingChamber | RuinedNursery | TrainingChamberAccess | MagmaPool | TowerofLight | TowerChamber 
                | RuinedGalleryMissileWall | RuinedGalleryTunnel | TransportAccessNorth | GatheringHall | HiveTotem | SunchamberFlaahgra | SunchamberGhosts 
                | WateryHallAccess | WateryHallScanPuzzle | WateryHallUnderwater | DynamoLower | DynamoSpiderTrack | BurnDomeMissile | BurnDomeIDrone 
                | FurnaceSpiderTracks | FurnaceInsideFurnace | HalloftheElders | Crossway | ElderChamber | Antechamber | PhendranaShorelinesBehindIce 
                | PhendranaShorelinesSpiderTrack | ChozoIceTemple | IceRuinsWest | IceRuinsEastBehindIce | IceRuinsEastSpiderTrack | ChapeloftheElders 
                | RuinedCourtyard | PhendranaCanyon | QuarantineCave | ResearchLabHydra | QuarantineMonitor | Observatory | TransportAccess | ControlTower 
                | ResearchCore | FrostCave | ResearchLabAetherTank | ResearchLabAetherMorphTrack | GravityChamberUnderwater | GravityChamberGrappleLedge 
                | StorageCave | SecurityCave | LandingSite | Alcove | FrigateCrashSite | OvergrownCavern | RootCave | ArtifactTemple | TransportTunnelB 
                | ArborChamber | CargoFreightLifttoDeckGamma | BiohazardContainment | HydroAccessTunnel | GreatTreeChamber | LifeGroveTunnel | LifeGroveStart 
                | LifeGroveUnderwaterSpinner | MainQuarry | SecurityAccessA | StorageDepotB | StorageDepotA | EliteResearchPhazonElite | EliteResearchLaser 
                | EliteControlAccess | VentilationShaft | PhazonProcessingCenter | ProcessingCenterAccess | EliteQuarters | CentralDynamo | MetroidQuarantineB 
                | MetroidQuarantineA | FungalHallB | PhazonMiningTunnel | FungalHallAccess | LavaLake | TriclopsPit | StorageCavern | TransportTunnelA 
                | WarriorShrine | ShoreTunnel | FieryShoresMorphTrack | FieryShoresWarriorShrineTunnel | PlasmaProcessing | MagmoorWorkstation

                --Possible pseudo items: Ruined Fountain Collected, Maze item, opened save room in mines, opened OP backdoor in mines, Sunchamber, Chozo Ice Temple, HOTE statue
                -- Research Lab Hydra barrier
                deriving  (Read, Eq, Show, Enum)

noReq :: [ItemName] -> Bool
noReq _ = True 

morph :: [ItemName] -> Bool
morph x = contains x MorphBall

sj :: [ItemName] -> Bool
sj x = contains x SpaceJumpBoots

sjOrBombs :: [ItemName] -> Bool
sjOrBombs x = contains x SpaceJumpBoots || bombs x

missile :: [ItemName] -> Bool
missile x = contains x Missile

bombs :: [ItemName] -> Bool
bombs x = containsAll x [MorphBall, MorphBallBomb]

boost :: [ItemName] -> Bool
boost x = containsAll x [MorphBall, BoostBall]

spider :: [ItemName] -> Bool
spider x = containsAll x [MorphBall, SpiderBall]

grapple :: [ItemName] -> Bool
grapple x = contains x GrappleBeam

boostBombs :: [ItemName] -> Bool
boostBombs x = containsAll x [MorphBall, BoostBall, MorphBallBomb]

arbor :: [ItemName] -> Bool
arbor x = containsAll x [SpaceJumpBoots, GrappleBeam, PlasmaBeam]

sjGrapple :: [ItemName] -> Bool
sjGrapple x = containsAll x [SpaceJumpBoots, GrappleBeam]

mainPipe :: [ItemName] -> Bool
mainPipe x = contains x SpaceJumpBoots || containsAll x [BoostBall, MorphBall]

morphMissile :: [ItemName] -> Bool
morphMissile x = containsAll x [MorphBall, Missile]

fcsClimb :: [ItemName] -> Bool
fcsClimb _ = False

frigatePowerDoor :: [ItemName] -> Bool
frigatePowerDoor _ = False

fcsEntry :: [ItemName] -> Bool
fcsEntry x = contains x IceBeam && (contains x GrappleBeam || (contains x MorphBall && fcsItem x ))

fcsItem :: [ItemName] -> Bool
fcsItem x = contains x SpaceJumpBoots || contains x GravitySuit

wave :: [ItemName] -> Bool
wave x = contains x WaveBeam

ice :: [ItemName] -> Bool
ice x = contains x IceBeam

plasma :: [ItemName] -> Bool
plasma x = contains x PlasmaBeam

spiderIce :: [ItemName] -> Bool
spiderIce x = containsAll x [IceBeam, MorphBall, SpiderBall]

supers :: [ItemName] -> Bool
supers x = containsAll x [Missile, SuperMissile, ChargeBeam]

frigateRoom :: [ItemName] -> Bool
frigateRoom x = containsAll x [WaveBeam, GravitySuit]

gravSpace :: [ItemName] -> Bool
gravSpace x = containsAll x [GravitySuit, SpaceJumpBoots]

lgUnderWater :: [ItemName] -> Bool
lgUnderWater x = containsAll x [MorphBall, BoostBall, PowerBomb, SpaceJumpBoots]

hydroTunnel ::  [ItemName] -> Bool
hydroTunnel x = containsAll x [GravitySuit, MorphBall, MorphBallBomb]

gthClimb :: [ItemName] -> Bool
gthClimb x = containsAll x [SpaceJumpBoots, BoostBall, MorphBall]

bars :: [ItemName] -> Bool
bars _ = False

blocked :: [ItemName] -> Bool
blocked _ = False

lifeGroveT :: [ItemName] -> Bool
lifeGroveT x = containsAll x [PowerBomb, MorphBall, BoostBall]

towerChamber :: [ItemName] -> Bool
towerChamber x = containsAll x [GravitySuit, SpaceJumpBoots, WaveBeam]

rsHalf :: [ItemName] -> Bool
rsHalf x = containsAll x [MorphBall, BoostBall]

tolAccess :: [ItemName] -> Bool
tolAccess x = containsAll x [MorphBall, BoostBall, SpiderBall, WaveBeam]

towerOfLight :: [ItemName] -> Bool
towerOfLight x = containsCount 8 Missile x && sj x

heatResist :: [ItemName] -> Bool
heatResist x  = containsAny x [VariaSuit, GravitySuit, PhazonSuit]

crossMagmaPool :: [ItemName] -> Bool
crossMagmaPool x  = heatResist x && containsAll x [GrappleBeam,WaveBeam]

magmaPoolItem :: [ItemName] -> Bool
magmaPoolItem x  = heatResist x && containsAll x [GrappleBeam,PowerBomb]

tcItem :: [ItemName] -> Bool
tcItem x = containsAll x [MorphBall, BoostBall, MorphBallBomb, SpiderBall]

tcTunnel :: [ItemName] -> Bool
tcTunnel = boostBombs

climbSunTower :: [ItemName] -> Bool
climbSunTower x = containsAll x [MorphBall, SpiderBall, MorphBallBomb, SuperMissile, ChargeBeam]

sunchamberghost :: [ItemName] -> Bool
sunchamberghost = climbSunTower

wateryHallTraverse :: [ItemName] -> Bool 
wateryHallTraverse x = containsAll x [MorphBall, MorphBallBomb, Missile]

wateryHallWater :: [ItemName] -> Bool 
wateryHallWater x = contains x GravitySuit && (contains x SpaceJumpBoots || bombs x) 

furnaceTraverse :: [ItemName] -> Bool 
furnaceTraverse x = containsAll x [MorphBall, MorphBallBomb, SpiderBall]

furnaceItem :: [ItemName] -> Bool 
furnaceItem x = containsAll x [MorphBall, MorphBallBomb, SpiderBall, PowerBomb, BoostBall]

crosswayTraverse :: [ItemName] -> Bool 
crosswayTraverse x = containsAll x [MorphBall, BoostBall, Missile]

crosswayItem :: [ItemName] -> Bool 
crosswayItem x = containsAll x [MorphBall, BoostBall, Missile, SuperMissile, ChargeBeam, SpiderBall, MorphBallBomb]

hoteWave :: [ItemName] -> Bool 
hoteWave x = containsAll x [MorphBall, SpiderBall, MorphBallBomb, WaveBeam]

hoteIce :: [ItemName] -> Bool 
hoteIce x = containsAll x [MorphBall, SpiderBall, MorphBallBomb, IceBeam]

hotePlasma :: [ItemName] -> Bool 
hotePlasma x = containsAll x [MorphBall, MorphBallBomb, PlasmaBeam, IceBeam]

reflectPoolTop :: [ItemName] -> Bool 
reflectPoolTop x = containsAll x [MorphBall, MorphBallBomb, BoostBall, Missile]

reflectPoolIce :: [ItemName] -> Bool 
reflectPoolIce x = containsAll x [MorphBall, MorphBallBomb, BoostBall, IceBeam]

iceBarrier :: [ItemName] -> Bool
iceBarrier x = containsAny x [Missile, ChargeBeam]

shorelinesTower :: [ItemName] -> Bool
shorelinesTower x = containsAll x [MorphBall, SpiderBall, ChargeBeam, SuperMissile, Missile]

iceTempleClimb :: [ItemName] -> Bool
iceTempleClimb x = containsAll x [SpaceJumpBoots, MorphBall, MorphBallBomb, Missile]

iceTempleItem :: [ItemName] -> Bool
iceTempleItem x = containsAll x [SpaceJumpBoots, MorphBall, MorphBallBomb, PlasmaBeam]

irwDoor :: [ItemName] -> Bool
irwDoor x = containsAll x [SpaceJumpBoots, WaveBeam]

irwItem:: [ItemName] -> Bool
irwItem x = containsAll x [SpaceJumpBoots, Missile, PlasmaBeam]

ruinedCourtyardConduit :: [ItemName] -> Bool
ruinedCourtyardConduit x = containsAll x [ChargeBeam, Missile, SuperMissile, WaveBeam]

ruinedCourtyardSave :: [ItemName] -> Bool
ruinedCourtyardSave x = containsAll x [SpaceJumpBoots, Missile]

ruinedCourtyardClimb :: [ItemName] -> Bool
ruinedCourtyardClimb x = spider x || containsAll x [SpaceJumpBoots, MorphBall, BoostBall, MorphBallBomb]

quarantineTunnel :: [ItemName] -> Bool
quarantineTunnel x = containsAll x [MorphBall, WaveBeam]

phenElevatorClimb :: [ItemName] -> Bool
phenElevatorClimb x = containsAll x [MorphBall, SpiderBall, IceBeam]

observatoryClimb :: [ItemName] -> Bool
observatoryClimb x = containsAll x [MorphBall, BoostBall, MorphBallBomb, SpaceJumpBoots]

observatorySave :: [ItemName] -> Bool
observatorySave x = observatoryClimb x && contains x Missile

controlTowerItem :: [ItemName] -> Bool
controlTowerItem x = containsAll x [MorphBall, MorphBallBomb, PlasmaBeam, Missile]

rlaTrack :: [ItemName] -> Bool
rlaTrack x = contains x MorphBall && containsAny x [MorphBallBomb, SpaceJumpBoots]

toStorageCave :: [ItemName] -> Bool
toStorageCave x = containsAll x [SpaceJumpBoots, GrappleBeam, PlasmaBeam, MorphBall, PowerBomb]

fromStorageCave :: [ItemName] -> Bool
fromStorageCave x = containsAll x [PlasmaBeam, MorphBall, PowerBomb]

toSecurityCave :: [ItemName] -> Bool
toSecurityCave x = containsAll x [SpaceJumpBoots, GrappleBeam, MorphBall]

phenEdgeLower :: [ItemName] -> Bool
phenEdgeLower x = containsAll x [WaveBeam, GravitySuit, SpaceJumpBoots]

frozenPikeBottom :: [ItemName] -> Bool
frozenPikeBottom x = containsAll x [WaveBeam, GravitySuit, SpaceJumpBoots]

frozenPikeClimb :: [ItemName] -> Bool
frozenPikeClimb x = containsAll x [MorphBall,MorphBallBomb,SpaceJumpBoots]

gravLedge :: [ItemName] -> Bool
gravLedge x = containsAll x [PlasmaBeam, GrappleBeam]

climbGravityChamber :: [ItemName] -> Bool
climbGravityChamber x = contains x GravitySuit && (contains x SpaceJumpBoots || bombs x)

gravityChamberToLakeTunnel :: [ItemName] -> Bool
gravityChamberToLakeTunnel x = climbGravityChamber x && contains x WaveBeam

hunterCaveClimb :: [ItemName] -> Bool
hunterCaveClimb x = contains x Missile && (contains x SpaceJumpBoots || bombs x)

hunterCaveUpper :: [ItemName] -> Bool
hunterCaveUpper x = containsAll x [Missile, GrappleBeam]

hunterCaveLower :: [ItemName] -> Bool
hunterCaveLower x = contains x Missile && (contains x SpaceJumpBoots || bombs x)

frostCaveAccess :: [ItemName] -> Bool
frostCaveAccess x = containsAll x [MorphBall, WaveBeam]

frostCaveDoor :: [ItemName] -> Bool
frostCaveDoor x = contains x Missile && (contains x SpaceJumpBoots || bombs x)

frostCaveItem :: [ItemName] -> Bool
frostCaveItem x = containsAll x [GrappleBeam, Missile]

frostCaveToTunnel :: [ItemName] -> Bool
frostCaveToTunnel x = containsAll x [Missile,WaveBeam,MorphBall] && (contains x SpaceJumpBoots || bombs x)

containsCount :: Eq a => Int -> a -> [a] -> Bool
containsCount num elem list
    | num < 0 = False
    | num == 0 = True
    | otherwise = num <= count elem list

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

contains :: [ItemName] -> ItemName -> Bool
contains items item = item `elem` items

containsAll :: [ItemName] -> [ItemName] -> Bool
containsAll [] [] = True
containsAll items [] = True
containsAll [] checks = False 
containsAll items (x:rest) = contains items x && containsAll items rest

containsAny :: [ItemName] -> [ItemName] -> Bool
containsAny [] [] = True
containsAny items [] = True
containsAny [] checks = False 
containsAny items (x:rest) = contains items x || containsAll items rest

buildNodes :: [Node]
buildNodes = [ -- Tallon Overworld Rooms
            Room OLandingSite [Edge noReq (R OCanyonCavern)
                                    ,Edge noReq (R OWaterfallCavern)
                                    ,Edge sj (R OGully)
                                    ,Edge sj (R OAlcove)
                                    ,Edge noReq (R OTempleHall)
                                    ,Edge morph (I LandingSite)]
            ,Room OCanyonCavern [Edge noReq (R OLandingSite)
                                    ,Edge noReq (R OTallonCanyon)]
            ,Room OTallonCanyon [Edge noReq (R OCanyonCavern)
                                    ,Edge bombs (R OGully)
                                    ,Edge noReq (R ORootTunnel)
                                    ,Edge noReq (R OTransportTunnelA)]
            ,Room OGully [Edge bombs (R OTallonCanyon)
                                    ,Edge noReq (R OLandingSite)]
            ,Room ORootTunnel [Edge noReq (R OTallonCanyon)
                                    ,Edge missile (R ORootCave)]
            ,Room ORootCave [Edge missile (R ORootTunnel)
                                    ,Edge noReq (R OTransportTunnelB)
                                    ,Edge arbor (R OArborChamber)
                                    ,Edge sjGrapple (I RootCave)]
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
                                    ,Edge fcsClimb (R OOvergrownCavern)
                                    ,Edge fcsEntry (R OFrigateAccessTunnel)
                                    ,Edge fcsItem (I FrigateCrashSite)]
            ,Room OOvergrownCavern [Edge ice (R OFrigateCrashSite)
                                    ,Edge ice (R OTransportTunnelC)
                                    ,Edge noReq (I OvergrownCavern)]
            ,Room OTransportTunnelC [Edge ice (R OOvergrownCavern)
                                    ,Edge ice (R OTransporttoChozoRuinsEast)]
            ,Room OTransporttoChozoRuinsEast [Edge noReq (R RTransporttoTallonOverworldEast)
                                    ,Edge ice (R OTransportTunnelC)]
            ,Room OFrigateAccessTunnel [Edge ice (R OFrigateCrashSite)
                                    ,Edge noReq (R OMainVentilationShaftSectionC)]
            ,Room OMainVentilationShaftSectionC [Edge noReq (R OFrigateAccessTunnel)
                                    ,Edge noReq (R OMainVentilationShaftSectionB)]
            ,Room OMainVentilationShaftSectionB [Edge wave (R OMainVentilationShaftSectionA)
                                    ,Edge sj (R OMainVentilationShaftSectionC)]
            ,Room OMainVentilationShaftSectionA [Edge frigatePowerDoor (R OMainVentilationShaftSectionB)
                                    ,Edge noReq (R OReactorCore)]
            ,Room OReactorCore [Edge fcsItem (R OMainVentilationShaftSectionA)
                                    ,Edge wave (R OReactorAccess)]
            ,Room OReactorAccess [Edge wave (R OCargoFreightLifttoDeckGamma)
                                    ,Edge noReq (R OReactorCore)
                                    ,Edge noReq (R OSaveStation)]
            ,Room OSaveStation [Edge noReq (R OReactorAccess)]
            ,Room OCargoFreightLifttoDeckGamma [Edge frigateRoom (R ODeckBetaTransitHall)
                                    ,Edge noReq (R OReactorAccess)
                                    ,Edge missile (I CargoFreightLifttoDeckGamma)]
            ,Room ODeckBetaTransitHall [Edge noReq (R OCargoFreightLifttoDeckGamma)
                                    ,Edge noReq (R OBiohazardContainment)]
            ,Room OBiohazardContainment [Edge noReq (R ODeckBetaTransitHall)
                                    ,Edge frigateRoom (R ODeckBetaSecurityHall)
                                    ,Edge supers (I BiohazardContainment)]
            ,Room ODeckBetaSecurityHall [Edge noReq (R OBiohazardContainment)
                                    ,Edge noReq (R OBiotechResearchArea1)]
            ,Room OBiotechResearchArea1 [Edge noReq (R ODeckBetaSecurityHall)
                                    ,Edge frigateRoom (R ODeckBetaConduitHall)]
            ,Room ODeckBetaConduitHall [Edge noReq (R OBiotechResearchArea1)
                                    ,Edge noReq (R OConnectionElevatortoDeckBeta)]
            ,Room OConnectionElevatortoDeckBeta [Edge gravSpace (R ODeckBetaConduitHall)
                                    ,Edge noReq (R OHydroAccessTunnel)]
            ,Room OHydroAccessTunnel [Edge noReq (R OConnectionElevatortoDeckBeta)
                                    ,Edge hydroTunnel (R OGreatTreeHall)
                                    ,Edge morph (I HydroAccessTunnel)]
            --Great Tree Hall is split into two rooms, an upper and lower section
            ,Room OGreatTreeHall [Edge hydroTunnel (R OHydroAccessTunnel)
                                    ,Edge ice (R OTransportTunnelE)
                                    ,Edge gthClimb (R OGreatTreeHallTop)]
            ,Room OGreatTreeHallTop [Edge ice (R OTransportTunnelD)
                                    ,Edge sj (R OGreatTreeChamber)
                                    ,Edge spiderIce (R OLifeGroveTunnel)
                                    ,Edge bars (R OGreatTreeHall)]
            ,Room OTransportTunnelD [Edge ice (R OGreatTreeHallTop)
                                    ,Edge ice (R OTransporttoChozoRuinsSouth)]
            ,Room OTransporttoChozoRuinsSouth [Edge ice (R OTransportTunnelD)
                                    ,Edge noReq (R RTransporttoTallonOverworldSouth)]
            ,Room OGreatTreeChamber [Edge noReq (R OGreatTreeHallTop)
                                    ,Edge noReq (I GreatTreeChamber)]
            ,Room OLifeGroveTunnel [Edge noReq (R OGreatTreeHallTop)
                                    ,Edge lifeGroveT (R OLifeGrove)
                                    ,Edge lifeGroveT (I LifeGroveTunnel)]
            ,Room OLifeGrove [Edge morph (R OLifeGroveTunnel)
                                    ,Edge noReq (I LifeGroveStart)
                                    ,Edge lgUnderWater (I LifeGroveUnderwaterSpinner)]
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

            -- Chozo Ruins Rooms
            ,Room RTransporttoTallonOverworldNorth [Edge noReq (R OTransporttoChozoRuinsWest)
                                    ,Edge noReq (R RRuinsEntrance)]
            ,Room RRuinsEntrance [Edge noReq (R RTransporttoTallonOverworldNorth)
                                    ,Edge noReq (R RMainPlaza)]
            ,Room RMainPlaza [Edge noReq (R RRuinsEntrance)
                                    ,Edge blocked (R RPlazaAccess)
                                    ,Edge morph (R RRuinedFountainAccess)
                                    ,Edge missile (R RRuinedShrineAccess)
                                    ,Edge noReq (R RNurseryAccess)
                                    ,Edge blocked (R RPistonTunnel)
                                    ,Edge blocked (R RMainPlazaLedge)
                                    ,Edge mainPipe (I MainPlazaHalfPipe)
                                    ,Edge sjGrapple (I MainPlazaGrappleLedge)
                                    ,Edge supers (I MainPlazaTree)]
            --Created new room to hold the main plaza ledge item, and allow one-way traversal through Vault
            ,Room RMainPlazaLedge [Edge noReq (R RMainPlaza)
                                    ,Edge noReq (I MainPlazaLockedDoor)] 
            ,Room RPlazaAccess [Edge noReq (R RVault)
                                    ,Edge noReq (R RMainPlazaLedge)]
            ,Room RVault [Edge noReq (R RPlazaAccess)
                                    ,Edge noReq (R RVaultAccess)
                                    ,Edge bombs (I Vault)]
            ,Room RVaultAccess [Edge morph (R RVault)
                                    ,Edge noReq (R RTransporttoMagmoorCavernsNorth)]
            ,Room RTransporttoMagmoorCavernsNorth [Edge noReq (R RVaultAccess)
                                    ,Edge noReq (R CTransporttoChozoRuinsNorth)
                                    ,Edge climbSunTower (R RSunTower)
                                    ,Edge noReq (R RTransportAccessNorth)]
            -- Need to check warp here
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
                                    ,Edge tolAccess (R RTowerofLightAccess)
                                    ,Edge bombs (I RuinedShrineLowerTunnel)
                                    ,Edge rsHalf (I RuinedShrineHalfPipe)
                                    ,Edge noReq (I RuinedShrineBeetleBattle)]
            ,Room RTowerofLightAccess [Edge wave (R RRuinedShrine)
                                    ,Edge wave (R RTowerofLight)]
            ,Room RTowerofLight [Edge wave (R RTowerofLightAccess)
                                    ,Edge towerChamber (R RTowerChamber)
                                    ,Edge towerOfLight (I TowerofLight)]
            ,Room RTowerChamber [Edge wave (R RTowerofLight)
                                    ,Edge noReq (I TowerChamber)]
            ,Room RRuinedFountainAccess [Edge noReq (R RRuinedFountainNonWarp)
                                    ,Edge morph (R RMainPlaza)]
            -- The Ruined Fountain Warp puts you on top of the item and forces you to collect it
            ,Room RRuinedFountainNonWarp [Edge spider (R RRuinedFountain)
                                    ,Edge noReq (R RRuinedFountainAccess)
                                    ,Edge noReq (R RMeditationFountain)
                                    ,Edge noReq (R RArboretumAccess)]
            ,Room RRuinedFountain [Edge noReq (I RuinedFountain)]
            ,Room RMeditationFountain [Edge noReq (R RRuinedFountainNonWarp)
                                    ,Edge heatResist (R RMagmaPool)]
            ,Room RMagmaPool [Edge noReq (R RMeditationFountain)
                                    ,Edge crossMagmaPool (R RTrainingChamberAccess)
                                    ,Edge magmaPoolItem (I MagmaPool)]
            ,Room RTrainingChamberAccess [Edge wave (R RMagmaPool)
                                    ,Edge wave (R RTrainingChamber)
                                    ,Edge morph (I TrainingChamberAccess)]
            ,Room RTrainingChamber [Edge wave (R RTowerofLightAccess)
                                    ,Edge tcTunnel (R RPistonTunnel)
                                    ,Edge tcItem (I TrainingChamber)]
            ,Room RPistonTunnel [Edge morph (R RMainPlaza)
                                    ,Edge morph (R RTrainingChamber)]
            ,Room RArboretumAccess [Edge noReq (R RRuinedFountain)
                                    ,Edge missile (R RArboretum)]
            ,Room RArboretum [Edge missile (R RArboretumAccess)
                                    ,Edge bombs (R RSunchamberLobby)
                                    ,Edge missile (R RGatheringHallAccess)]
            ,Room RSunchamberLobby [Edge missile (R RArboretum)
                                    ,Edge noReq (R RSunchamberAccess)]
            ,Room RSunchamberAccess [Edge noReq (R RSunchamberLobby)
                                    ,Edge noReq (R RSunchamber)]
            -- The door vines are not considered here. Instead the ghost item has more requirements
            ,Room RSunchamber [Edge noReq (R RSunchamberAccess)
                                    ,Edge noReq (R RSunTowerAccess)
                                    ,Edge bombs (I SunchamberFlaahgra)
                                    ,Edge sunchamberghost (I SunchamberGhosts)]
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
                                    ,Edge morph (R REastAtrium)]
            ,Room RWateryHallAccess [Edge noReq (R RGatheringHall)
                                    ,Edge missile (R RWateryHall)
                                    ,Edge missile (I WateryHallAccess)]
            ,Room RWateryHall [Edge missile (R RWateryHallAccess)
                                    ,Edge wateryHallTraverse (R RDynamoAccess)
                                    ,Edge wateryHallWater (I WateryHallUnderwater)
                                    ,Edge noReq (I WateryHallScanPuzzle)]
            ,Room RDynamoAccess [Edge missile (R RWateryHall)
                                    ,Edge missile (R RDynamo)]
            ,Room RDynamo [Edge missile (R RDynamoAccess)
                                    ,Edge missile (I DynamoLower)
                                    ,Edge spider (I DynamoSpiderTrack)]
            ,Room RSaveStation2 [Edge noReq (R RGatheringHall)]
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
                                    ,Edge furnaceTraverse (R RFurnace)
                                    ,Edge bombs (I FurnaceInsideFurnace)]
            ,Room RFurnace [Edge bombs (R RFurnaceFront)
                                    ,Edge morph (R RCrosswayAccessWest)
                                    ,Edge ice (R REastFurnaceAccess)
                                    ,Edge furnaceItem (I FurnaceSpiderTracks)]
            ,Room REastFurnaceAccess [Edge ice (R RFurnace)
                                    ,Edge ice (R RHalloftheElders)]
            ,Room RCrosswayAccessWest [Edge morph (R RFurnace)
                                    ,Edge wave (R RCrossway)]
            ,Room RCrossway [Edge noReq (R RCrosswayAccessWest)
                                    ,Edge crosswayTraverse (R RElderHallAccess)
                                    ,Edge ice (R RCrosswayAccessSouth)
                                    ,Edge crosswayItem (I Crossway)]
            ,Room RElderHallAccess [Edge missile (R RCrossway)
                                    ,Edge noReq (R RHalloftheElders)]
            ,Room RCrosswayAccessSouth [Edge ice (R RCrossway)
                                    ,Edge ice (R RHalloftheElders)]
            ,Room RHalloftheElders [Edge ice (R RCrosswayAccessSouth)
                                    ,Edge ice (R REastFurnaceAccess)
                                    ,Edge sjOrBombs (R RElderHallAccess)
                                    ,Edge hoteWave (R RReflectingPoolAccess)
                                    ,Edge hotePlasma (R RElderChamber)
                                    ,Edge hoteIce (I HalloftheElders)]
            ,Room RElderChamber [Edge noReq (I ElderChamber)
                                    ,Edge ice (R RHalloftheElders)] -- Need to check if statue is moved?
            ,Room RReflectingPoolAccess [Edge noReq (R RHalloftheElders)
                                    ,Edge noReq (R RReflectingPool)]
            ,Room RReflectingPool [Edge noReq (R RReflectingPoolAccess)
                                    ,Edge reflectPoolTop (R RSaveStation3)
                                    ,Edge reflectPoolTop (R RAntechamber)
                                    ,Edge reflectPoolIce (R RTransportAccessSouth)]
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
            
            -- Phendrana Drifts Rooms
            ,Room DTransporttoMagmoorCavernsWest [Edge noReq (R CTransporttoPhendranaDriftsNorth)
                                    ,Edge noReq (R DShorelineEntrance)]
            ,Room DShorelineEntrance [Edge noReq (R DTransporttoMagmoorCavernsWest)
                                    ,Edge iceBarrier (R DPhendranaShorelines)]
            ,Room DPhendranaShorelines [Edge iceBarrier (R DShorelineEntrance)
                                    ,Edge noReq (R DSaveStationB)
                                    ,Edge noReq (R DIceRuinsAccess)
                                    ,Edge sj (R DPlazaWalkway)
                                    ,Edge sj (R DRuinsEntryway)
                                    ,Edge sj (R DTempleEntryway)
                                    ,Edge plasma (I PhendranaShorelinesBehindIce)
                                    ,Edge shorelinesTower (I PhendranaShorelinesSpiderTrack)]
            ,Room DSaveStationB [Edge noReq (R DPhendranaShorelines)]
            ,Room DTempleEntryway [Edge noReq (R DPhendranaShorelines)
                                    ,Edge iceBarrier (R DChozoIceTemple)]
            ,Room DChozoIceTemple [Edge iceBarrier (R DTempleEntryway)
                                    ,Edge iceTempleClimb (R DChapelTunnel)
                                    ,Edge iceTempleItem (I ChozoIceTemple)]
            ,Room DChapelTunnel [Edge blocked (R DChozoIceTemple)  -- Fix this later
                                    ,Edge noReq (R DChapeloftheElders)] -- Warp point is near Chapel of the Elders
            ,Room DChapeloftheElders [Edge wave (R DChapelTunnel)
                                    ,Edge missile (I ChapeloftheElders)]
            ,Room DIceRuinsAccess [Edge noReq (R DPhendranaShorelines)
                                    ,Edge iceBarrier (R DIceRuinsEast)]
            ,Room DIceRuinsEast [Edge iceBarrier (R DIceRuinsAccess)
                                    ,Edge noReq (R DPlazaWalkway)
                                    ,Edge spider (I IceRuinsEastSpiderTrack)
                                    ,Edge plasma (I IceRuinsEastBehindIce)]
            ,Room DPlazaWalkway [Edge noReq (R DIceRuinsEast)
                                    ,Edge noReq (R DPhendranaShorelines)]
            ,Room DRuinsEntryway [Edge noReq (R DPhendranaShorelines)
                                    ,Edge noReq (R DIceRuinsWest)]
            ,Room DIceRuinsWest [Edge noReq (R DRuinsEntryway)
                                    ,Edge missile (R DCanyonEntryway)
                                    ,Edge irwDoor (R DCourtyardEntryway)
                                    ,Edge irwItem (I IceRuinsWest)]
            ,Room DCanyonEntryway [Edge noReq (R DIceRuinsWest)
                                    ,Edge noReq (R DPhendranaCanyon)]
            ,Room DPhendranaCanyon [Edge noReq (R DCanyonEntryway)
                                    ,Edge noReq (I PhendranaCanyon)]
            ,Room DCourtyardEntryway [Edge noReq (R DIceRuinsWest)
                                    ,Edge ruinedCourtyardClimb (R DRuinedCourtyard)] -- Ruined courtyard spawn is at the top of the room
            ,Room DRuinedCourtyard [Edge noReq (R DCourtyardEntryway)
                                    ,Edge ruinedCourtyardSave (R DSaveStationA)
                                    ,Edge wave (R DSpecimenStorage)
                                    ,Edge ruinedCourtyardConduit (R DQuarantineAccess) 
                                    ,Edge morph (I RuinedCourtyard)]
            ,Room DSaveStationA [Edge missile (R DCourtyardEntryway) -- If you fall
                                    ,Edge ruinedCourtyardSave (R DRuinedCourtyard) -- If can make it to the spawn point
                                    ,Edge morph (I RuinedCourtyard)] -- You can grab the item by falling here, without reaching the warp
            ,Room DQuarantineAccess [Edge noReq (R DRuinedCourtyard)
                                    ,Edge noReq (R DNorthQuarantineTunnel)]
            ,Room DNorthQuarantineTunnel [Edge wave (R DQuarantineAccess)
                                    ,Edge quarantineTunnel (R DQuarantineCave)]
            ,Room DQuarantineCave [Edge quarantineTunnel (R DNorthQuarantineTunnel)
                                    ,Edge spider (R DQuarantineCaveBack)
                                    ,Edge noReq (I QuarantineCave)]
            -- Added a new "room" representing the other door in quarantine cave
            ,Room DQuarantineCaveBack [Edge grapple (R DQuarantineMonitor)
                                    ,Edge quarantineTunnel (R DSouthQuarantineTunnel)]
            ,Room DQuarantineMonitor [Edge grapple (R DQuarantineCaveBack)
                                    ,Edge spider (R DQuarantineCave)
                                    ,Edge noReq (I QuarantineCave) -- Can drop into thardus fight
                                    ,Edge noReq (I QuarantineMonitor)]
            ,Room DSouthQuarantineTunnel [Edge quarantineTunnel (R DQuarantineCaveBack)
                                    ,Edge wave (R DTransporttoMagmoorCavernsSouth)]
            ,Room DTransporttoMagmoorCavernsSouth [Edge wave (R DSouthQuarantineTunnel)
                                    ,Edge noReq (R CTransporttoPhendranaDriftsSouth)
                                    ,Edge phenElevatorClimb (R DTransportAccess)]
            ,Room DTransportAccess [Edge ice (R DTransporttoMagmoorCavernsSouth)
                                    ,Edge wave (R DFrozenPike)
                                    ,Edge plasma (I TransportAccess)]
            ,Room DSpecimenStorage [Edge wave (R DRuinedCourtyard)
                                    ,Edge wave (R DResearchEntrance)]
            ,Room DResearchEntrance [Edge wave (R DSpecimenStorage)
                                    ,Edge noReq (R DMapStation)
                                    ,Edge wave (R DHydraLabEntryway)]
            ,Room DHydraLabEntryway [Edge wave (R DResearchEntrance)
                                    ,Edge wave (R DResearchLabHydra)]
            ,Room DResearchLabHydra [Edge wave (R DHydraLabEntryway)
                                    ,Edge wave (R DObservatoryAccess)
                                    ,Edge supers (I ResearchLabHydra)]
            ,Room DObservatoryAccess [Edge wave (R DResearchLabHydra)
                                    ,Edge wave (R DObservatory)]
            ,Room DObservatory [Edge wave (R DObservatoryAccess)
                                    ,Edge observatoryClimb (R DWestTowerEntrance)
                                    ,Edge observatorySave (R DSaveStationD)
                                    ,Edge observatoryClimb (I Observatory)]
            ,Room DSaveStationD [Edge missile (R DObservatory)] -- May want to make item accessible from here
            ,Room DWestTowerEntrance [Edge wave (R DObservatory)
                                    ,Edge missile (R DWestTower)]
            ,Room DWestTower [Edge missile (R DWestTowerEntrance)
                                    ,Edge wave (R DControlTower)]
            ,Room DControlTower [Edge wave (R DWestTower)
                                    ,Edge wave (R DEastTower)
                                    ,Edge controlTowerItem (I ControlTower)]
            ,Room DEastTower [Edge wave (R DControlTower)
                                    ,Edge wave (R DAetherLabEntryway)]
            ,Room DAetherLabEntryway [Edge wave (R DEastTower)
                                    ,Edge wave (R DResearchLabAether)]
            ,Room DResearchLabAether [Edge wave (R DAetherLabEntryway)
                                    ,Edge wave (R DResearchCoreAccess)
                                    ,Edge missile (I ResearchLabAetherTank)
                                    ,Edge rlaTrack (I ResearchLabAetherMorphTrack)]
            ,Room DResearchCoreAccess [Edge wave (R DResearchLabAether)
                                    ,Edge wave (R DResearchCore)]
            ,Room DResearchCore [Edge wave (R DResearchCoreAccess)
                                    ,Edge ice (R DPikeAccess)
                                    ,Edge noReq (I ResearchCore)]
            ,Room DPikeAccess [Edge ice (R DResearchCore)
                                    ,Edge wave (R DFrozenPike)]
            ,Room DFrozenPike [Edge frozenPikeClimb (R DTransportAccess)
                                    ,Edge wave (R DPikeAccess)
                                    ,Edge wave (R DFrostCaveAccess)
                                    ,Edge frozenPikeBottom (R DHunterCaveAccess)]
            ,Room DFrostCaveAccess [Edge wave (R DFrozenPike)
                                    ,Edge frostCaveAccess (R DFrostCave)]
            ,Room DFrostCave [Edge frostCaveAccess (R DFrostCaveAccess)
                                    ,Edge frostCaveDoor (R DSaveStationC)
                                    ,Edge frostCaveToTunnel (R DUpperEdgeTunnel)
                                    ,Edge frostCaveItem (I FrostCave)]
            ,Room DSaveStationC [Edge frostCaveDoor (R DFrostCave)]
            ,Room DUpperEdgeTunnel [Edge frostCaveAccess (R DFrostCave)
                                    ,Edge wave (R DPhendranasEdge)]
            ,Room DPhendranasEdge [Edge wave (R DUpperEdgeTunnel)
                                    ,Edge toStorageCave (R DStorageCave)
                                    ,Edge toSecurityCave (R DSecurityCave)
                                    ,Edge noReq (R DLowerEdgeTunnel)]
            ,Room DStorageCave [Edge fromStorageCave (R DPhendranasEdge)
                                    ,Edge noReq (I StorageCave)]
            ,Room DSecurityCave [Edge morph (R DPhendranasEdge)
                                    ,Edge noReq (I SecurityCave)]
            ,Room DLowerEdgeTunnel [Edge phenEdgeLower (R DPhendranasEdge)
                                    ,Edge wave (R DHunterCave)]
            ,Room DHunterCave [Edge wave (R DLowerEdgeTunnel)
                                    ,Edge hunterCaveLower (R DLakeTunnel)
                                    ,Edge hunterCaveUpper (R DHunterCaveFar)]
            ,Room DHunterCaveFar [Edge sj (R DHunterCave)
                                    ,Edge wave (R DChamberAccess)
                                    ,Edge wave (R DHunterCaveAccess)]
            ,Room DLakeTunnel [Edge hunterCaveClimb (R DHunterCave)
                                    ,Edge wave (R DGravityChamber)]
            ,Room DGravityChamber [Edge gravityChamberToLakeTunnel (R DLakeTunnel)
                                    ,Edge climbGravityChamber (R DGravityChamberTop)
                                    ,Edge noReq (I GravityChamberUnderwater)]
            ,Room DGravityChamberTop [Edge noReq (R DGravityChamber)
                                    ,Edge wave (R DChamberAccess)
                                    ,Edge gravLedge (I GravityChamberGrappleLedge)]
            ,Room DChamberAccess [Edge wave (R DGravityChamberTop)
                                    ,Edge wave (R DHunterCaveFar)]
            ,Room DHunterCaveAccess [Edge wave (R DHunterCaveFar)
                                    ,Edge frozenPikeBottom (R DFrozenPike)]
                                    ]