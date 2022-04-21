module Node where

data Id = R RoomId | I ItemId
        deriving  (Read, Eq, Ord, Show)

data Node = Room {roomId :: RoomId, edges :: [Edge]} | Item {itemId :: ItemId, itemName :: ItemName, warp :: RoomId}
        deriving (Show)

data Edge = Edge {canUse :: [ItemName] -> Bool, nodeId :: Id}

instance Show Edge where 
    show (Edge _ nodeId) = show nodeId

data ItemName = MorphBall | MorphBallBomb | IceBeam | WaveBeam | PlasmaBeam | SpaceJumpBoots | PhazonSuit | GravitySuit 
                | VariaSuit | SpiderBall | BoostBall | PowerBomb | ChargeBeam | SuperMissile | XRayVisor | GrappleBeam
                | ThermalVisor | Missile | EnergyTank | Wavebuster | IceSpreader | Flamethrower | Artifact

                | FrigatePowerDoor | MainQuarryBarriers | ChozoIceTempleBarrier
                deriving  (Read, Eq, Ord, Show, Enum)

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
                | DGravityChamber | DChamberAccess | DHunterCaveAccess | DQuarantineCaveBack | DGravityChamberTop | DHunterCaveFar | DObservatoryTop

                | MTransporttoTallonOverworldSouth | MQuarryAccess | MMainQuarry | MSaveStationMinesA | MSecurityAccessA | MMineSecurityStation | MSecurityAccessB 
                | MStorageDepotA | MEliteResearch | MResearchAccess | MOreProcessing | MElevatorAccessA | MElevatorA | MStorageDepotB | MWasteDisposal 
                | MEliteControlAccess | MEliteControl | MMaintenanceTunnel | MVentilationShaft | MControlRoom | MOmegaResearch | MMapStationMines | MDynamoAccess 
                | MCentralDynamo | MSaveStationMinesB | MQuarantineAccessA | MMetroidQuarantineA | MElevatorAccessB | MElevatorB | MFungalHallAccess | MFungalHallA 
                | MPhazonMiningTunnel | MFungalHallB | MMissileStationMines | MQuarantineAccessB | MMetroidQuarantineB | MSaveStationMinesC | MEliteQuartersAccess 
                | MEliteQuarters | MProcessingCenterAccess | MPhazonProcessingCenter | MTransportAccess | MTransporttoMagmoorCavernsSouth | MMetroidQuarantineABack
                | MMetroidQuarantineBBack
                deriving  (Read, Eq, Ord, Show, Enum)
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

                | FrigatePowerDoorTrigger | MainQuarryBarrierTriggers | ChozoIceTempleTrigger

                --Possible pseudo items: Ruined Fountain Collected, Maze item, opened OP backdoor in mines, Sunchamber, HOTE statue
                -- Research Lab Hydra barrier, Mine Security Station?
                deriving  (Read, Eq, Ord, Show, Enum)

data Difficulty = Easy | Medium | Hard | VeryHard | Extreme

-- Basic Predicates
noReq :: [ItemName] -> Bool
noReq _ = True 

blocked :: [ItemName] -> Bool
blocked _ = False

complete :: [ItemName] -> Bool
complete = containsCount 12 Artifact

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

pb :: [ItemName] -> Bool
pb x = containsAll x [PowerBomb, MorphBall]

boost :: [ItemName] -> Bool
boost x = containsAll x [MorphBall, BoostBall]

spider :: [ItemName] -> Bool
spider x = containsAll x [MorphBall, SpiderBall]

waveIce :: [ItemName] -> Bool
waveIce x = containsAll x [WaveBeam, PlasmaBeam]

grapple :: [ItemName] -> Bool
grapple x = contains x GrappleBeam

boostBombs :: [ItemName] -> Bool
boostBombs x = containsAll x [MorphBall, BoostBall, MorphBallBomb]

morphMissile :: [ItemName] -> Bool
morphMissile x = containsAll x [MorphBall, Missile]

bombsPbs :: [ItemName] -> Bool
bombsPbs x = containsAll x [MorphBall, MorphBallBomb, PowerBomb]

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

gravSpace :: [ItemName] -> Bool
gravSpace x = containsAll x [GravitySuit, SpaceJumpBoots]

wavePb :: [ItemName] -> Bool
wavePb x = wave x && pb x

heatResist :: [ItemName] -> Bool
heatResist x  = containsAny x [VariaSuit, GravitySuit, PhazonSuit]

floaty :: [ItemName] -> Bool
floaty x = not $ contains x GravitySuit

-- A bit of an obscure trick, you can use infinite speed in landing site to unload the room
tallonFloaty :: [ItemName] -> Bool
tallonFloaty x = boost x && floaty x

mainQuarryBarrierIce :: [ItemName] -> Bool
mainQuarryBarrierIce x = containsAll x [MainQuarryBarriers, IceBeam]

mainQuarryBarrierWave :: [ItemName] -> Bool
mainQuarryBarrierWave x = containsAll x [MainQuarryBarriers, WaveBeam]

chozoIceTempleBarrier :: [ItemName] -> Bool
chozoIceTempleBarrier x = contains x ChozoIceTempleBarrier

-- Tallon Predicates
sjf :: Difficulty -> [ItemName] -> Bool
sjf diff x = case diff of 
    Easy -> sj x
    Medium -> sj x
    Hard -> True
    VeryHard -> True
    Extreme -> True

rootCaveItem :: Difficulty -> [ItemName] -> Bool
rootCaveItem diff x = case diff of 
    Easy -> containsAll x [SpaceJumpBoots, GrappleBeam, XRayVisor]
    Medium -> containsAll x [SpaceJumpBoots, GrappleBeam]
    Hard -> contains x SpaceJumpBoots
    VeryHard -> contains x SpaceJumpBoots
    Extreme -> contains x SpaceJumpBoots

arbor :: Difficulty -> [ItemName] -> Bool
arbor diff x = case diff of 
    Easy -> containsAll x [SpaceJumpBoots, GrappleBeam, XRayVisor, PlasmaBeam]
    Medium -> containsAll x [SpaceJumpBoots, GrappleBeam, PlasmaBeam]
    Hard -> containsAll x [SpaceJumpBoots, PlasmaBeam]
    VeryHard -> contains x PlasmaBeam
    Extreme -> contains x PlasmaBeam

fcsClimb :: Difficulty -> [ItemName] -> Bool
fcsClimb diff x = case diff of 
    Easy -> False
    Medium -> contains x SpaceJumpBoots && contains x IceBeam
    Hard -> contains x SpaceJumpBoots && contains x IceBeam
    VeryHard -> sjOrBombs x && contains x IceBeam
    Extreme -> sjOrBombs x && contains x IceBeam

frigatePowerDoor :: Difficulty -> [ItemName] -> Bool
frigatePowerDoor diff x = case diff of 
    Easy -> contains x FrigatePowerDoor
    Medium -> contains x FrigatePowerDoor
    Hard -> contains x FrigatePowerDoor
    VeryHard -> bombs x || contains x FrigatePowerDoor
    Extreme -> bombs x || contains x FrigatePowerDoor

fcsEntry :: Difficulty -> [ItemName] -> Bool
fcsEntry diff x = case diff of 
    Easy -> contains x IceBeam && (contains x GrappleBeam || (contains x MorphBall && (contains x SpaceJumpBoots || contains x GravitySuit)))
    Medium -> contains x IceBeam && (contains x GrappleBeam || contains x MorphBall)
    Hard -> contains x IceBeam && (contains x GrappleBeam || contains x MorphBall || contains x SpaceJumpBoots)
    VeryHard -> contains x IceBeam && (contains x GrappleBeam || contains x MorphBall || contains x SpaceJumpBoots)
    Extreme -> contains x IceBeam && (contains x GrappleBeam || contains x MorphBall || contains x SpaceJumpBoots) || (contains x SpaceJumpBoots && tallonFloaty x)
    
fcsItem :: Difficulty -> [ItemName] -> Bool
fcsItem diff x = case diff of 
    Easy -> sj x && contains x GravitySuit
    Medium -> sjOrBombs x && contains x GravitySuit
    Hard -> True
    VeryHard -> True
    Extreme -> True
    
climbFrigateMvs :: Difficulty -> [ItemName] -> Bool
climbFrigateMvs diff x = case diff of
    Easy -> sj x
    Medium -> sj x
    Hard -> sjOrBombs x
    VeryHard -> sjOrBombs x
    Extreme -> sjOrBombs x

climbReactorCore :: Difficulty -> [ItemName] -> Bool
climbReactorCore diff x = case diff of
    Easy -> sj x
    Medium -> sj x
    Hard -> sj x || containsAll x [GravitySuit, MorphBall, MorphBallBomb]
    VeryHard -> sj x || containsAll x [GravitySuit, MorphBall, MorphBallBomb]
    Extreme -> sj x || containsAll x [GravitySuit, MorphBall, MorphBallBomb]

cargoFreightLift :: Difficulty -> [ItemName] -> Bool
cargoFreightLift diff x = case diff of 
    Easy -> containsAll x [WaveBeam, GravitySuit, SpaceJumpBoots]
    Medium -> contains x WaveBeam && (bombs x || containsAll x [GravitySuit, SpaceJumpBoots])
    Hard -> contains x WaveBeam && (bombs x || containsAll x [GravitySuit, SpaceJumpBoots])
    VeryHard -> contains x WaveBeam && (bombs x || containsAll x [GravitySuit, SpaceJumpBoots])
    Extreme -> contains x WaveBeam && (bombs x || containsAll x [GravitySuit, SpaceJumpBoots])

biohazard :: Difficulty -> [ItemName] -> Bool
biohazard _ x = contains x WaveBeam

--TODO revisit this for accuracy
climbBiohazard :: Difficulty -> [ItemName] -> Bool
climbBiohazard diff x = case diff of 
    Easy -> containsAll x [GravitySuit, SpaceJumpBoots]
    Medium -> sj x || (contains x GravitySuit && bombs x)
    Hard -> sj x || contains x GravitySuit || bombs x
    VeryHard -> sj x ||  contains x GravitySuit || bombs x
    Extreme -> sj x || contains x GravitySuit || bombs x

biotech :: Difficulty -> [ItemName] -> Bool
biotech diff x = case diff of 
    Easy -> containsAll x [GravitySuit, SpaceJumpBoots, WaveBeam]
    Medium -> contains x WaveBeam && (sj x || (contains x GravitySuit && bombs x))
    Hard -> contains x WaveBeam
    VeryHard -> contains x WaveBeam
    Extreme -> contains x WaveBeam

--TODO revisit this for accuracy
biotechReverse :: Difficulty -> [ItemName] -> Bool
biotechReverse diff x = case diff of 
    Easy -> containsAll x [GravitySuit, SpaceJumpBoots]
    Medium -> sj x || (contains x GravitySuit && bombs x)
    Hard -> True
    VeryHard -> True
    Extreme -> True

lgUnderWater :: Difficulty -> [ItemName] -> Bool
lgUnderWater diff x = case diff of 
    Easy -> containsAll x [MorphBall, BoostBall, PowerBomb, SpaceJumpBoots]
    Medium -> containsAll x [MorphBall, BoostBall, PowerBomb] && sjOrBombs x
    Hard -> containsAll x [MorphBall, BoostBall, PowerBomb] && sjOrBombs x
    VeryHard -> pb x && sjOrBombs x
    Extreme -> pb x && sjOrBombs x

hydroTunnel ::  Difficulty -> [ItemName] -> Bool
hydroTunnel diff x = case diff of 
    Easy -> containsAll x [GravitySuit, MorphBall, MorphBallBomb]
    Medium -> containsAll x [GravitySuit, MorphBall, MorphBallBomb]
    Hard -> containsAll x [GravitySuit, MorphBall, MorphBallBomb] || boost x
    VeryHard -> containsAll x [GravitySuit, MorphBall, MorphBallBomb] || boost x
    Extreme -> containsAll x [GravitySuit, MorphBall, MorphBallBomb] || boost x

gthClimb :: Difficulty -> [ItemName] -> Bool
gthClimb diff x = case diff of 
    Easy -> containsAll x [SpaceJumpBoots, BoostBall, MorphBall]
    Medium -> containsAll x [SpaceJumpBoots, BoostBall, MorphBall]
    Hard -> containsAll x [SpaceJumpBoots, MorphBall] && containsAny x [BoostBall, MorphBallBomb]
    VeryHard -> containsAll x [SpaceJumpBoots, MorphBall] && containsAny x [BoostBall, MorphBallBomb]
    Extreme -> contains x MorphBall && containsAny x [BoostBall, MorphBallBomb]

bars :: Difficulty -> [ItemName] -> Bool
bars diff x = case diff of 
    Easy -> False
    Medium -> False
    Hard -> bombs x
    VeryHard -> bombs x
    Extreme -> bombs x

lifeGroveTunnel :: Difficulty -> [ItemName] -> Bool
lifeGroveTunnel diff x = case diff of 
    Easy -> containsAll x [PowerBomb, MorphBall, BoostBall]
    Medium -> containsAll x [PowerBomb, MorphBall, BoostBall]
    Hard -> containsAll x [PowerBomb, MorphBall, BoostBall]
    VeryHard -> containsAll x [PowerBomb, MorphBall] && containsAny x [MorphBallBomb, BoostBall]
    Extreme -> containsAll x [PowerBomb, MorphBall] && containsAny x [MorphBallBomb, BoostBall]

lifeGroveTunnelItem :: Difficulty -> [ItemName] -> Bool
lifeGroveTunnelItem diff x = case diff of 
    Easy -> containsAll x [PowerBomb, MorphBall, BoostBall, MorphBallBomb]
    Medium -> containsAll x [PowerBomb, MorphBall, BoostBall, MorphBallBomb]
    Hard -> containsAll x [PowerBomb, MorphBall, BoostBall, MorphBallBomb]
    VeryHard -> containsAll x [PowerBomb, MorphBall, MorphBallBomb]
    Extreme -> containsAll x [PowerBomb, MorphBall, MorphBallBomb]

gthSpiderTrack ::  Difficulty -> [ItemName] -> Bool
gthSpiderTrack diff x = case diff of 
    Easy -> containsAll x [SpiderBall, MorphBall, SpaceJumpBoots]
    Medium -> containsAll x [SpiderBall, MorphBall, SpaceJumpBoots]
    Hard -> containsAll x [SpiderBall, MorphBall, SpaceJumpBoots] 
    VeryHard -> containsAll x [SpiderBall, MorphBall, SpaceJumpBoots] 
    Extreme -> containsAll x [SpiderBall, MorphBall, SpaceJumpBoots]  || bombs x

gtcEnter ::  Difficulty -> [ItemName] -> Bool
gtcEnter diff x = case diff of 
    Easy -> sj x
    Medium -> sj x
    Hard -> sjOrBombs x
    VeryHard -> sjOrBombs x
    Extreme -> sjOrBombs x

-- Chozo Predicates
mainPipe :: Difficulty -> [ItemName] -> Bool
mainPipe diff x = case diff of 
    Easy -> sj x || boost x 
    Medium -> sj x || boost x 
    Hard -> sj x || boost x 
    VeryHard -> sj x || boost x || bombs x
    Extreme -> sj x || boost x || bombs x

mainPlazaGrappleLedge :: Difficulty -> [ItemName] -> Bool
mainPlazaGrappleLedge diff x = case diff of 
    Easy -> contains x GrappleBeam
    Medium -> contains x GrappleBeam
    Hard -> containsAny x [GrappleBeam, SpaceJumpBoots]
    VeryHard -> containsAny x [GrappleBeam, SpaceJumpBoots]
    Extreme -> containsAny x [GrappleBeam, SpaceJumpBoots]

mainPlazaLedge :: Difficulty -> [ItemName] -> Bool
mainPlazaLedge diff x = case diff of 
    Easy -> False
    Medium -> contains x SpaceJumpBoots
    Hard -> contains x SpaceJumpBoots
    VeryHard -> contains x SpaceJumpBoots
    Extreme -> contains x SpaceJumpBoots

ruinedFountainItem :: Difficulty -> [ItemName] -> Bool
ruinedFountainItem _ x = contains x SpiderBall

towerChamber :: Difficulty -> [ItemName] -> Bool
towerChamber diff x = case diff of 
    Easy -> containsAll x [GravitySuit, SpaceJumpBoots, WaveBeam]
    Medium -> containsAll x [SpaceJumpBoots, WaveBeam]
    Hard -> contains x WaveBeam
    VeryHard -> contains x WaveBeam
    Extreme -> contains x WaveBeam
     
rsHalf :: Difficulty -> [ItemName] -> Bool
rsHalf diff x = case diff of 
    Easy -> boost x
    Medium -> boost x || containsAll x [SpaceJumpBoots, MorphBall]
    Hard -> boost x || containsAll x [SpaceJumpBoots, MorphBall]
    VeryHard -> contains x MorphBall
    Extreme -> contains x MorphBall

tolAccess :: Difficulty -> [ItemName] -> Bool
tolAccess diff x = case diff of 
    Easy -> containsAll x [MorphBall, BoostBall, SpiderBall, WaveBeam]
    Medium -> containsAll x [MorphBall, BoostBall, SpiderBall, WaveBeam] || containsAll x [SpaceJumpBoots, WaveBeam]
    Hard -> containsAll x [MorphBall, BoostBall, SpiderBall, WaveBeam] || containsAll x [SpaceJumpBoots, WaveBeam]
    VeryHard -> contains x WaveBeam
    Extreme -> contains x WaveBeam

towerOfLight :: Difficulty -> [ItemName] -> Bool
towerOfLight diff x = case diff of 
    Easy -> containsCount 8 Missile x && sj x
    Medium -> containsCount 8 Missile x && sj x
    Hard -> sj x
    VeryHard -> (containsCount 8 Missile x && bombs x) || sj x
    Extreme -> (containsCount 8 Missile x && bombs x) || sj x

crossMagmaPool :: Difficulty -> [ItemName] -> Bool
crossMagmaPool diff x  = case diff of 
    Easy -> heatResist x && containsAll x [GrappleBeam,WaveBeam]
    Medium -> heatResist x && containsAll x [GrappleBeam,WaveBeam]
    Hard -> ((heatResist x && contains x GrappleBeam) || sj x) && wave x
    VeryHard -> ((heatResist x && contains x GrappleBeam) || sj x) && wave x
    Extreme -> ((heatResist x && contains x GrappleBeam) || sj x) && wave x

--TODO check how many etanks this requires
magmaPoolItem :: Difficulty -> [ItemName] -> Bool
magmaPoolItem diff x  = case diff of 
    Easy -> heatResist x && containsAll x [GrappleBeam,MorphBall,PowerBomb]
    Medium -> heatResist x && containsAll x [GrappleBeam,MorphBall,PowerBomb]
    Hard -> heatResist x && ((containsAny x [GrappleBeam,SpaceJumpBoots] && pb x) || boost x)
    VeryHard -> (grapple x && pb x && heatResist x) || (sj x && pb x && (containsCount 3 EnergyTank x ||heatResist x)) || (boost x && (containsCount 7 EnergyTank x || heatResist x))
    Extreme -> (grapple x && pb x && heatResist x) || (sj x && pb x && (containsCount 3 EnergyTank x ||heatResist x)) || (boost x && (containsCount 7 EnergyTank x || heatResist x))

tcItem :: Difficulty -> [ItemName] -> Bool
tcItem _ x = containsAll x [MorphBall, BoostBall, MorphBallBomb, SpiderBall]

tcTunnel :: Difficulty -> [ItemName] -> Bool
tcTunnel _ = boostBombs

climbSunTower :: Difficulty -> [ItemName] -> Bool
climbSunTower _ x = containsAll x [MorphBall, SpiderBall, MorphBallBomb, Missile, SuperMissile, ChargeBeam]

sunchamberghost :: Difficulty -> [ItemName] -> Bool
sunchamberghost = climbSunTower

wateryHallTraverse :: Difficulty -> [ItemName] -> Bool 
wateryHallTraverse diff x = containsAll x [MorphBall, MorphBallBomb, Missile]

wateryHallWater :: Difficulty -> [ItemName] -> Bool 
wateryHallWater diff x = case diff of 
    Easy -> contains x GravitySuit && (contains x SpaceJumpBoots || bombs x) 
    Medium -> contains x GravitySuit && (contains x SpaceJumpBoots || bombs x) 
    Hard -> True
    VeryHard ->  True
    Extreme ->  True

furnaceTraverse :: Difficulty -> [ItemName] -> Bool 
furnaceTraverse diff x = case diff of 
    Easy -> containsAll x [MorphBall, MorphBallBomb, SpiderBall]
    Medium -> containsAll x [MorphBall, MorphBallBomb]
    Hard -> containsAll x [MorphBall, MorphBallBomb]
    VeryHard ->  containsAll x [MorphBall, MorphBallBomb]
    Extreme ->  containsAll x [MorphBall, MorphBallBomb]

furnaceItem :: Difficulty -> [ItemName] -> Bool 
furnaceItem diff x = case diff of 
    Easy ->containsAll x [MorphBall, MorphBallBomb, SpiderBall, PowerBomb, BoostBall]
    Medium -> containsAll x [MorphBall, MorphBallBomb, SpiderBall, PowerBomb, BoostBall]
    Hard -> containsAll x [MorphBall, MorphBallBomb, SpiderBall] && (containsAll x [PowerBomb, BoostBall] || sj x)
    VeryHard ->  containsAll x [MorphBall, MorphBallBomb, SpiderBall] && (containsAll x [PowerBomb, BoostBall] || sj x)
    Extreme ->  containsAll x [MorphBall, MorphBallBomb, SpiderBall] && (containsAll x [PowerBomb, BoostBall] || sj x)

crosswayTraverse :: Difficulty -> [ItemName] -> Bool 
crosswayTraverse diff x = case diff of 
    Easy -> containsAll x [MorphBall, BoostBall, Missile]
    Medium -> contains x Missile && (boost x || sj x)
    Hard -> contains x Missile && (boost x || sj x)
    VeryHard -> contains x Missile && (boost x || sj x || bombs x)
    Extreme -> contains x Missile && (boost x || sj x || bombs x)

crosswayItem :: Difficulty -> [ItemName] -> Bool 
crosswayItem diff x = case diff of 
    Easy -> containsAll x [MorphBall, BoostBall, Missile, SuperMissile, ChargeBeam, SpiderBall, MorphBallBomb]
    Medium -> containsAll x [MorphBall, BoostBall, Missile, SuperMissile, ChargeBeam, SpiderBall, MorphBallBomb]
    Hard -> (sj x && contains x MorphBall) || containsAll x [MorphBall, BoostBall, Missile, SuperMissile, ChargeBeam, SpiderBall, MorphBallBomb]
    VeryHard -> (sj x && contains x MorphBall) || containsAll x [MorphBall, BoostBall, Missile, SuperMissile, ChargeBeam, SpiderBall, MorphBallBomb]
    Extreme -> (sj x && contains x MorphBall) || containsAll x [MorphBall, BoostBall, Missile, SuperMissile, ChargeBeam, SpiderBall, MorphBallBomb]

hoteWave :: Difficulty -> [ItemName] -> Bool 
hoteWave diff x = case diff of 
    Easy -> containsAll x [MorphBall, SpiderBall, MorphBallBomb, WaveBeam]
    Medium -> containsAll x [MorphBall, MorphBallBomb, WaveBeam] && (spider x || sj x)
    Hard -> containsAll x [MorphBall, MorphBallBomb, WaveBeam] && (spider x || sj x)
    VeryHard -> containsAll x [MorphBall, MorphBallBomb, WaveBeam]
    Extreme -> containsAll x [MorphBall, MorphBallBomb, WaveBeam]

hoteIce :: Difficulty -> [ItemName] -> Bool 
hoteIce diff x = case diff of 
    Easy -> containsAll x [MorphBall, SpiderBall, MorphBallBomb, IceBeam]
    Medium -> containsAll x [MorphBall, MorphBallBomb, IceBeam] && (spider x || sj x)
    Hard -> containsAll x [MorphBall, MorphBallBomb, IceBeam] && (spider x || sj x)
    VeryHard -> containsAll x [MorphBall, MorphBallBomb, IceBeam]
    Extreme -> containsAll x [MorphBall, MorphBallBomb, IceBeam]

-- TODO maybe add infinite speed?
hotePlasma :: Difficulty -> [ItemName] -> Bool 
hotePlasma diff x = case diff of 
    Easy -> containsAll x [MorphBall, SpiderBall, MorphBallBomb, IceBeam, PlasmaBeam]
    Medium -> containsAll x [MorphBall, MorphBallBomb, IceBeam, PlasmaBeam] && (spider x || sj x)
    Hard -> containsAll x [MorphBall, MorphBallBomb, IceBeam, PlasmaBeam] && (spider x || sj x)
    VeryHard -> containsAll x [MorphBall, MorphBallBomb, IceBeam, PlasmaBeam]
    Extreme -> containsAll x [MorphBall, MorphBallBomb, IceBeam, PlasmaBeam]

reflectPoolSave :: Difficulty -> [ItemName] -> Bool 
reflectPoolSave diff x = reflectPoolAntechamber diff x && contains x Missile

reflectPoolIceDoor :: Difficulty -> [ItemName] -> Bool 
reflectPoolIceDoor diff x = reflectPoolAntechamber diff x && contains x IceBeam

reflectPoolAntechamber :: Difficulty -> [ItemName] -> Bool 
reflectPoolAntechamber diff x = case diff of 
    Easy -> containsAll x [MorphBall, MorphBallBomb, BoostBall]
    Medium -> containsAll x [MorphBall, MorphBallBomb, BoostBall] || sj x
    Hard -> containsAll x [MorphBall, MorphBallBomb, BoostBall] || sj x
    VeryHard -> bombs x || sj x
    Extreme -> bombs x || sj x

-- Magmoor Predicates
vmr1Tank :: Difficulty -> [ItemName] -> Bool 
vmr1Tank diff x = case diff of 
    Easy -> heatResist x
    Medium -> heatResist x
    Hard -> heatResist x || containsCount 2 EnergyTank x && sj x
    VeryHard -> heatResist x || (containsCount 1 EnergyTank x && sj x) || containsCount 2 EnergyTank x
    Extreme -> heatResist x || (containsCount 1 EnergyTank x && sj x) || containsCount 2 EnergyTank x

vmr2Tank :: Difficulty -> [ItemName] -> Bool 
vmr2Tank diff x = case diff of 
    Easy -> heatResist x
    Medium -> heatResist x
    Hard -> heatResist x || containsCount 3 EnergyTank x && sj x
    VeryHard -> heatResist x || (containsCount 2 EnergyTank x && sj x) || containsCount 3 EnergyTank x
    Extreme -> heatResist x || (containsCount 2 EnergyTank x && sj x) || containsCount 3 EnergyTank x

vmr3Tank :: Difficulty -> [ItemName] -> Bool 
vmr3Tank diff x = case diff of 
    Easy -> heatResist x
    Medium -> heatResist x
    Hard -> heatResist x || containsCount 4 EnergyTank x && sj x
    VeryHard -> heatResist x || (containsCount 3 EnergyTank x && sj x) || containsCount 4 EnergyTank x
    Extreme -> heatResist x || (containsCount 3 EnergyTank x && sj x) || containsCount 4 EnergyTank x

vmr4Tank :: Difficulty -> [ItemName] -> Bool 
vmr4Tank diff x = case diff of 
    Easy -> heatResist x
    Medium -> heatResist x
    Hard -> heatResist x || containsCount 5 EnergyTank x && sj x
    VeryHard -> heatResist x || (containsCount 4 EnergyTank x && sj x) || containsCount 5 EnergyTank x
    Extreme -> heatResist x || (containsCount 4 EnergyTank x && sj x) || containsCount 5 EnergyTank x

heatResistOr8Etanks :: Difficulty -> [ItemName] -> Bool 
heatResistOr8Etanks diff x = case diff of 
    Easy -> heatResist x
    Medium -> heatResist x
    Hard -> heatResist x || containsCount 8 EnergyTank x
    VeryHard -> heatResist x || containsCount 8 EnergyTank x
    Extreme -> heatResist x || containsCount 8 EnergyTank x

lavaLakeTraversal :: Difficulty -> [ItemName] -> Bool 
lavaLakeTraversal diff x = vmr4Tank diff x && bombs x

lavaLakeReverseTraversal :: Difficulty -> [ItemName] -> Bool 
lavaLakeReverseTraversal diff x = vmr2Tank diff x && bombs x

lavaLakeItem :: Difficulty -> [ItemName] -> Bool 
lavaLakeItem diff x = case diff of 
    Easy -> missile x && sj x && heatResist x
    Medium -> missile x && sj x && heatResist x
    Hard -> missile x && (heatResist x || (sj x && containsCount 2 EnergyTank x))
    VeryHard -> missile x && (heatResist x || (sj x && contains x EnergyTank) || containsCount 2 EnergyTank x)
    Extreme -> missile x && (heatResist x || (sj x && contains x EnergyTank) || containsCount 2 EnergyTank x)

pitTunnel :: Difficulty -> [ItemName] -> Bool 
pitTunnel diff x = vmr2Tank diff x && contains x MorphBall

pitTunnelReverse :: Difficulty -> [ItemName] -> Bool 
pitTunnelReverse diff x = vmr3Tank diff x && contains x MorphBall

triclopsPitItem :: Difficulty -> [ItemName] -> Bool 
triclopsPitItem diff x = case diff of 
    Easy -> containsAll x [SpaceJumpBoots, Missile] && heatResist x
    Medium -> containsAll x [SpaceJumpBoots, Missile] && heatResist x
    Hard -> missile x && vmr1Tank diff x
    VeryHard -> missile x && vmr1Tank diff x
    Extreme -> missile x && vmr1Tank diff x

storageCavern :: Difficulty -> [ItemName] -> Bool 
storageCavern diff x = morph x && vmr1Tank diff x

toTransportTunnelA :: Difficulty -> [ItemName] -> Bool 
toTransportTunnelA diff x = bombs x && vmr1Tank diff x

monitorStationClimb :: Difficulty -> [ItemName] -> Bool 
monitorStationClimb diff x = case diff of 
    Easy ->heatResist x && containsAll x [SpaceJumpBoots,MorphBall,BoostBall]
    Medium -> heatResist x && containsAll x [SpaceJumpBoots,MorphBall,BoostBall]
    Hard -> vmr3Tank diff x && (sj x || bombs x)
    VeryHard -> vmr3Tank diff x && (sj x || bombs x)
    Extreme -> vmr3Tank diff x && (sj x || bombs x)

warriorShrineTunnel :: Difficulty -> [ItemName] -> Bool
warriorShrineTunnel diff x = vmr4Tank diff x && pb x && bombs x

-- TODO can you use spider without heat resistance?
crossTft :: Difficulty -> [ItemName] -> Bool
crossTft diff x = case diff of 
    Easy -> spider x
    Medium -> spider x
    Hard -> spider x || sj x || (contains x GravitySuit && bombs x && containsCount 2 EnergyTank x)
    VeryHard -> spider x || sj x || (contains x GravitySuit && bombs x && containsCount 2 EnergyTank x)
    Extreme -> spider x || sj x || (contains x GravitySuit && bombs x && containsCount 2 EnergyTank x)

crossTftReverse :: Difficulty -> [ItemName] -> Bool
crossTftReverse diff x = case diff of 
    Easy -> spider x
    Medium -> spider x
    Hard -> spider x || sj x || (contains x GravitySuit && bombs x && containsCount 2 EnergyTank x)
    VeryHard -> spider x || sj x || heatResist x
    Extreme -> spider x || sj x || heatResist x

crossTwinFires :: Difficulty -> [ItemName] -> Bool
crossTwinFires diff x = sjOrBombs x && contains x WaveBeam

crossNorthCoreTunnel :: Difficulty -> [ItemName] -> Bool
crossNorthCoreTunnel diff x = case diff of 
    Easy -> containsAll x [Missile, SpaceJumpBoots, WaveBeam]
    Medium -> containsAll x [Missile, SpaceJumpBoots, WaveBeam]
    Hard -> containsAll x [SpaceJumpBoots, WaveBeam]
    VeryHard -> contains x WaveBeam && (missile x || sj x)
    Extreme -> contains x WaveBeam && (missile x || sj x)

workstationTunnel :: Difficulty -> [ItemName] -> Bool
workstationTunnel diff x = containsAll x [IceBeam, PowerBomb, MorphBall]

workstationItem :: Difficulty -> [ItemName] -> Bool
workstationItem diff x = containsAll x [MorphBall, WaveBeam]

workstationWaveDoor :: Difficulty -> [ItemName] -> Bool
workstationWaveDoor diff x = sjOrBombs x && contains x WaveBeam

geoCore :: Difficulty -> [ItemName] -> Bool
geoCore diff x = case diff of 
    Easy -> containsAll x [SpaceJumpBoots, GrappleBeam, SpiderBall, MorphBall, MorphBallBomb, BoostBall, IceBeam]
    Medium -> containsAll x [SpaceJumpBoots, GrappleBeam, SpiderBall, MorphBall, MorphBallBomb, BoostBall, IceBeam]
    Hard -> containsAll x [SpaceJumpBoots, MorphBall, MorphBallBomb, BoostBall, IceBeam]
    VeryHard -> containsAll x [SpaceJumpBoots, MorphBall, MorphBallBomb, BoostBall, IceBeam]
    Extreme -> containsAll x [SpaceJumpBoots, MorphBall, MorphBallBomb, BoostBall, IceBeam]

-- Phendrana Predicates
iceBarrier :: Difficulty -> [ItemName] -> Bool
iceBarrier _ x = containsAny x [Missile, ChargeBeam]

shorelinesTower :: Difficulty -> [ItemName] -> Bool
shorelinesTower diff x = case diff of 
    Easy -> containsAll x [MorphBall, SpaceJumpBoots, SpiderBall, ChargeBeam, SuperMissile, Missile]
    Medium -> containsAll x [MorphBall, SpaceJumpBoots, SpiderBall, ChargeBeam, SuperMissile, Missile]
    Hard -> containsAll x [MorphBall, SpaceJumpBoots, SpiderBall, ChargeBeam, SuperMissile, Missile]
    VeryHard -> containsAll x [MorphBall, SpiderBall, ChargeBeam, SuperMissile, Missile] && sjOrBombs x
    Extreme -> containsAll x [MorphBall, SpiderBall, ChargeBeam, SuperMissile, Missile] && sjOrBombs x


iceTempleClimb :: Difficulty -> [ItemName] -> Bool
iceTempleClimb diff x = case diff of 
    Easy -> containsAll x [SpaceJumpBoots, MorphBall, MorphBallBomb, Missile]
    Medium -> containsAll x [SpaceJumpBoots, MorphBall, MorphBallBomb, Missile]
    Hard -> containsAll x [MorphBall, MorphBallBomb, Missile]
    VeryHard -> containsAll x [MorphBall, MorphBallBomb, Missile]
    Extreme -> containsAll x [MorphBall, MorphBallBomb, Missile]

iceTempleItem :: Difficulty -> [ItemName] -> Bool
iceTempleItem diff x = case diff of 
    Easy -> containsAll x [SpaceJumpBoots, MorphBall, MorphBallBomb, PlasmaBeam]
    Medium ->containsAll x [SpaceJumpBoots, MorphBall, MorphBallBomb, PlasmaBeam]
    Hard -> containsAll x [MorphBall, MorphBallBomb, PlasmaBeam]
    VeryHard -> containsAll x [MorphBall, MorphBallBomb, PlasmaBeam]
    Extreme -> containsAll x [MorphBall, MorphBallBomb, PlasmaBeam]

climbShorelines :: Difficulty -> [ItemName] -> Bool 
climbShorelines diff x = case diff of 
    Easy -> sj x
    Medium -> sj x
    Hard -> sj x
    VeryHard -> sjOrBombs x
    Extreme -> sjOrBombs x

ireSpiderTrack :: Difficulty -> [ItemName] -> Bool 
ireSpiderTrack diff x = case diff of 
    Easy -> spider x
    Medium -> spider x
    Hard -> spider x
    VeryHard -> spider x || bombs x
    Extreme -> spider x || bombs x

irwDoor :: Difficulty -> [ItemName] -> Bool
irwDoor diff x = case diff of 
    Easy -> containsAll x [SpaceJumpBoots, WaveBeam]
    Medium -> containsAll x [SpaceJumpBoots, WaveBeam]
    Hard -> containsAll x [SpaceJumpBoots, WaveBeam]
    VeryHard -> sjOrBombs x && wave x
    Extreme -> sjOrBombs x && wave x

irwItem:: Difficulty -> [ItemName] -> Bool
irwItem diff x = case diff of 
    Easy -> containsAll x [SpaceJumpBoots, Missile, PlasmaBeam]
    Medium -> containsAll x [SpaceJumpBoots, Missile, PlasmaBeam]
    Hard -> containsAll x [Missile, PlasmaBeam]
    VeryHard -> containsAll x [Missile, PlasmaBeam]
    Extreme -> containsAll x [Missile, PlasmaBeam]

ruinedCourtyardConduit :: Difficulty -> [ItemName] -> Bool
ruinedCourtyardConduit _ x = containsAll x [ChargeBeam, Missile, SuperMissile, WaveBeam]

ruinedCourtyardSave :: Difficulty -> [ItemName] -> Bool
ruinedCourtyardSave diff x = containsAll x [SpaceJumpBoots, Missile]

--Might not need bombs if using spider track, but bombs are almost always unrandomized anyway
ruinedCourtyardClimb :: Difficulty -> [ItemName] -> Bool
ruinedCourtyardClimb diff x = case diff of 
    Easy -> (spider x && sjOrBombs x) || containsAll x [SpaceJumpBoots, MorphBall, BoostBall, MorphBallBomb]
    Medium -> (spider x && sjOrBombs x) || containsAll x [SpaceJumpBoots, MorphBall, BoostBall, MorphBallBomb]
    Hard -> (spider x && sjOrBombs x) || sj x
    VeryHard -> sjOrBombs x
    Extreme -> sjOrBombs x

quarantineTunnel :: Difficulty -> [ItemName] -> Bool
quarantineTunnel _ x = containsAll x [MorphBall, WaveBeam]

climbQuarantineCaveEntrance :: Difficulty -> [ItemName] -> Bool
climbQuarantineCaveEntrance diff x = case diff of 
    Easy -> spider x
    Medium -> spider x
    Hard -> spider x || sj x 
    VeryHard -> spider x || sj x
    Extreme -> spider x || sj x

climbQuarantineCaveBack :: Difficulty -> [ItemName] -> Bool
climbQuarantineCaveBack diff x = case diff of 
    Easy -> spider x 
    Medium -> spider x || (sj x && grapple x)
    Hard -> spider x || sj x || grapple x
    VeryHard -> spider x || sj x || grapple x
    Extreme -> spider x || sj x || grapple x

quarantineMonitor :: Difficulty -> [ItemName] -> Bool
quarantineMonitor diff x = case diff of 
    Easy -> grapple x 
    Medium -> grapple x 
    Hard -> grapple x || sj x
    VeryHard -> grapple x || sj x
    Extreme -> grapple x || sj x

phenElevatorClimb :: Difficulty -> [ItemName] -> Bool
phenElevatorClimb diff x = case diff of 
    Easy -> containsAll x [MorphBall, SpiderBall, IceBeam]
    Medium -> containsAll x [MorphBall, SpiderBall, IceBeam]
    Hard -> (spider x || sj x) && ice x
    VeryHard -> (spider x || sj x || bombs x) && ice x
    Extreme -> (spider x || sj x || bombs x) && ice x

observatoryClimb :: Difficulty -> [ItemName] -> Bool
observatoryClimb diff x = case diff of 
    Easy -> containsAll x [MorphBall, BoostBall, MorphBallBomb, SpaceJumpBoots]
    Medium -> containsAll x [MorphBall, BoostBall, MorphBallBomb, SpaceJumpBoots]
    Hard -> sj x
    VeryHard -> sj x
    Extreme -> sj x

observatorySave :: Difficulty -> [ItemName] -> Bool
observatorySave diff x = case diff of 
    Easy -> sj x && contains x Missile
    Medium -> sj x && contains x Missile
    Hard -> sj x && contains x Missile
    VeryHard -> contains x Missile
    Extreme -> contains x Missile

observatoryItem :: Difficulty -> [ItemName] -> Bool
observatoryItem diff x = case diff of 
    Easy -> containsAll x [MorphBall, BoostBall, MorphBallBomb, SpaceJumpBoots]
    Medium -> sj x
    Hard -> sj x
    VeryHard -> sjOrBombs x
    Extreme -> sjOrBombs x

controlTowerItem :: Difficulty -> [ItemName] -> Bool
controlTowerItem diff x = case diff of 
    Easy -> containsAll x [MorphBall, PlasmaBeam, Missile] && sjOrBombs x
    Medium -> containsAll x [MorphBall, PlasmaBeam, Missile] && sjOrBombs x
    Hard -> (bombs x && plasma x && missile x) || (sj x && missile x && morph x)
    VeryHard -> (bombs x && plasma x && missile x) || (sj x && missile x && morph x)
    Extreme -> (bombs x && plasma x && missile x) || (sj x && missile x && morph x)

rlaTrack :: Difficulty -> [ItemName] -> Bool
rlaTrack _ x = contains x MorphBall && containsAny x [MorphBallBomb, SpaceJumpBoots]

toStorageCave :: Difficulty -> [ItemName] -> Bool
toStorageCave diff x =  case diff of 
    Easy -> containsAll x [SpaceJumpBoots, GrappleBeam, PlasmaBeam, MorphBall, PowerBomb]
    Medium -> containsAll x [SpaceJumpBoots, PlasmaBeam, MorphBall, PowerBomb]
    Hard ->containsAll x [SpaceJumpBoots, PlasmaBeam, MorphBall, PowerBomb]
    VeryHard -> containsAll x [PlasmaBeam, MorphBall, PowerBomb] && sjOrBombs x
    Extreme -> containsAll x [PlasmaBeam, MorphBall, PowerBomb] && sjOrBombs x

fromStorageCave :: Difficulty -> [ItemName] -> Bool
fromStorageCave _ x = containsAll x [PlasmaBeam, MorphBall, PowerBomb]

toSecurityCave :: Difficulty -> [ItemName] -> Bool
toSecurityCave diff x = case diff of 
    Easy -> containsAll x [SpaceJumpBoots, GrappleBeam, MorphBall]
    Medium -> containsAll x [SpaceJumpBoots, MorphBall]
    Hard -> containsAll x [SpaceJumpBoots, MorphBall]
    VeryHard -> morph x && (sjOrBombs x || grapple x)
    Extreme -> morph x && (sjOrBombs x || grapple x)

phenEdgeLower :: Difficulty -> [ItemName] -> Bool
phenEdgeLower diff x = case diff of 
    Easy -> containsAll x [WaveBeam, GravitySuit, SpaceJumpBoots]
    Medium -> containsAll x [WaveBeam, SpaceJumpBoots]
    Hard -> containsAll x [WaveBeam, SpaceJumpBoots]
    VeryHard -> wave x && sjOrBombs x
    Extreme -> wave x && sjOrBombs x

frozenPikeBottom :: Difficulty -> [ItemName] -> Bool
frozenPikeBottom diff x = case diff of 
    Easy -> containsAll x [WaveBeam, GravitySuit, SpaceJumpBoots]
    Medium -> wave x && sjOrBombs x
    Hard ->  wave x && sjOrBombs x
    VeryHard ->  wave x && sjOrBombs x
    Extreme ->  wave x && sjOrBombs x

frozenPikeClimb :: Difficulty -> [ItemName] -> Bool
frozenPikeClimb diff x = case diff of 
    Easy -> containsAll x [MorphBall,MorphBallBomb,SpaceJumpBoots]
    Medium -> containsAll x [MorphBall,MorphBallBomb,SpaceJumpBoots]
    Hard -> containsAll x [MorphBall,MorphBallBomb,SpaceJumpBoots]
    VeryHard -> sjOrBombs x
    Extreme -> sjOrBombs x

gravLedge :: Difficulty -> [ItemName] -> Bool
gravLedge diff x = case diff of 
    Easy -> containsAll x [PlasmaBeam, GrappleBeam]
    Medium -> containsAll x [PlasmaBeam, GrappleBeam]
    Hard -> containsAll x [PlasmaBeam, GrappleBeam] || sj x
    VeryHard -> containsAll x [PlasmaBeam, GrappleBeam] || sj x
    Extreme -> containsAll x [PlasmaBeam, GrappleBeam] || sj x

climbGravityChamber :: Difficulty -> [ItemName] -> Bool
climbGravityChamber diff x = case diff of 
    Easy -> contains x GravitySuit && sjOrBombs x
    Medium -> contains x GravitySuit && sjOrBombs x
    Hard -> contains x GravitySuit && sjOrBombs x
    VeryHard -> (contains x GravitySuit && sjOrBombs x) || sj x
    Extreme -> (contains x GravitySuit && sjOrBombs x) || sj x

gravityChamberToLakeTunnel :: Difficulty -> [ItemName] -> Bool
gravityChamberToLakeTunnel diff x = climbGravityChamber diff x && contains x WaveBeam

hunterCaveClimb :: Difficulty -> [ItemName] -> Bool
hunterCaveClimb diff x = case diff of 
    Easy -> contains x Missile && (contains x SpaceJumpBoots || bombs x)
    Medium -> sjOrBombs x
    Hard -> sjOrBombs x
    VeryHard -> sjOrBombs x
    Extreme -> sjOrBombs x

hunterCaveUpper :: Difficulty -> [ItemName] -> Bool
hunterCaveUpper diff x = case diff of 
    Easy -> containsAll x [Missile, GrappleBeam]
    Medium -> containsAll x [Missile, GrappleBeam]
    Hard -> sj x || containsAll x [Missile, GrappleBeam]
    VeryHard -> sj x || containsAll x [Missile, GrappleBeam]
    Extreme -> sj x || containsAll x [Missile, GrappleBeam]

hunterCaveLower :: Difficulty -> [ItemName] -> Bool
hunterCaveLower _ x = contains x Missile && (contains x SpaceJumpBoots || bombs x)

frostCaveAccess :: Difficulty -> [ItemName] -> Bool
frostCaveAccess _ x = containsAll x [MorphBall, WaveBeam]

frostCaveDoor :: Difficulty -> [ItemName] -> Bool
frostCaveDoor _ x = containsAll x [Missile,WaveBeam] && (contains x SpaceJumpBoots || bombs x)

frostCaveItem :: Difficulty -> [ItemName] -> Bool
frostCaveItem diff x = case diff of 
    Easy -> containsAll x [GrappleBeam, Missile]
    Medium -> containsAll x [GrappleBeam, Missile]
    Hard -> missile x && (sj x || grapple x)
    VeryHard -> missile x && (sj x || bombs x || grapple x)
    Extreme -> missile x && (sj x || bombs x || grapple x)

frostCaveToTunnel :: Difficulty -> [ItemName] -> Bool
frostCaveToTunnel _ x = containsAll x [Missile,WaveBeam,MorphBall] && (contains x SpaceJumpBoots || bombs x)

-- Mines Predicates
quarrySave :: Difficulty -> [ItemName] -> Bool
quarrySave diff x = case diff of 
    Easy -> containsAll x [SpiderBall, MorphBall, WaveBeam]
    Medium -> wave x && (spider x || sj x)
    Hard -> wave x && (spider x || sj x)
    VeryHard -> wave x && (spider x || sj x || bombs x)
    Extreme -> wave x && (spider x || sj x || bombs x)

quarryItem :: Difficulty -> [ItemName] -> Bool
quarryItem diff x = case diff of 
    Easy -> containsAll x [SpaceJumpBoots, WaveBeam, MorphBall, SpiderBall]
    Medium -> containsAll x [SpaceJumpBoots, WaveBeam, MorphBall, SpiderBall]
    Hard -> wave x && (spider x || sj x)
    VeryHard -> wave x && (spider x || sj x)
    Extreme -> wave x && (spider x || sj x)

reachWasteDisposal :: Difficulty -> [ItemName] -> Bool
reachWasteDisposal diff x = case diff of
    Easy -> containsAll x [SpaceJumpBoots,WaveBeam,IceBeam,GrappleBeam]
    Medium -> containsAll x [WaveBeam,IceBeam,GrappleBeam] && sjOrBombs x
    Hard -> ice x && sjOrBombs x
    VeryHard ->  ice x && sjOrBombs x
    Extreme -> ice x && sjOrBombs x

oreProcessingClimb :: Difficulty -> [ItemName] -> Bool
oreProcessingClimb diff x = case diff of
    Easy -> containsAll x [MorphBall, SpiderBall, MorphBallBomb, IceBeam]
    Medium -> containsAll x [MorphBall, SpiderBall, MorphBallBomb, IceBeam]
    Hard -> ice x && (containsAll x [MorphBall, SpiderBall, MorphBallBomb] || sj x)
    VeryHard ->  ice x && (containsAll x [MorphBall, SpiderBall, MorphBallBomb] || sj x)
    Extreme -> ice x && (containsAll x [MorphBall, SpiderBall, MorphBallBomb] || sj x)

oreProcessingTop :: Difficulty -> [ItemName] -> Bool
oreProcessingTop diff x = case diff of
    Easy -> containsAll x [MorphBall, SpiderBall, MorphBallBomb, PowerBomb, IceBeam]
    Medium -> containsAll x [MorphBall, SpiderBall, MorphBallBomb, PowerBomb, IceBeam]
    Hard -> ice x && (containsAll x [MorphBall, SpiderBall, MorphBallBomb, PowerBomb] || sj x)
    VeryHard ->  ice x && (containsAll x [MorphBall, SpiderBall, MorphBallBomb, PowerBomb] || sj x)
    Extreme -> ice x && (containsAll x [MorphBall, SpiderBall, MorphBallBomb, PowerBomb] || sj x)

wasteDisposalTraversal :: Difficulty -> [ItemName] -> Bool
wasteDisposalTraversal _ x = containsAll x [MorphBall, MorphBallBomb, IceBeam]

shaftClimb1 :: Difficulty -> [ItemName] -> Bool
shaftClimb1 diff x = case diff of
    Easy -> containsAll x [MorphBall, SpiderBall, IceBeam]
    Medium -> containsAll x [MorphBall, SpiderBall, IceBeam]
    Hard -> ice x && (spider x || (bombs x && sj x))
    VeryHard ->  ice x && (spider x || (bombs x && sj x))
    Extreme -> ice x && (spider x || (bombs x && sj x))

shaftClimb2 :: Difficulty -> [ItemName] -> Bool
shaftClimb2 diff x = case diff of
    Easy -> containsAll x [MorphBall, SpiderBall, BoostBall, IceBeam]
    Medium -> containsAll x [MorphBall, SpiderBall, BoostBall, IceBeam]
    Hard -> ice x && boost x && (spider x || sj x)
    VeryHard ->  ice x && boost x && (spider x || sj x)
    Extreme -> ice x && boost x && (spider x || sj x)

maintTunnel :: Difficulty -> [ItemName] -> Bool
maintTunnel _ x = containsAll x [MorphBall, IceBeam, PowerBomb]

ppcClimb :: Difficulty -> [ItemName] -> Bool
ppcClimb diff x = case diff of
    Easy -> containsAll x [MorphBall, SpiderBall, SpaceJumpBoots, IceBeam]
    Medium -> containsAll x [MorphBall, SpiderBall, SpaceJumpBoots, IceBeam]
    Hard -> ice x && sj x
    VeryHard -> ice x && sjOrBombs x
    Extreme -> ice x && sjOrBombs x

toMinesElevator :: Difficulty -> [ItemName] -> Bool
toMinesElevator diff x = case diff of
    Easy -> containsAll x [GrappleBeam, IceBeam]
    Medium -> ice x && (grapple x || sj x)
    Hard -> ice x 
    VeryHard -> ice x 
    Extreme -> ice x 

centralDynamoClimb :: Difficulty -> [ItemName] -> Bool
centralDynamoClimb diff x = case diff of
    Easy -> contains x IceBeam && sj x
    Medium -> contains x IceBeam && sj x
    Hard -> contains x IceBeam && sj x
    VeryHard -> contains x IceBeam && sjOrBombs x
    Extreme -> contains x IceBeam && sjOrBombs x

mqaItem :: Difficulty -> [ItemName] -> Bool
mqaItem diff x = case diff of
    Easy -> containsAll x [SpaceJumpBoots, XRayVisor, MorphBall, PowerBomb]
    Medium -> containsAll x [SpaceJumpBoots, MorphBall, PowerBomb]
    Hard -> sj x
    VeryHard -> sj x || (bombs x && pb x)
    Extreme -> sj x || (bombs x && pb x)

mqaTraversal :: Difficulty -> [ItemName] -> Bool
mqaTraversal diff x = case diff of
    Easy -> containsAll x [SpaceJumpBoots, XRayVisor, MorphBall, SpiderBall, IceBeam]
    Medium -> containsAll x [SpaceJumpBoots, MorphBall, SpiderBall, IceBeam]
    Hard -> ice x && sj x && (pb x || spider x)
    VeryHard -> ice x && sjOrBombs x && (pb x || spider x)
    Extreme -> ice x && sjOrBombs x && (pb x || spider x)

ecaItem :: Difficulty -> [ItemName] -> Bool
ecaItem diff x = case diff of
    Easy -> containsAll x [SpaceJumpBoots, MorphBall, MorphBallBomb]
    Medium -> containsAll x [SpaceJumpBoots, MorphBall, MorphBallBomb]
    Hard -> containsAll x [SpaceJumpBoots, MorphBall, MorphBallBomb]
    VeryHard -> (morph x && sj x) || bombs x
    Extreme -> (morph x && sj x) || bombs x

eliteResearchTopItem :: Difficulty -> [ItemName] -> Bool
eliteResearchTopItem diff x = case diff of
    Easy -> containsAll x [SpaceJumpBoots, MorphBall, BoostBall]
    Medium -> containsAll x [SpaceJumpBoots, MorphBall, BoostBall]
    Hard -> containsAll x [SpaceJumpBoots, MorphBall, BoostBall]
    VeryHard -> sjOrBombs x
    Extreme -> sjOrBombs x

eliteResearchDoor :: Difficulty -> [ItemName] -> Bool
eliteResearchDoor diff x = case diff of
    Easy -> containsAll x [SpaceJumpBoots, MorphBall, BoostBall, IceBeam]
    Medium -> containsAll x [SpaceJumpBoots, MorphBall, BoostBall, IceBeam]
    Hard -> containsAll x [SpaceJumpBoots, MorphBall, BoostBall, IceBeam]
    VeryHard -> sjOrBombs x && ice x
    Extreme -> sjOrBombs x && ice x

toStarageDepotA :: Difficulty -> [ItemName] -> Bool
toStarageDepotA diff x = case diff of
    Easy -> containsAll x [WaveBeam, MorphBall, PowerBomb, PlasmaBeam]
    Medium -> containsAll x [MorphBall, PowerBomb, PlasmaBeam]
    Hard -> containsAll x [MorphBall, PowerBomb, PlasmaBeam]
    VeryHard -> containsAll x [MorphBall, PowerBomb, PlasmaBeam]
    Extreme -> containsAll x [MorphBall, PowerBomb, PlasmaBeam]

climbFungalHallAccess :: Difficulty -> [ItemName] -> Bool
climbFungalHallAccess diff x = case diff of
    Easy -> containsAll x [SpaceJumpBoots, PlasmaBeam]
    Medium -> containsAll x [SpaceJumpBoots, PlasmaBeam]
    Hard -> containsAll x [SpaceJumpBoots, PlasmaBeam]
    VeryHard -> sjOrBombs x && plasma x
    Extreme -> sjOrBombs x && plasma x

fungalHallATraversal :: Difficulty -> [ItemName] -> Bool
fungalHallATraversal diff x = case diff of
    Easy -> containsAll x [SpaceJumpBoots, GrappleBeam, IceBeam]
    Medium -> containsAll x [SpaceJumpBoots, IceBeam]
    Hard -> containsAll x [SpaceJumpBoots, IceBeam]
    VeryHard -> sjOrBombs x && ice x
    Extreme -> sjOrBombs x && ice x

miningTunnelTraversal :: Difficulty -> [ItemName] -> Bool
miningTunnelTraversal _ x = containsAll x [MorphBall, MorphBallBomb, PlasmaBeam]

-- TODO Double check e-tank requirements
miningTunnelItem :: Difficulty -> [ItemName] -> Bool
miningTunnelItem diff x = case diff of
    Easy -> containsAll x [MorphBall, MorphBallBomb, PhazonSuit]
    Medium -> containsAll x [MorphBall, MorphBallBomb, PhazonSuit]
    Hard -> containsAll x [MorphBall, MorphBallBomb] && (contains x PhazonSuit || (containsCount 10 EnergyTank x && boost x))
    VeryHard -> containsAll x [MorphBall, MorphBallBomb] && (contains x PhazonSuit || (containsCount 6 EnergyTank x && boost x))
    Extreme -> containsAll x [MorphBall, MorphBallBomb] && (contains x PhazonSuit || (containsCount 6 EnergyTank x && boost x))

quarantineAccessBTraversal :: Difficulty -> [ItemName] -> Bool
quarantineAccessBTraversal diff x = case diff of
    Easy -> containsAll x [SpaceJumpBoots, PlasmaBeam]
    Medium -> plasma x
    Hard -> plasma x
    VeryHard -> plasma x
    Extreme -> plasma x

fungalHallBTraversal :: Difficulty -> [ItemName] -> Bool
fungalHallBTraversal diff x = case diff of
    Easy -> containsAll x [SpaceJumpBoots, GrappleBeam, PlasmaBeam]
    Medium -> containsAll x [SpaceJumpBoots, PlasmaBeam]
    Hard -> containsAll x [SpaceJumpBoots, PlasmaBeam]
    VeryHard -> sjOrBombs x && plasma x
    Extreme -> sjOrBombs x && plasma x

mqbTraversal :: Difficulty -> [ItemName] -> Bool
mqbTraversal diff x = case diff of
    Easy -> containsAll x [SpiderBall, MorphBall, SpaceJumpBoots, GrappleBeam]
    Medium -> containsAll x [SpiderBall, MorphBall, GrappleBeam]
    Hard -> containsAll x [SpiderBall, MorphBall, GrappleBeam] || sj x
    VeryHard -> containsAll x [SpiderBall, MorphBall, GrappleBeam] || sjOrBombs x
    Extreme -> containsAll x [SpiderBall, MorphBall, GrappleBeam] || sjOrBombs x

ppcBottomClimb :: Difficulty -> [ItemName] -> Bool
ppcBottomClimb diff x = case diff of
    Easy -> containsAll x [SpaceJumpBoots, PlasmaBeam, SpiderBall, MorphBall]
    Medium -> containsAll x [SpaceJumpBoots, PlasmaBeam, SpiderBall, MorphBall]
    Hard -> containsAll x [SpaceJumpBoots, PlasmaBeam]
    VeryHard -> sjOrBombs x && plasma x
    Extreme -> sjOrBombs x && plasma x

eliteQuarters :: Difficulty -> [ItemName] -> Bool
eliteQuarters _ x = contains x XRayVisor

eliteQuartersPlasma :: Difficulty -> [ItemName] -> Bool
eliteQuartersPlasma diff x = contains x PlasmaBeam && eliteQuarters diff x

mqbBackClimb :: Difficulty -> [ItemName] -> Bool
mqbBackClimb diff x = case diff of
    Easy -> containsAll x [SpaceJumpBoots, PlasmaBeam]
    Medium -> plasma x && sjOrBombs x
    Hard -> plasma x && sjOrBombs x
    VeryHard -> plasma x && sjOrBombs x
    Extreme -> plasma x && sjOrBombs x

-- Helper functions
containsCount :: Eq a => Int -> a -> [a] -> Bool
containsCount num element list
    | num < 0 = False
    | num == 0 = True
    | otherwise = num <= count element list

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

contains :: [ItemName] -> ItemName -> Bool
contains items item = item `Prelude.elem` items

containsAll :: [ItemName] -> [ItemName] -> Bool
containsAll [] [] = True
containsAll _ [] = True
containsAll [] _ = False 
containsAll items (x:rest) = contains items x && containsAll items rest

containsAny :: [ItemName] -> [ItemName] -> Bool
containsAny [] [] = True
containsAny _ [] = True
containsAny [] _ = False 
containsAny items (x:rest) = contains items x || containsAll items rest

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
                                    ,Edge noReq (R OTallonCanyon)]
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

            -- Chozo Ruins Rooms
            ,Room RTransporttoTallonOverworldNorth [Edge noReq (R OTransporttoChozoRuinsWest)
                                    ,Edge noReq (R RRuinsEntrance)]
            ,Room RRuinsEntrance [Edge noReq (R RTransporttoTallonOverworldNorth)
                                    ,Edge noReq (R RMainPlaza)]
            ,Room RMainPlaza [Edge noReq (R RRuinsEntrance)
                                    ,Edge morph (R RRuinedFountainAccess)
                                    ,Edge missile (R RRuinedShrineAccess)
                                    ,Edge noReq (R RNurseryAccess)
                                    ,Edge (mainPlazaGrappleLedge diff) (R RPistonTunnel)
                                    ,Edge (mainPlazaLedge diff) (R RMainPlazaLedge)
                                    ,Edge (mainPipe diff) (I MainPlazaHalfPipe)
                                    ,Edge (mainPlazaGrappleLedge diff) (I MainPlazaGrappleLedge)
                                    ,Edge supers (I MainPlazaTree)]
            --Created new room to hold the main plaza lEdge (item diff), and allow one-way traversal through Vault
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
            ,Room RRuinedFountain [Edge noReq (I RuinedFountain)] -- For simplicity, you can't escape a ruined fountain warp twice...sorry
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
                                    ,Edge (tcTunnel diff) (R RPistonTunnel)
                                    ,Edge (tcItem diff) (I TrainingChamber)]
            ,Room RPistonTunnel [Edge morph (R RMainPlaza)
                                    ,Edge blocked (R RTrainingChamber)] -- Since it is blocked initially, it's simpler to consider it one-way
            ,Room RArboretumAccess [Edge noReq (R RRuinedFountainNonWarp)
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
                                    ,Edge bombs (I GatheringHall)]
            ,Room RWateryHallAccess [Edge noReq (R RGatheringHall)
                                    ,Edge missile (R RWateryHall)
                                    ,Edge missile (I WateryHallAccess)]
            ,Room RWateryHall [Edge missile (R RWateryHallAccess)
                                    ,Edge (wateryHallTraverse diff) (R RDynamoAccess)
                                    ,Edge (wateryHallWater diff) (I WateryHallUnderwater)
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
                                    ,Edge ice (R RHalloftheElders)] -- Need to check if statue is moved?
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
            
            --Magmoor Caverns Rooms
            ,Room CTransporttoChozoRuinsNorth [Edge noReq (R RTransporttoMagmoorCavernsNorth)
                                    ,Edge noReq (R CBurningTrail)]
            ,Room CBurningTrail [Edge noReq (R CTransporttoChozoRuinsNorth)
                                    ,Edge missile (R CSaveStationMagmoorA)
                                    ,Edge noReq (R CLakeTunnel)]
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
                                    ,Edge (heatResistOr8Etanks diff) (R CMonitorTunnel) -- This has a high requirement to deter this path to get to phendrana
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
                                    ,Edge (toStarageDepotA diff) (R MStorageDepotA)
                                    ,Edge wave (R MSecurityAccessB)]
            ,Room MStorageDepotA [Edge blocked (R MMineSecurityStation) -- For simplicity, it's blocked for now.
                                    ,Edge noReq (I StorageDepotA)]
            ,Room MSecurityAccessB [Edge wave (R MMineSecurityStation)
                                    ,Edge ice (R MEliteResearch)]
            ,Room MEliteResearch [Edge ice (R MSecurityAccessB)
                                    ,Edge (eliteResearchDoor diff) (R MResearchAccess)
                                    ,Edge (eliteResearchTopItem diff) (I EliteResearchLaser)
                                    ,Edge pb (I EliteResearchPhazonElite)]
            -- Currently require boosting through wall
            ,Room MResearchAccess [Edge (shaftClimb2 diff) (R MEliteResearch)
                                    ,Edge (oreProcessingClimb diff) (R MOreProcessing)]
            ,Room MOreProcessing [Edge ice (R MResearchAccess)
                                    ,Edge ice (R MElevatorAccessA)
                                    ,Edge (oreProcessingTop diff) (R MWasteDisposal)
                                    ,Edge (oreProcessingTop diff) (R MStorageDepotB)]
            ,Room MWasteDisposal [Edge ice (R MOreProcessing)
                                    ,Edge (wasteDisposalTraversal diff) (R MMainQuarry)]
            ,Room MStorageDepotB [Edge ice (R MOreProcessing)
                                    ,Edge noReq (I StorageDepotB)]
            ,Room MElevatorAccessA [Edge ice (R MOreProcessing)
                                    ,Edge ice (R MElevatorA)]
            ,Room MElevatorA [Edge (shaftClimb1 diff) (R MElevatorAccessA)
                                    ,Edge ice (R MEliteControlAccess)]
            ,Room MEliteControlAccess [Edge ice (R MElevatorA)
                                    ,Edge wave (R MEliteControl)
                                    ,Edge (ecaItem diff) (I EliteControlAccess)]
            ,Room MEliteControl [Edge wave (R MEliteControlAccess)
                                    ,Edge ice (R MMaintenanceTunnel)
                                    ,Edge ice (R MVentilationShaft)]
            ,Room MMaintenanceTunnel [Edge ice (R MEliteControl)
                                    ,Edge (maintTunnel diff) (R MPhazonProcessingCenter)]
            ,Room MPhazonProcessingCenter [Edge (maintTunnel diff) (R MMaintenanceTunnel)
                                    ,Edge blocked (R MProcessingCenterAccess) -- Not going to deal with this door
                                    ,Edge (ppcClimb diff) (R MTransportAccess)
                                    ,Edge pb (I PhazonProcessingCenter)]
            ,Room MTransportAccess [Edge ice (R MPhazonProcessingCenter)
                                    ,Edge (toMinesElevator diff) (R MTransporttoMagmoorCavernsSouth)]
            ,Room MTransporttoMagmoorCavernsSouth [Edge (toMinesElevator diff) (R MTransportAccess)
                                    ,Edge noReq (R CTransporttoPhazonMinesWest)]
            -- Warp is at the top
            ,Room MVentilationShaft [Edge blocked (R MEliteControl) -- Again not dealing with the barrier
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
            -- Again, considering the barrier to be one-way
            ,Room MMetroidQuarantineA [Edge wave (R MQuarantineAccessA)
                                    ,Edge noReq (R MMetroidQuarantineABack)]
            ,Room MMetroidQuarantineABack [Edge blocked (R MMetroidQuarantineA)
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
                                        ,Edge (fungalHallBTraversal diff) (R MMissileStationMines)
                                        ,Edge (fungalHallBTraversal diff) (R MQuarantineAccessB)
                                        ,Edge bombs (I FungalHallB)]
            ,Room MMissileStationMines [Edge morph (R MFungalHallB)] -- You get warped out of bounds and need morph
            ,Room MQuarantineAccessB [Edge plasma (R MFungalHallB)
                                        ,Edge (quarantineAccessBTraversal diff) (R MMetroidQuarantineB)]
            -- These rooms are treated as though the barrier is one-way (Warp is on the mushroom side)
            ,Room MMetroidQuarantineB [Edge (quarantineAccessBTraversal diff) (R MQuarantineAccessB)
                                        ,Edge (mqbTraversal diff) (R MMetroidQuarantineBBack)]
            ,Room MMetroidQuarantineBBack [Edge blocked (R MMetroidQuarantineB)
                                        ,Edge plasma (R MSaveStationMinesC)
                                        ,Edge (mqbBackClimb diff) (R MEliteQuartersAccess)
                                        ,Edge supers (I MetroidQuarantineB)]
            ,Room MSaveStationMinesC [Edge plasma (R MMetroidQuarantineBBack)]
            ,Room MEliteQuartersAccess [Edge plasma (R MMetroidQuarantineBBack)
                                        ,Edge plasma (R MEliteQuarters)]
            ,Room MEliteQuarters [Edge (eliteQuartersPlasma diff) (R MEliteQuartersAccess)
                                        ,Edge (eliteQuartersPlasma diff) (R MProcessingCenterAccess)
                                        ,Edge (eliteQuarters diff) (I EliteQuarters)]
            ,Room MProcessingCenterAccess [Edge plasma (R MEliteQuarters)
                                        ,Edge (ppcBottomClimb diff) (R MPhazonProcessingCenter)
                                        ,Edge noReq (I ProcessingCenterAccess)]

            -- Pseudo-items
            ,Item FrigatePowerDoorTrigger FrigatePowerDoor OMainVentilationShaftSectionB
            ,Item MainQuarryBarrierTriggers MainQuarryBarriers MMainQuarry
            ,Item ChozoIceTempleTrigger ChozoIceTempleBarrier DChozoIceTemple
            ]