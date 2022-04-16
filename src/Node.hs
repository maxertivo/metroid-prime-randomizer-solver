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
                | DGravityChamber | DChamberAccess | DHunterCaveAccess | DQuarantineCaveBack | DGravityChamberTop | DHunterCaveFar

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

                --Possible pseudo items: Ruined Fountain Collected, Maze item, opened save room in mines (can also be barrier), opened OP backdoor in mines, Sunchamber, Chozo Ice Temple, HOTE statue
                -- Research Lab Hydra barrier, Frigate Power Door, Mine Security Station?
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
    Easy -> False
    Medium -> False
    Hard -> False
    VeryHard -> bombs x
    Extreme -> bombs x

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
    Hard -> (contains x GravitySuit && bombs x) || sj x
    VeryHard ->  bombs x || sj x -- Via out of bounds
    Extreme ->  bombs x || sj x

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
lavaLakeTraversal :: Difficulty -> [ItemName] -> Bool 
lavaLakeTraversal diff x = heatResist x && bombs x

lavaLakeItem :: Difficulty -> [ItemName] -> Bool 
lavaLakeItem diff x = containsAll x [Missile, SpaceJumpBoots]

pitTunnel :: Difficulty -> [ItemName] -> Bool 
pitTunnel diff x = heatResist x && contains x MorphBall

triclopsPitItem :: Difficulty -> [ItemName] -> Bool 
triclopsPitItem diff x = containsAll x [SpaceJumpBoots, Missile]

toTransportTunnelA :: Difficulty -> [ItemName] -> Bool 
toTransportTunnelA diff x = bombs x && heatResist x

monitorStationClimb :: Difficulty -> [ItemName] -> Bool 
monitorStationClimb diff x = heatResist x && containsAll x [SpaceJumpBoots,MorphBall,BoostBall]

crossTwinFires :: Difficulty -> [ItemName] -> Bool
crossTwinFires diff x = sjOrBombs x && contains x WaveBeam

crossNorthCoreTunnel :: Difficulty -> [ItemName] -> Bool
crossNorthCoreTunnel diff x = containsAll x [Missile, SpaceJumpBoots, WaveBeam]

workstationTunnel :: Difficulty -> [ItemName] -> Bool
workstationTunnel diff x = containsAll x [IceBeam, PowerBomb, MorphBall]

workstationItem :: Difficulty -> [ItemName] -> Bool
workstationItem diff x = containsAll x [MorphBall, WaveBeam]

workstationWaveDoor :: Difficulty -> [ItemName] -> Bool
workstationWaveDoor diff x = sjOrBombs x && contains x WaveBeam

geoCore :: Difficulty -> [ItemName] -> Bool
geoCore diff x = containsAll x [SpaceJumpBoots, GrappleBeam, SpiderBall, MorphBall, MorphBallBomb, BoostBall, IceBeam]

-- Phendrana Predicates
iceBarrier :: Difficulty -> [ItemName] -> Bool
iceBarrier diff x = containsAny x [Missile, ChargeBeam]

shorelinesTower :: Difficulty -> [ItemName] -> Bool
shorelinesTower diff x = containsAll x [MorphBall, SpiderBall, ChargeBeam, SuperMissile, Missile]

iceTempleClimb :: Difficulty -> [ItemName] -> Bool
iceTempleClimb diff x = containsAll x [SpaceJumpBoots, MorphBall, MorphBallBomb, Missile]

iceTempleItem :: Difficulty -> [ItemName] -> Bool
iceTempleItem diff x = containsAll x [SpaceJumpBoots, MorphBall, MorphBallBomb, PlasmaBeam]

irwDoor :: Difficulty -> [ItemName] -> Bool
irwDoor diff x = containsAll x [SpaceJumpBoots, WaveBeam]

irwItem:: Difficulty -> [ItemName] -> Bool
irwItem diff x = containsAll x [SpaceJumpBoots, Missile, PlasmaBeam]

ruinedCourtyardConduit :: Difficulty -> [ItemName] -> Bool
ruinedCourtyardConduit diff x = containsAll x [ChargeBeam, Missile, SuperMissile, WaveBeam]

ruinedCourtyardSave :: Difficulty -> [ItemName] -> Bool
ruinedCourtyardSave diff x = containsAll x [SpaceJumpBoots, Missile]

ruinedCourtyardClimb :: Difficulty -> [ItemName] -> Bool
ruinedCourtyardClimb diff x = spider x || containsAll x [SpaceJumpBoots, MorphBall, BoostBall, MorphBallBomb]

quarantineTunnel :: Difficulty -> [ItemName] -> Bool
quarantineTunnel diff x = containsAll x [MorphBall, WaveBeam]

phenElevatorClimb :: Difficulty -> [ItemName] -> Bool
phenElevatorClimb diff x = containsAll x [MorphBall, SpiderBall, IceBeam]

observatoryClimb :: Difficulty -> [ItemName] -> Bool
observatoryClimb diff x = containsAll x [MorphBall, BoostBall, MorphBallBomb, SpaceJumpBoots]

observatorySave :: Difficulty -> [ItemName] -> Bool
observatorySave diff x = observatoryClimb diff x && contains x Missile

controlTowerItem :: Difficulty -> [ItemName] -> Bool
controlTowerItem diff x = containsAll x [MorphBall, MorphBallBomb, PlasmaBeam, Missile]

rlaTrack :: Difficulty -> [ItemName] -> Bool
rlaTrack diff x = contains x MorphBall && containsAny x [MorphBallBomb, SpaceJumpBoots]

toStorageCave :: Difficulty -> [ItemName] -> Bool
toStorageCave diff x = containsAll x [SpaceJumpBoots, GrappleBeam, PlasmaBeam, MorphBall, PowerBomb]

fromStorageCave :: Difficulty -> [ItemName] -> Bool
fromStorageCave diff x = containsAll x [PlasmaBeam, MorphBall, PowerBomb]

toSecurityCave :: Difficulty -> [ItemName] -> Bool
toSecurityCave diff x = containsAll x [SpaceJumpBoots, GrappleBeam, MorphBall]

phenEdgeLower :: Difficulty -> [ItemName] -> Bool
phenEdgeLower diff x = containsAll x [WaveBeam, GravitySuit, SpaceJumpBoots]

frozenPikeBottom :: Difficulty -> [ItemName] -> Bool
frozenPikeBottom diff x = containsAll x [WaveBeam, GravitySuit, SpaceJumpBoots]

frozenPikeClimb :: Difficulty -> [ItemName] -> Bool
frozenPikeClimb diff x = containsAll x [MorphBall,MorphBallBomb,SpaceJumpBoots]

gravLedge :: Difficulty -> [ItemName] -> Bool
gravLedge diff x = containsAll x [PlasmaBeam, GrappleBeam]

climbGravityChamber :: Difficulty -> [ItemName] -> Bool
climbGravityChamber diff x = contains x GravitySuit && (contains x SpaceJumpBoots || bombs x)

gravityChamberToLakeTunnel :: Difficulty -> [ItemName] -> Bool
gravityChamberToLakeTunnel diff x = climbGravityChamber diff x && contains x WaveBeam

hunterCaveClimb :: Difficulty -> [ItemName] -> Bool
hunterCaveClimb diff x = contains x Missile && (contains x SpaceJumpBoots || bombs x)

hunterCaveUpper :: Difficulty -> [ItemName] -> Bool
hunterCaveUpper diff x = containsAll x [Missile, GrappleBeam]

hunterCaveLower :: Difficulty -> [ItemName] -> Bool
hunterCaveLower diff x = contains x Missile && (contains x SpaceJumpBoots || bombs x)

frostCaveAccess :: Difficulty -> [ItemName] -> Bool
frostCaveAccess diff x = containsAll x [MorphBall, WaveBeam]

frostCaveDoor :: Difficulty -> [ItemName] -> Bool
frostCaveDoor diff x = contains x Missile && (contains x SpaceJumpBoots || bombs x)

frostCaveItem :: Difficulty -> [ItemName] -> Bool
frostCaveItem diff x = containsAll x [GrappleBeam, Missile]

frostCaveToTunnel :: Difficulty -> [ItemName] -> Bool
frostCaveToTunnel diff x = containsAll x [Missile,WaveBeam,MorphBall] && (contains x SpaceJumpBoots || bombs x)

-- Mines Predicates
quarrySave :: Difficulty -> [ItemName] -> Bool
quarrySave diff x = containsAll x [SpiderBall, WaveBeam]

quarryItem :: Difficulty -> [ItemName] -> Bool
quarryItem diff x = containsAll x [SpaceJumpBoots, WaveBeam, MorphBall, SpiderBall]

oreProcessingClimb :: Difficulty -> [ItemName] -> Bool
oreProcessingClimb diff x = containsAll x [MorphBall, SpiderBall, MorphBallBomb, IceBeam]

oreProcessingTop :: Difficulty -> [ItemName] -> Bool
oreProcessingTop diff x = containsAll x [MorphBall, SpiderBall, MorphBallBomb, PowerBomb, IceBeam]

wasteDisposalTraversal :: Difficulty -> [ItemName] -> Bool
wasteDisposalTraversal diff x = containsAll x [MorphBall, MorphBallBomb, IceBeam]

shaftClimb :: Difficulty -> [ItemName] -> Bool
shaftClimb diff x = containsAll x [MorphBall, SpiderBall, IceBeam]

maintTunnel :: Difficulty -> [ItemName] -> Bool
maintTunnel diff x = containsAll x [MorphBall, IceBeam, PowerBomb]

ppcClimb :: Difficulty -> [ItemName] -> Bool
ppcClimb diff x = containsAll x [MorphBall, SpiderBall, SpaceJumpBoots, IceBeam]

toMinesElevator :: Difficulty -> [ItemName] -> Bool
toMinesElevator diff x = containsAll x [GrappleBeam, IceBeam]

centralDynamoClimb :: Difficulty -> [ItemName] -> Bool
centralDynamoClimb diff x = contains x IceBeam && sjOrBombs x

mqaItem :: Difficulty -> [ItemName] -> Bool
mqaItem diff x = containsAll x [SpaceJumpBoots, XRayVisor, MorphBall, SpiderBall]

mqaTraversal :: Difficulty -> [ItemName] -> Bool
mqaTraversal diff x = containsAll x [SpaceJumpBoots, XRayVisor, MorphBall, PowerBomb, IceBeam]

ecaItem :: Difficulty -> [ItemName] -> Bool
ecaItem diff x = containsAll x [SpaceJumpBoots, MorphBall, MorphBallBomb]

eliteResearchTopItem :: Difficulty -> [ItemName] -> Bool
eliteResearchTopItem diff x = containsAll x [SpaceJumpBoots, MorphBall, BoostBall]

eliteResearchDoor :: Difficulty -> [ItemName] -> Bool
eliteResearchDoor diff x = containsAll x [SpaceJumpBoots, MorphBall, BoostBall]

toStarageDepotA :: Difficulty -> [ItemName] -> Bool
toStarageDepotA diff x = containsAll x [WaveBeam, MorphBall, PowerBomb, PlasmaBeam]

climbFungalHallAccess :: Difficulty -> [ItemName] -> Bool
climbFungalHallAccess diff x = containsAll x [SpaceJumpBoots, PlasmaBeam]

fungalHallATraversal :: Difficulty -> [ItemName] -> Bool
fungalHallATraversal diff x = containsAll x [SpaceJumpBoots, GrappleBeam, IceBeam]

miningTunnelTraversal :: Difficulty -> [ItemName] -> Bool
miningTunnelTraversal diff x = containsAll x [MorphBall, MorphBallBomb, PlasmaBeam]

miningTunnelItem :: Difficulty -> [ItemName] -> Bool
miningTunnelItem diff x = containsAll x [MorphBall, MorphBallBomb] && (contains x PhazonSuit || containsCount 11 EnergyTank x)

quarantineAccessBTraversal :: Difficulty -> [ItemName] -> Bool
quarantineAccessBTraversal diff x = containsAll x [SpaceJumpBoots, IceBeam]

fungalHallBTraversal :: Difficulty -> [ItemName] -> Bool
fungalHallBTraversal diff x = containsAll x [SpaceJumpBoots, GrappleBeam, PlasmaBeam]

mqbTraversal :: Difficulty -> [ItemName] -> Bool
mqbTraversal diff x = containsAll x [SpiderBall, MorphBall, GrappleBeam]

mqbTraversalItem :: Difficulty -> [ItemName] -> Bool
mqbTraversalItem diff x = supers x && mqaTraversal diff x

mqbItem :: Difficulty -> [ItemName] -> Bool
mqbItem diff x = containsAll x [SuperMissile,Missile,ChargeBeam,PlasmaBeam]

ppcBottomClimb :: Difficulty -> [ItemName] -> Bool
ppcBottomClimb diff x = containsAll x [SpaceJumpBoots, PlasmaBeam, SpiderBall, MorphBall]

eliteQuarters :: Difficulty -> [ItemName] -> Bool
eliteQuarters diff x = contains x XRayVisor

eliteQuartersPlasma :: Difficulty -> [ItemName] -> Bool
eliteQuartersPlasma diff x = contains x PlasmaBeam && eliteQuarters diff x

mqbBackClimb :: Difficulty -> [ItemName] -> Bool
mqbBackClimb diff x = containsAll x [SpaceJumpBoots, PlasmaBeam]

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
                                    ,Edge (climbFrigateMvs diff) (R OMainVentilationShaftSectionC)]
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
                                    ,Edge heatResist (R CLavaLake)]
            ,Room CLavaLake [Edge noReq (R CLakeTunnel)
                                    ,Edge (lavaLakeTraversal diff) (R CPitTunnel)
                                    ,Edge (lavaLakeItem diff) (I LavaLake)]
            ,Room CPitTunnel [Edge (lavaLakeTraversal diff) (R CLavaLake)
                                    ,Edge (pitTunnel diff) (R CTriclopsPit)]
            ,Room CTriclopsPit [Edge (pitTunnel diff) (R CPitTunnel)
                                    ,Edge (pitTunnel diff) (R CStorageCavern)
                                    ,Edge heatResist (R CMonitorTunnel)
                                    ,Edge (triclopsPitItem diff) (I TriclopsPit)]
            ,Room CStorageCavern [Edge (triclopsPitItem diff) (R CTriclopsPit)
                                    ,Edge noReq (I StorageCavern)]
            ,Room CMonitorTunnel [Edge heatResist (R CTriclopsPit)
                                    ,Edge heatResist (R CMonitorStation)]
            ,Room CMonitorStation [Edge heatResist (R CMonitorTunnel)
                                    ,Edge heatResist (R CShoreTunnel)
                                    ,Edge (toTransportTunnelA diff) (R CTransportTunnelA)
                                    ,Edge (monitorStationClimb diff) (R CWarriorShrine)]
            ,Room CTransportTunnelA [Edge bombs (R CMonitorStation)
                                    ,Edge noReq (R CTransporttoPhendranaDriftsNorth)
                                    ,Edge bombs (I TransportTunnelA)]
            ,Room CTransporttoPhendranaDriftsNorth [Edge noReq (R CTransportTunnelA)
                                    ,Edge noReq (R DTransporttoMagmoorCavernsWest)]
            ,Room CWarriorShrine [Edge heatResist (R CMonitorStation)
                                    ,Edge bombsPbs (R CFieryShores)
                                    ,Edge noReq (I WarriorShrine)
                                    ,Edge pb (I FieryShoresWarriorShrineTunnel)]
            ,Room CShoreTunnel [Edge heatResist (R CMonitorStation)
                                    ,Edge heatResist (R CFieryShores)
                                    ,Edge pb (I ShoreTunnel)]
            ,Room CFieryShores [Edge heatResist (R CShoreTunnel)
                                    ,Edge heatResist (R CTransportTunnelB)
                                    ,Edge bombs (I FieryShoresMorphTrack)]
            ,Room CTransportTunnelB [Edge heatResist (R CFieryShores)
                                    ,Edge noReq (R CTransporttoTallonOverworldWest)]
            ,Room CTransporttoTallonOverworldWest [Edge noReq (R CTransportTunnelB)
                                    ,Edge noReq (R OTransporttoMagmoorCavernsEast)
                                    ,Edge spider (R CTwinFiresTunnel)]
            ,Room CTwinFiresTunnel [Edge spider (R CTransporttoTallonOverworldWest)
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
                                    ,Edge sj (R DPlazaWalkway)
                                    ,Edge sj (R DRuinsEntryway)
                                    ,Edge sj (R DTempleEntryway)
                                    ,Edge plasma (I PhendranaShorelinesBehindIce)
                                    ,Edge (shorelinesTower diff) (I PhendranaShorelinesSpiderTrack)]
            ,Room DSaveStationB [Edge noReq (R DPhendranaShorelines)]
            ,Room DTempleEntryway [Edge noReq (R DPhendranaShorelines)
                                    ,Edge (iceBarrier diff) (R DChozoIceTemple)]
            ,Room DChozoIceTemple [Edge (iceBarrier diff) (R DTempleEntryway)
                                    ,Edge (iceTempleClimb diff) (R DChapelTunnel)
                                    ,Edge (iceTempleItem diff) (I ChozoIceTemple)]
            ,Room DChapelTunnel [Edge blocked (R DChozoIceTemple)  -- Fix this later
                                    ,Edge noReq (R DChapeloftheElders)] -- Warp point is near Chapel of the Elders
            ,Room DChapeloftheElders [Edge wave (R DChapelTunnel)
                                    ,Edge missile (I ChapeloftheElders)]
            ,Room DIceRuinsAccess [Edge noReq (R DPhendranaShorelines)
                                    ,Edge (iceBarrier diff) (R DIceRuinsEast)]
            ,Room DIceRuinsEast [Edge (iceBarrier diff) (R DIceRuinsAccess)
                                    ,Edge noReq (R DPlazaWalkway)
                                    ,Edge spider (I IceRuinsEastSpiderTrack)
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
                                    ,Edge spider (R DQuarantineCaveBack)
                                    ,Edge noReq (I QuarantineCave)]
            -- Added a new "room" representing the other door in quarantine cave
            ,Room DQuarantineCaveBack [Edge grapple (R DQuarantineMonitor)
                                    ,Edge (quarantineTunnel diff) (R DSouthQuarantineTunnel)]
            ,Room DQuarantineMonitor [Edge grapple (R DQuarantineCaveBack)
                                    ,Edge spider (R DQuarantineCave)
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
                                    ,Edge (observatoryClimb diff) (R DWestTowerEntrance)
                                    ,Edge (observatorySave diff) (R DSaveStationD)
                                    ,Edge (observatoryClimb diff) (I Observatory)]
            ,Room DSaveStationD [Edge missile (R DObservatory)] -- May want to make item accessible from here
            ,Room DWestTowerEntrance [Edge wave (R DObservatory)
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
            ,Room DHunterCaveFar [Edge sj (R DHunterCave)
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
                                    ,Edge blocked (R MWasteDisposal) -- Address this later
                                    ,Edge ice (R MSecurityAccessA)
                                    ,Edge (quarryItem diff) (I MainQuarry)]
            ,Room MSaveStationMinesA [Edge noReq (R MMainQuarry)]
            ,Room MSecurityAccessA [Edge ice (R MMainQuarry)
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
            -- Not dealing with boosting through the wall for now
            ,Room MResearchAccess [Edge blocked (R MEliteResearch)
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
            ,Room MElevatorA [Edge (shaftClimb diff) (R MElevatorAccessA)
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
            ]