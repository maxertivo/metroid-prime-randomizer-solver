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
                | RArboretumAccess | RSunchamberLobby | RSunchamberAccess | RSunchamber | RSunTowerAccess | RSunTower
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
                | DGravityChamber | DChamberAccess | DHunterCaveAccess

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

                --Possible pseudo items: Ruined Fountain Collected, Maze item, opened save room in mines, opened OP backdoor in mines
                deriving  (Read, Eq, Show, Enum)

noReq :: [ItemName] -> Bool
noReq _ = True 

morph :: [ItemName] -> Bool
morph x = contains x MorphBall

sj :: [ItemName] -> Bool
sj x = contains x SpaceJumpBoots

missile :: [ItemName] -> Bool
missile x = contains x Missile

bombs :: [ItemName] -> Bool
bombs x = contains x MorphBallBomb

arbor :: [ItemName] -> Bool
arbor x = containsAll x [SpaceJumpBoots, GrappleBeam, PlasmaBeam]

spaceGrapple :: [ItemName] -> Bool
spaceGrapple x = containsAll x [SpaceJumpBoots, GrappleBeam]

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

spiderIce :: [ItemName] -> Bool
spiderIce x = containsAll x [IceBeam, SpiderBall]

supers :: [ItemName] -> Bool
supers x = containsAll x [Missile, SuperMissile, ChargeBeam]

frigateRoom :: [ItemName] -> Bool
frigateRoom x = containsAll x [WaveBeam, GravitySuit]

gravSpace :: [ItemName] -> Bool
gravSpace x = containsAll x [GravitySuit, SpaceJumpBoots]

boostSpace :: [ItemName] -> Bool
boostSpace x = containsAll x [MorphBall, BoostBall, SpaceJumpBoots]

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

spider :: [ItemName] -> Bool
spider x = contains x SpiderBall

heatResist :: [ItemName] -> Bool
heatResist x  = containsAny x [VariaSuit, GravitySuit, PhazonSuit]

crossMagmaPool :: [ItemName] -> Bool
crossMagmaPool x  = heatResist x && containsAll x [GrappleBeam,WaveBeam]

magmaPoolItem :: [ItemName] -> Bool
magmaPoolItem x  = heatResist x && containsAll x [GrappleBeam,PowerBomb]

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
                                    ,Edge spaceGrapple (I RootCave)]
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
            --Not sure about warp point
            ,Room OMainVentilationShaftSectionC [Edge sj (R OFrigateAccessTunnel)
                                    ,Edge noReq (R OMainVentilationShaftSectionB)]
            ,Room OMainVentilationShaftSectionB [Edge wave (R OMainVentilationShaftSectionA)
                                    ,Edge noReq (R OMainVentilationShaftSectionC)]
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
            --Not sure about warp point
            ,Room OHydroAccessTunnel [Edge hydroTunnel (R OConnectionElevatortoDeckBeta)
                                    ,Edge hydroTunnel (R OGreatTreeHall)
                                    ,Edge morph (I HydroAccessTunnel)]
            --Not sure about warp point
            --Great Tree Hall is split into two rooms, an upper and lower section
            ,Room OGreatTreeHall [Edge noReq (R OHydroAccessTunnel)
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
            -- Assuming you get warped to top, you can leave
            ,Room OLifeGrove [Edge morph (R OLifeGroveTunnel)
                                    ,Edge noReq (I LifeGroveStart)
                                    ,Edge boostSpace (I LifeGroveUnderwaterSpinner)]
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

            -- Chozo Ruins
            ,Room RTransporttoTallonOverworldNorth [Edge noReq (R OTransporttoChozoRuinsWest)
                                    ,Edge noReq (R RRuinsEntrance)]
            ,Room RRuinsEntrance [Edge noReq (R RTransporttoTallonOverworldNorth)
                                    ,Edge noReq (R RMainPlaza)]
            ,Room RMainPlaza [Edge noReq (R RRuinsEntrance)
                                    ,Edge blocked (R RPlazaAccess)
                                    ,Edge noReq (R RRuinedFountainAccess)
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
                                    ,Edge noReq (R RSunTower)
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
            -- Not sure about warp point
            ,Room RRuinedFountainAccess [Edge morph (R RRuinedFountainNonWarp)
                                    ,Edge noReq (R RMainPlaza)]
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
                                    ]