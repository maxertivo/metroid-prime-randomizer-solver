module Node where

data Id = R RoomId | I ItemId

data Node = Room {roomId :: RoomId, edges :: [Edge]} | Item {itemId :: ItemId, itemName :: ItemName, warp :: RoomId}

data Edge = Edge {canUse :: [ItemName] -> Bool, nodeId :: Id}

data ItemName = Missile | EnergyTank | MorphBall | SpaceJump | MorphBallBombs | GrappleBeam | WaveBeam | IceBeam | PlasmaBeam | GravitySuit | BoostBall | PowerBomb
                | SpiderBall | SuperMissile | ChargeBeam
                deriving  (Read, Eq, Show, Enum)

-- Two different types of IDs here to make it more difficult to confuse Room and Item IDs
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

                | RTransporttoTallonOverworldSouth | RTransporttoTallonOverworldEast

                | CTransporttoTallonOverworldWest 
                | MTransporttoTallonOverworldSouth | MTransporttoChozoRuinsNorth
                deriving  (Read, Eq, Show, Enum)
data ItemId = LandingSite | RootCave | ArborChamber | TransportTunnelB | FrigateCrashSite | OvergrownCavern | CargoFreightLifttoDeckGamma 
                | BiohazardContainment | HydroAccessTunnel | GreatTreeChamber | LifeGroveTunnel | LifeGroveStart | LifeGroveUnderwaterSpinner
                | ArtifactTemple | MainPlazaLockedDoor | MainPlazaTree | MainPlazaGrappleLedge | MainPlazaHalfPipe | Vault | TransportAccessNorth
                | HiveTotem | RuinedGalleryMissileWall | RuinedGalleryTunnel | RuinedNursery
                deriving  (Read, Eq, Show, Enum)

noReq :: [ItemName] -> Bool
noReq _ = True 

morph :: [ItemName] -> Bool
morph x = contains x MorphBall

sj :: [ItemName] -> Bool
sj x = contains x SpaceJump

missile :: [ItemName] -> Bool
missile x = contains x Missile

bombs :: [ItemName] -> Bool
bombs x = contains x MorphBallBombs

arbor :: [ItemName] -> Bool
arbor x = containsAll x [SpaceJump, GrappleBeam, PlasmaBeam]

spaceGrapple :: [ItemName] -> Bool
spaceGrapple x = containsAll x [SpaceJump, GrappleBeam]

sjGrapple :: [ItemName] -> Bool
sjGrapple x = containsAll x [SpaceJump, GrappleBeam]

mainPipe :: [ItemName] -> Bool
mainPipe x = contains x SpaceJump || containsAll x [BoostBall, MorphBall]

morphMissile :: [ItemName] -> Bool
morphMissile x = containsAll x [MorphBall, Missile]

fcsClimb :: [ItemName] -> Bool
fcsClimb _ = False

frigatePowerDoor :: [ItemName] -> Bool
frigatePowerDoor _ = False

fcsEntry :: [ItemName] -> Bool
fcsEntry x = contains x IceBeam && (contains x GrappleBeam || (contains x MorphBall && fcsItem x ))

fcsItem :: [ItemName] -> Bool
fcsItem x = contains x SpaceJump || contains x GravitySuit

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
gravSpace x = containsAll x [GravitySuit, SpaceJump]

boostSpace :: [ItemName] -> Bool
boostSpace x = containsAll x [MorphBall, BoostBall, SpaceJump]

hydroTunnel ::  [ItemName] -> Bool
hydroTunnel x = containsAll x [GravitySuit, MorphBall, MorphBallBombs]

gthClimb :: [ItemName] -> Bool
gthClimb x = containsAll x [SpaceJump, BoostBall, MorphBall]

bars :: [ItemName] -> Bool
bars _ = False

blocked :: [ItemName] -> Bool
blocked _ = False

lifeGroveT :: [ItemName] -> Bool
lifeGroveT x = containsAll x [PowerBomb, MorphBall, BoostBall]

contains :: [ItemName] -> ItemName -> Bool
contains items item = item `elem` items

containsAll :: [ItemName] -> [ItemName] -> Bool
containsAll [] [] = True
containsAll items [] = True
containsAll [] checks = False 
containsAll items (x:rest) = contains items x && containsAll items rest

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
                                    ,Edge noReq (R MTransporttoChozoRuinsNorth)
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
                                    ]