module Predicates where

import Node
import Util

import Data.Map (Map)
import Data.Set (Set, member)

-- Basic Predicates
noReq :: Map ItemName Int -> Set ItemId -> Bool
noReq _ _ = True

blocked :: Map ItemName Int -> Set ItemId -> Bool
blocked _ _ = False

complete :: Map ItemName Int -> Set ItemId -> Bool
complete inv _ = containsCount 12 Artifact inv && contains inv PhazonSuit && containsAll inv [WaveBeam, IceBeam, PlasmaBeam]

morph :: Map ItemName Int -> Set ItemId -> Bool
morph inv _ = contains inv MorphBall

sj :: Map ItemName Int -> Set ItemId -> Bool
sj inv _ = contains inv SpaceJumpBoots

sjOrBombs :: Map ItemName Int -> Set ItemId -> Bool
sjOrBombs inv ids = contains inv SpaceJumpBoots || bombs inv ids

missile :: Map ItemName Int -> Set ItemId -> Bool
missile inv _ = contains inv Missile

bombs :: Map ItemName Int -> Set ItemId -> Bool
bombs inv _ = containsAll inv [MorphBall, MorphBallBomb]

pb :: Map ItemName Int -> Set ItemId -> Bool
pb inv _ = containsAll inv [PowerBomb, MorphBall]

boost :: Map ItemName Int -> Set ItemId -> Bool
boost inv _ = containsAll inv [MorphBall, BoostBall]

spider :: Map ItemName Int -> Set ItemId -> Bool
spider inv _ = containsAll inv [MorphBall, SpiderBall]

waveIce :: Map ItemName Int -> Set ItemId -> Bool
waveIce inv _ = containsAll inv [WaveBeam, IceBeam]

grapple :: Map ItemName Int -> Set ItemId -> Bool
grapple inv _ = contains inv GrappleBeam

boostBombs :: Map ItemName Int -> Set ItemId -> Bool
boostBombs inv _ = containsAll inv [MorphBall, BoostBall, MorphBallBomb]

morphMissile :: Map ItemName Int -> Set ItemId -> Bool
morphMissile inv _ = containsAll inv [MorphBall, Missile]

bombsPbs :: Map ItemName Int -> Set ItemId -> Bool
bombsPbs inv _ = containsAll inv [MorphBall, MorphBallBomb, PowerBomb]

wave :: Map ItemName Int -> Set ItemId -> Bool
wave inv _ = contains inv WaveBeam

ice :: Map ItemName Int -> Set ItemId -> Bool
ice inv _ = contains inv IceBeam

plasma :: Map ItemName Int -> Set ItemId -> Bool
plasma inv _ = contains inv PlasmaBeam

supers :: Map ItemName Int -> Set ItemId -> Bool
supers inv _ = containsAll inv [Missile, SuperMissile, ChargeBeam]

gravSpace :: Map ItemName Int -> Set ItemId -> Bool
gravSpace inv _ = containsAll inv [GravitySuit, SpaceJumpBoots]

wavePb :: Map ItemName Int -> Set ItemId -> Bool
wavePb inv ids = wave inv ids && pb inv ids

heatResist :: Map ItemName Int -> Set ItemId -> Bool
heatResist inv _ = containsAny inv [VariaSuit, GravitySuit, PhazonSuit]

floaty :: Map ItemName Int -> Set ItemId -> Bool
floaty inv _ = not $ contains inv GravitySuit

-- A bit of an obscure trick, you can use infinite speed in landing site to unload the room
tallonFloaty :: Map ItemName Int -> Set ItemId -> Bool
tallonFloaty inv ids = boost inv ids && bombs inv ids && floaty inv ids

noVines :: Map ItemName Int -> Set ItemId -> Bool
noVines _ ids = not (member SunchamberFlaahgra ids) || member SunchamberGhosts ids

mainQuarryBarrierIce :: Map ItemName Int -> Set ItemId -> Bool
mainQuarryBarrierIce inv _ = containsAll inv [MainQuarryBarriers, IceBeam]

mainQuarryBarrierWave :: Map ItemName Int -> Set ItemId -> Bool
mainQuarryBarrierWave inv _ = containsAll inv [MainQuarryBarriers, WaveBeam]

chozoIceTempleBarrier :: Map ItemName Int -> Set ItemId -> Bool
chozoIceTempleBarrier inv _ = contains inv ChozoIceTempleBarrier

researchLabHydraBarrier :: Map ItemName Int -> Set ItemId -> Bool
researchLabHydraBarrier inv _ = contains inv ResearchLabHydraBarrier

eliteControlBarrier :: Map ItemName Int -> Set ItemId -> Bool
eliteControlBarrier inv _ = contains inv EliteControlBarrier

mqaBarrier :: Map ItemName Int -> Set ItemId -> Bool 
mqaBarrier inv _ = contains inv MetroidQuarantineABarrier

mqbBarrier :: Map ItemName Int -> Set ItemId -> Bool
mqbBarrier inv _ = contains inv MetroidQuarantineBBarrier

wallcrawl :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
wallcrawl diff inv ids =
    case diff of
        Easy -> False
        Medium -> False
        Hard -> bombs inv ids
        VeryHard -> bombs inv ids
        Expert -> bombs inv ids

longWallcrawl :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
longWallcrawl diff inv ids = bombs inv ids && diff == Expert

-- Tallon Predicates
sjf :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
sjf diff inv ids =
    case diff of
        Easy -> sj inv ids
        Medium -> sj inv ids
        Hard -> True
        VeryHard -> True
        Expert -> True

tallonCanyonSw :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
tallonCanyonSw diff inv ids =
    case diff of
        Easy -> False
        Medium -> False
        Hard -> False
        VeryHard -> boost inv ids && bombs inv ids
        Expert -> bombs inv ids

rootCaveItem :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
rootCaveItem diff inv _ =
    case diff of
        Easy -> containsAll inv [SpaceJumpBoots, GrappleBeam, XRayVisor]
        Medium -> containsAll inv [SpaceJumpBoots, GrappleBeam]
        Hard -> contains inv SpaceJumpBoots
        VeryHard -> contains inv SpaceJumpBoots
        Expert -> contains inv SpaceJumpBoots

arbor :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
arbor diff inv _ =
    case diff of
        Easy -> containsAll inv [SpaceJumpBoots, GrappleBeam, XRayVisor, PlasmaBeam]
        Medium -> containsAll inv [SpaceJumpBoots, GrappleBeam, PlasmaBeam]
        Hard -> containsAll inv [SpaceJumpBoots] -- You can wallcrawl around the plasma door
        VeryHard -> containsAll inv [SpaceJumpBoots]
        Expert -> containsAll inv [SpaceJumpBoots]

fcsClimb :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
fcsClimb diff inv ids =
    case diff of
        Easy -> False
        Medium -> contains inv SpaceJumpBoots && contains inv IceBeam
        Hard -> contains inv SpaceJumpBoots && contains inv IceBeam
        VeryHard -> sjOrBombs inv ids && contains inv IceBeam
        Expert -> sjOrBombs inv ids && contains inv IceBeam

frigatePowerDoor :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
frigatePowerDoor diff inv ids =
    case diff of
        Easy -> contains inv FrigatePowerDoor
        Medium -> contains inv FrigatePowerDoor
        Hard -> contains inv FrigatePowerDoor
        VeryHard -> bombs inv ids || contains inv FrigatePowerDoor
        Expert -> bombs inv ids || contains inv FrigatePowerDoor

fcsEntry :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
fcsEntry diff inv ids =
    case diff of
        Easy -> contains inv IceBeam && (contains inv GrappleBeam || (contains inv MorphBall && (contains inv SpaceJumpBoots || contains inv GravitySuit)))
        Medium -> contains inv IceBeam && (contains inv GrappleBeam || contains inv MorphBall)
        Hard -> contains inv IceBeam && (contains inv GrappleBeam || contains inv MorphBall || contains inv SpaceJumpBoots)
        VeryHard -> contains inv IceBeam && (contains inv GrappleBeam || contains inv MorphBall || contains inv SpaceJumpBoots)
        Expert -> (contains inv IceBeam && (contains inv GrappleBeam || contains inv MorphBall || contains inv SpaceJumpBoots)) || (contains inv SpaceJumpBoots && tallonFloaty inv ids)

fcsItem :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
fcsItem diff inv ids =
    case diff of
        Easy -> sj inv ids && contains inv GravitySuit
        Medium -> sjOrBombs inv ids && contains inv GravitySuit
        Hard -> True
        VeryHard -> True
        Expert -> True

climbFrigateMvs :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
climbFrigateMvs diff inv ids =
    case diff of
        Easy -> sj inv ids
        Medium -> sj inv ids
        Hard -> sjOrBombs inv ids
        VeryHard -> sjOrBombs inv ids
        Expert -> sjOrBombs inv ids

climbReactorCore :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
climbReactorCore diff inv ids =
    case diff of
        Easy -> sj inv ids
        Medium -> sj inv ids
        Hard -> sj inv ids || containsAll inv [GravitySuit, MorphBall, MorphBallBomb]
        VeryHard -> sj inv ids || containsAll inv [GravitySuit, MorphBall, MorphBallBomb]
        Expert -> sj inv ids || containsAll inv [GravitySuit, MorphBall, MorphBallBomb]

cargoFreightLift :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
cargoFreightLift diff inv ids =
    case diff of
        Easy -> containsAll inv [WaveBeam, GravitySuit, SpaceJumpBoots]
        Medium -> contains inv WaveBeam && ((bombs inv ids && not (contains inv GravitySuit)) || containsAll inv [GravitySuit, SpaceJumpBoots])
        Hard -> contains inv WaveBeam && ((bombs inv ids && not (contains inv GravitySuit)) || containsAll inv [GravitySuit, SpaceJumpBoots])
        VeryHard -> contains inv WaveBeam && (bombs inv ids || containsAll inv [GravitySuit, SpaceJumpBoots])
        Expert -> contains inv WaveBeam && (bombs inv ids || containsAll inv [GravitySuit, SpaceJumpBoots])

biohazard :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
biohazard _ inv _ = contains inv WaveBeam

climbBiohazard :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
climbBiohazard diff inv ids =
    case diff of
        Easy -> containsAll inv [GravitySuit, SpaceJumpBoots]
        Medium -> sj inv ids || (contains inv GravitySuit && bombs inv ids)
        Hard -> sj inv ids || contains inv GravitySuit || bombs inv ids
        VeryHard -> sj inv ids || contains inv GravitySuit || bombs inv ids
        Expert -> sj inv ids || contains inv GravitySuit || bombs inv ids

biotech :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
biotech diff inv ids =
    case diff of
        Easy -> containsAll inv [GravitySuit, SpaceJumpBoots, WaveBeam]
        Medium -> contains inv WaveBeam && (sj inv ids || (contains inv GravitySuit && bombs inv ids))
        Hard -> contains inv WaveBeam
        VeryHard -> contains inv WaveBeam
        Expert -> contains inv WaveBeam

biotechReverse :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
biotechReverse diff inv ids =
    case diff of
        Easy -> containsAll inv [GravitySuit, SpaceJumpBoots]
        Medium -> sj inv ids || (contains inv GravitySuit && bombs inv ids)
        Hard -> sjOrBombs inv ids
        VeryHard -> sjOrBombs inv ids -- You probably don't need bombs but you will likely have them anyway
        Expert -> sjOrBombs inv ids

lgUnderWater :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
lgUnderWater diff inv ids =
    case diff of
        Easy -> containsAll inv [MorphBall, MorphBallBomb, BoostBall, PowerBomb, SpaceJumpBoots]
        Medium -> containsAll inv [MorphBall, MorphBallBomb, BoostBall, PowerBomb]
        Hard -> containsAll inv [MorphBall, MorphBallBomb, BoostBall, PowerBomb]
        VeryHard -> pb inv ids && sjOrBombs inv ids
        Expert -> pb inv ids && sjOrBombs inv ids

hydroTunnel :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
hydroTunnel diff inv ids =
    case diff of
        Easy -> containsAll inv [GravitySuit, MorphBall, MorphBallBomb]
        Medium -> containsAll inv [GravitySuit, MorphBall, MorphBallBomb]
        Hard -> containsAll inv [GravitySuit, MorphBall, MorphBallBomb] || boost inv ids
        VeryHard -> containsAll inv [GravitySuit, MorphBall, MorphBallBomb] || boost inv ids
        Expert -> containsAll inv [GravitySuit, MorphBall, MorphBallBomb] || boost inv ids

gthClimb :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
gthClimb diff inv ids =
    case diff of
        Easy -> containsAll inv [SpaceJumpBoots, BoostBall, MorphBall]
        Medium -> containsAll inv [SpaceJumpBoots, BoostBall, MorphBall]
        Hard -> containsAll inv [SpaceJumpBoots, MorphBall] && containsAny inv [BoostBall, MorphBallBomb]
        VeryHard -> containsAll inv [SpaceJumpBoots, MorphBall] && containsAny inv [BoostBall, MorphBallBomb]
        Expert -> bombs inv ids

bars :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
bars diff inv ids =
    case diff of
        Easy -> False
        Medium -> False
        Hard -> bombs inv ids
        VeryHard -> bombs inv ids
        Expert -> bombs inv ids

lifeGroveTunnel :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
lifeGroveTunnel diff inv _ =
    case diff of
        Easy -> containsAll inv [PowerBomb, MorphBall, BoostBall]
        Medium -> containsAll inv [PowerBomb, MorphBall, BoostBall]
        Hard -> containsAll inv [PowerBomb, MorphBall, BoostBall]
        VeryHard -> containsAll inv [PowerBomb, MorphBall] && containsAny inv [MorphBallBomb, BoostBall]
        Expert -> containsAll inv [PowerBomb, MorphBall] && containsAny inv [MorphBallBomb, BoostBall]

lifeGroveTunnelItem :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
lifeGroveTunnelItem diff inv _ =
    case diff of
        Easy -> containsAll inv [PowerBomb, MorphBall, BoostBall, MorphBallBomb]
        Medium -> containsAll inv [PowerBomb, MorphBall, BoostBall, MorphBallBomb]
        Hard -> containsAll inv [PowerBomb, MorphBall, BoostBall, MorphBallBomb]
        VeryHard -> containsAll inv [PowerBomb, MorphBall, MorphBallBomb]
        Expert -> containsAll inv [PowerBomb, MorphBall, MorphBallBomb]

lifeGroveSw :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
lifeGroveSw diff inv ids =
    case diff of
        Easy -> False
        Medium -> False
        Hard -> pb inv ids && sj inv ids
        VeryHard -> pb inv ids && sj inv ids
        Expert -> pb inv ids && sj inv ids

gthSpiderTrack :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
gthSpiderTrack diff inv ids =
    case diff of
        Easy -> containsAll inv [SpiderBall, MorphBall, SpaceJumpBoots, IceBeam]
        Medium -> containsAll inv [SpiderBall, MorphBall, SpaceJumpBoots, IceBeam]
        Hard -> ((spider inv ids && bombs inv ids) || sj inv ids) && ice inv ids
        VeryHard -> ((spider inv ids && bombs inv ids) || sj inv ids) && ice inv ids
        Expert -> sjOrBombs inv ids && ice inv ids

gtcEnter :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
gtcEnter diff inv ids =
    case diff of
        Easy -> sj inv ids
        Medium -> sj inv ids
        Hard -> sj inv ids
        VeryHard -> sjOrBombs inv ids
        Expert -> sjOrBombs inv ids

gtcSw :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
gtcSw diff inv ids =
    case diff of
        Easy -> False
        Medium -> False
        Hard -> False
        VeryHard -> bombs inv ids
        Expert -> bombs inv ids

-- You can get past Connection Elevator to Deck Beta without gravity by wallcrawling
wallcrawlIntoFrigate :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
wallcrawlIntoFrigate diff inv ids =
    case diff of
        Easy -> False
        Medium -> False
        Hard -> False
        VeryHard -> bombs inv ids
        Expert -> bombs inv ids

-- Chozo Predicates
mainPipe :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
mainPipe diff inv ids =
    case diff of
        Easy -> sj inv ids || boost inv ids
        Medium -> sj inv ids || boost inv ids
        Hard -> sj inv ids || boost inv ids
        VeryHard -> sj inv ids || boost inv ids || bombs inv ids
        Expert -> sj inv ids || boost inv ids || bombs inv ids

mainPlazaGrappleLedge :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
mainPlazaGrappleLedge diff inv _ =
    case diff of
        Easy -> contains inv GrappleBeam
        Medium -> containsAny inv [GrappleBeam, SpaceJumpBoots]
        Hard -> containsAny inv [GrappleBeam, SpaceJumpBoots]
        VeryHard -> containsAny inv [GrappleBeam, SpaceJumpBoots]
        Expert -> containsAny inv [GrappleBeam, SpaceJumpBoots]

mainPlazaLedge :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
mainPlazaLedge diff inv _ =
    case diff of
        Easy -> False
        Medium -> contains inv SpaceJumpBoots
        Hard -> contains inv SpaceJumpBoots
        VeryHard -> contains inv SpaceJumpBoots
        Expert -> contains inv SpaceJumpBoots

mainPlazaSw :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
mainPlazaSw diff inv ids =
    case diff of
        Easy -> False
        Medium -> False
        Hard -> sj inv ids && bombs inv ids
        VeryHard -> sj inv ids && bombs inv ids
        Expert -> sj inv ids && bombs inv ids

ruinedFountainItem :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
ruinedFountainItem diff inv ids =
    case diff of
        Easy -> Data.Set.member SunchamberFlaahgra ids && contains inv SpiderBall
        Medium -> Data.Set.member SunchamberFlaahgra ids && contains inv SpiderBall
        Hard -> (Data.Set.member SunchamberFlaahgra ids || sj inv ids) && contains inv SpiderBall
        VeryHard -> (Data.Set.member SunchamberFlaahgra ids || sj inv ids) && contains inv SpiderBall
        Expert -> (Data.Set.member SunchamberFlaahgra ids || sj inv ids) && contains inv SpiderBall

leaveRuinedFountainItem :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
leaveRuinedFountainItem _ inv ids = Data.Set.member RuinedFountain ids && spider inv ids

towerChamber :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
towerChamber diff inv _ =
    case diff of
        Easy -> containsAll inv [GravitySuit, SpaceJumpBoots, WaveBeam]
        Medium -> containsAll inv [SpaceJumpBoots, WaveBeam]
        Hard -> contains inv WaveBeam
        VeryHard -> contains inv WaveBeam
        Expert -> contains inv WaveBeam

rsHalf :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
rsHalf diff inv ids =
    case diff of
        Easy -> boost inv ids
        Medium -> boost inv ids || containsAll inv [SpaceJumpBoots, MorphBall]
        Hard -> boost inv ids || containsAll inv [SpaceJumpBoots, MorphBall]
        VeryHard -> contains inv MorphBall
        Expert -> contains inv MorphBall

tolAccess :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
tolAccess diff inv _ =
    case diff of
        Easy -> containsAll inv [MorphBall, BoostBall, SpiderBall, WaveBeam]
        Medium -> containsAll inv [MorphBall, BoostBall, SpiderBall, WaveBeam] || containsAll inv [SpaceJumpBoots, WaveBeam]
        Hard -> containsAll inv [MorphBall, BoostBall, SpiderBall, WaveBeam] || containsAll inv [SpaceJumpBoots, WaveBeam]
        VeryHard -> contains inv WaveBeam
        Expert -> contains inv WaveBeam

towerOfLight :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
towerOfLight diff inv ids =
    case diff of
        Easy -> containsCount 8 Missile inv && sj inv ids
        Medium -> containsCount 8 Missile inv && sj inv ids
        Hard -> sj inv ids
        VeryHard -> (containsCount 8 Missile inv && bombs inv ids) || sj inv ids
        Expert -> (containsCount 8 Missile inv && bombs inv ids) || sj inv ids

crossMagmaPool :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
crossMagmaPool diff inv ids =
    case diff of
        Easy -> heatResist inv ids && containsAll inv [GrappleBeam, WaveBeam]
        Medium -> heatResist inv ids && containsAll inv [GrappleBeam, WaveBeam]
        Hard -> ((heatResist inv ids && contains inv GrappleBeam) || sj inv ids) && wave inv ids
        VeryHard -> ((heatResist inv ids && contains inv GrappleBeam) || sj inv ids) && wave inv ids
        Expert -> ((heatResist inv ids && contains inv GrappleBeam) || sj inv ids) && wave inv ids

magmaPoolItem :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
magmaPoolItem diff inv ids =
    case diff of
        Easy -> heatResist inv ids && containsAll inv [GrappleBeam, MorphBall, PowerBomb]
        Medium -> heatResist inv ids && containsAll inv [GrappleBeam, MorphBall, PowerBomb]
        Hard -> heatResist inv ids && ((containsAny inv [GrappleBeam, SpaceJumpBoots] && pb inv ids) || boost inv ids)
        VeryHard ->
            (grapple inv ids && pb inv ids && heatResist inv ids) ||
            (sj inv ids && pb inv ids && (contains inv EnergyTank || heatResist inv ids)) || (boost inv ids && (containsCount 5 EnergyTank inv || heatResist inv ids))
        Expert -> (grapple inv ids && pb inv ids && heatResist inv ids) || (sj inv ids && pb inv ids) || (boost inv ids && (containsCount 5 EnergyTank inv || heatResist inv ids))

tcItem :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
tcItem _ inv _ = containsAll inv [MorphBall, BoostBall, MorphBallBomb, SpiderBall]

tcTunnel :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
tcTunnel _ inv ids = boost inv ids && bombs inv ids

climbSunTower :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
climbSunTower diff inv _ = 
    case diff of
        Easy -> containsAll inv [MorphBall, SpiderBall, MorphBallBomb, Missile, SuperMissile, ChargeBeam]
        Medium -> containsAll inv [MorphBall, SpiderBall, MorphBallBomb, Missile, SuperMissile, ChargeBeam]
        Hard -> containsAll inv [MorphBall, SpiderBall, MorphBallBomb, Missile, SuperMissile, ChargeBeam]
        VeryHard -> containsAll inv [MorphBall, SpiderBall, MorphBallBomb, Missile, SuperMissile, ChargeBeam]
        Expert -> containsAll inv [MorphBall, SpiderBall, MorphBallBomb] -- Sesshoumaru bomb jump

sunchamberghost :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
sunchamberghost _ _ = Data.Set.member SunchamberFlaahgra

gatheringHallSw :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
gatheringHallSw diff inv ids =
    case diff of
        Easy -> False
        Medium -> False
        Hard -> sj inv ids && bombs inv ids
        VeryHard -> bombs inv ids
        Expert -> bombs inv ids

wateryHallSw :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
wateryHallSw diff inv ids =
    case diff of
        Easy -> False
        Medium -> False
        Hard -> sj inv ids && bombs inv ids
        VeryHard -> bombs inv ids
        Expert -> bombs inv ids

wateryHallTraverse :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
wateryHallTraverse _ inv _ = containsAll inv [MorphBall, MorphBallBomb, Missile]

wateryHallWater :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
wateryHallWater diff inv ids =
    case diff of
        Easy -> contains inv GravitySuit && (contains inv SpaceJumpBoots || bombs inv ids)
        Medium -> contains inv GravitySuit && (contains inv SpaceJumpBoots || bombs inv ids)
        Hard -> True
        VeryHard -> True
        Expert -> True

furnaceTraverse :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
furnaceTraverse diff inv _ =
    case diff of
        Easy -> containsAll inv [MorphBall, MorphBallBomb, SpiderBall]
        Medium -> containsAll inv [MorphBall, MorphBallBomb]
        Hard -> containsAll inv [MorphBall, MorphBallBomb]
        VeryHard -> containsAll inv [MorphBall, MorphBallBomb]
        Expert -> containsAll inv [MorphBall, MorphBallBomb]

furnaceItem :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
furnaceItem diff inv ids =
    case diff of
        Easy -> containsAll inv [MorphBall, MorphBallBomb, SpiderBall, PowerBomb, BoostBall]
        Medium -> containsAll inv [MorphBall, MorphBallBomb, SpiderBall, PowerBomb, BoostBall]
        Hard -> containsAll inv [MorphBall, MorphBallBomb, SpiderBall] && (containsAll inv [PowerBomb, BoostBall] || sj inv ids)
        VeryHard -> containsAll inv [MorphBall, MorphBallBomb, SpiderBall] && (containsAll inv [PowerBomb, BoostBall] || sj inv ids)
        Expert -> containsAll inv [MorphBall, MorphBallBomb, SpiderBall] && (containsAll inv [PowerBomb, BoostBall] || sj inv ids)

crosswayInfiniteSpeed :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
crosswayInfiniteSpeed diff inv _ = diff == Expert && containsAll inv [MorphBall, BoostBall, Missile]

crosswayTraverse :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
crosswayTraverse diff inv ids =
    case diff of
        Easy -> containsAll inv [MorphBall, BoostBall, Missile]
        Medium -> contains inv Missile && (boost inv ids || sj inv ids)
        Hard -> contains inv Missile && (boost inv ids || sj inv ids)
        VeryHard -> contains inv Missile && (boost inv ids || sj inv ids || bombs inv ids)
        Expert -> contains inv Missile && (boost inv ids || sj inv ids || bombs inv ids)

crosswayItem :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
crosswayItem diff inv ids =
    case diff of
        Easy -> containsAll inv [MorphBall, BoostBall, Missile, SuperMissile, ChargeBeam, SpiderBall, MorphBallBomb]
        Medium -> containsAll inv [MorphBall, BoostBall, Missile, SuperMissile, ChargeBeam, SpiderBall, MorphBallBomb]
        Hard -> (sj inv ids && contains inv MorphBall) || containsAll inv [MorphBall, BoostBall, Missile, SuperMissile, ChargeBeam, SpiderBall, MorphBallBomb]
        VeryHard -> (sj inv ids && contains inv MorphBall) || containsAll inv [MorphBall, BoostBall, Missile, SuperMissile, ChargeBeam, MorphBallBomb]
        Expert -> (sj inv ids && contains inv MorphBall) || containsAll inv [MorphBall, BoostBall, Missile, SuperMissile, ChargeBeam, MorphBallBomb]

hoteWave :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
hoteWave diff inv ids =
    case diff of
        Easy -> containsAll inv [MorphBall, SpiderBall, MorphBallBomb, WaveBeam]
        Medium -> containsAll inv [MorphBall, MorphBallBomb, WaveBeam] && (spider inv ids || sj inv ids)
        Hard -> containsAll inv [MorphBall, MorphBallBomb, WaveBeam] && (spider inv ids || sj inv ids)
        VeryHard -> containsAll inv [MorphBall, MorphBallBomb] -- We don't need to even use the wave slot using an HBJ
        Expert -> containsAll inv [MorphBall, MorphBallBomb]

hoteIce :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
hoteIce diff inv ids =
    case diff of
        Easy -> containsAll inv [MorphBall, SpiderBall, MorphBallBomb, IceBeam]
        Medium -> containsAll inv [MorphBall, MorphBallBomb, IceBeam] && (spider inv ids || sj inv ids)
        Hard -> containsAll inv [MorphBall, MorphBallBomb, IceBeam] && (spider inv ids || sj inv ids)
        VeryHard -> containsAll inv [MorphBall, MorphBallBomb, IceBeam]
        Expert -> containsAll inv [MorphBall, MorphBallBomb, IceBeam]

hotePlasma :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
hotePlasma diff inv ids =
    case diff of
        Easy -> containsAll inv [MorphBall, SpiderBall, MorphBallBomb, IceBeam, PlasmaBeam]
        Medium -> containsAll inv [MorphBall, MorphBallBomb, IceBeam, PlasmaBeam] && (spider inv ids || sj inv ids)
        Hard -> containsAll inv [MorphBall, MorphBallBomb, IceBeam, PlasmaBeam] && (spider inv ids || sj inv ids)
        VeryHard -> containsAll inv [MorphBall, MorphBallBomb, IceBeam, PlasmaBeam]
        Expert -> containsAll inv [MorphBall, MorphBallBomb, IceBeam, PlasmaBeam]

reflectPoolSave :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
reflectPoolSave diff inv ids = reflectPoolAntechamber diff inv ids && contains inv Missile

reflectPoolIceDoor :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
reflectPoolIceDoor diff inv ids = reflectPoolAntechamber diff inv ids && contains inv IceBeam

reflectPoolAntechamber :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
reflectPoolAntechamber diff inv ids =
    case diff of
        Easy -> containsAll inv [MorphBall, MorphBallBomb, BoostBall]
        Medium -> containsAll inv [MorphBall, MorphBallBomb, BoostBall] || sj inv ids
        Hard -> containsAll inv [MorphBall, MorphBallBomb, BoostBall] || sj inv ids
        VeryHard -> bombs inv ids || sj inv ids
        Expert -> bombs inv ids || sj inv ids

-- Magmoor Predicates
vmr1Tank :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
vmr1Tank diff inv ids =
    case diff of
        Easy -> heatResist inv ids
        Medium -> heatResist inv ids
        Hard -> heatResist inv ids || containsCount 2 EnergyTank inv && sj inv ids
        VeryHard -> heatResist inv ids || (containsCount 1 EnergyTank inv && sj inv ids) || containsCount 2 EnergyTank inv 
        Expert -> heatResist inv ids || (containsCount 1 EnergyTank inv && sj inv ids) || containsCount 2 EnergyTank inv 

vmr2Tank :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
vmr2Tank diff inv ids =
    case diff of
        Easy -> heatResist inv ids
        Medium -> heatResist inv ids
        Hard -> heatResist inv ids || containsCount 3 EnergyTank inv && sj inv ids
        VeryHard -> heatResist inv ids || (containsCount 2 EnergyTank inv && sj inv ids) || containsCount 3 EnergyTank inv 
        Expert -> heatResist inv ids || (containsCount 2 EnergyTank inv && sj inv ids) || containsCount 3 EnergyTank inv 

vmr3Tank :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
vmr3Tank diff inv ids =
    case diff of
        Easy -> heatResist inv ids
        Medium -> heatResist inv ids
        Hard -> heatResist inv ids || containsCount 4 EnergyTank inv && sj inv ids
        VeryHard -> heatResist inv ids || (containsCount 3 EnergyTank inv && sj inv ids) || containsCount 4 EnergyTank inv 
        Expert -> heatResist inv ids || (containsCount 3 EnergyTank inv && sj inv ids) || containsCount 4 EnergyTank inv 

vmr4Tank :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
vmr4Tank diff inv ids =
    case diff of
        Easy -> heatResist inv ids
        Medium -> heatResist inv ids
        Hard -> heatResist inv ids || containsCount 5 EnergyTank inv && sj inv ids
        VeryHard -> heatResist inv ids || (containsCount 4 EnergyTank inv && sj inv ids) || containsCount 5 EnergyTank inv 
        Expert -> heatResist inv ids || (containsCount 4 EnergyTank inv && sj inv ids) || containsCount 5 EnergyTank inv 

heatResistOr8Etanks :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
heatResistOr8Etanks diff inv ids =
    case diff of
        Easy -> heatResist inv ids
        Medium -> heatResist inv ids
        Hard -> heatResist inv ids || containsCount 8 EnergyTank inv 
        VeryHard -> heatResist inv ids || containsCount 8 EnergyTank inv 
        Expert -> heatResist inv ids || containsCount 8 EnergyTank inv 

burningTrailSw :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
burningTrailSw diff inv ids =
    case diff of
        Easy -> False
        Medium -> False
        Hard -> False
        VeryHard -> bombs inv ids
        Expert -> bombs inv ids

magmoorFrontWallcrawl :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
magmoorFrontWallcrawl diff inv ids =
    case diff of
        Easy -> False
        Medium -> False
        Hard -> False
        VeryHard -> False
        Expert -> heatResist inv ids && bombs inv ids

lavaLakeTraversal :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
lavaLakeTraversal diff inv ids = vmr4Tank diff inv ids && bombs inv ids

lavaLakeReverseTraversal :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
lavaLakeReverseTraversal diff inv ids = vmr2Tank diff inv ids && bombs inv ids

lavaLakeItem :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
lavaLakeItem diff inv ids =
    case diff of
        Easy -> missile inv ids && sj inv ids && heatResist inv ids
        Medium -> missile inv ids && sj inv ids && heatResist inv ids
        Hard -> missile inv ids && (heatResist inv ids || (sj inv ids && containsCount 2 EnergyTank inv ))
        VeryHard -> missile inv ids && (heatResist inv ids || (sj inv ids && contains inv EnergyTank) || containsCount 2 EnergyTank inv )
        Expert -> missile inv ids && (heatResist inv ids || (sj inv ids && contains inv EnergyTank) || containsCount 2 EnergyTank inv )

pitTunnel :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
pitTunnel diff inv ids = vmr2Tank diff inv ids && contains inv MorphBall

pitTunnelReverse :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
pitTunnelReverse diff inv ids = vmr3Tank diff inv ids && contains inv MorphBall

triclopsPitItem :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
triclopsPitItem diff inv ids =
    case diff of
        Easy -> containsAll inv [SpaceJumpBoots, Missile] && heatResist inv ids
        Medium -> containsAll inv [SpaceJumpBoots, Missile] && heatResist inv ids
        Hard -> missile inv ids && vmr1Tank diff inv ids
        VeryHard -> missile inv ids && vmr1Tank diff inv ids
        Expert -> missile inv ids && vmr1Tank diff inv ids

storageCavern :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
storageCavern diff inv ids = morph inv ids && vmr1Tank diff inv ids

toTransportTunnelA :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
toTransportTunnelA diff inv ids = bombs inv ids && vmr1Tank diff inv ids

monitorStationClimb :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
monitorStationClimb diff inv ids =
    case diff of
        Easy -> heatResist inv ids && containsAll inv [SpaceJumpBoots, MorphBall, BoostBall]
        Medium -> heatResist inv ids && containsAll inv [SpaceJumpBoots, MorphBall, BoostBall]
        Hard -> vmr3Tank diff inv ids && (sj inv ids || bombs inv ids)
        VeryHard -> vmr3Tank diff inv ids && (sj inv ids || bombs inv ids)
        Expert -> vmr3Tank diff inv ids && (sj inv ids || bombs inv ids)

warriorShrineTunnel :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
warriorShrineTunnel diff inv ids = vmr4Tank diff inv ids && pb inv ids && bombs inv ids

crossTft :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
crossTft diff inv ids =
    case diff of
        Easy -> spider inv ids && heatResist inv ids
        Medium -> spider inv ids && heatResist inv ids
        Hard -> (spider inv ids && heatResist inv ids) || sj inv ids || (contains inv GravitySuit && bombs inv ids && containsCount 2 EnergyTank inv )
        VeryHard -> (spider inv ids && heatResist inv ids) || sj inv ids || (contains inv GravitySuit && bombs inv ids && containsCount 2 EnergyTank inv )
        Expert -> (spider inv ids && heatResist inv ids) || sj inv ids || (contains inv GravitySuit && bombs inv ids && containsCount 2 EnergyTank inv )

crossTftReverse :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
crossTftReverse diff inv ids =
    case diff of
        Easy -> spider inv ids && heatResist inv ids
        Medium -> (spider inv ids && heatResist inv ids) || sj inv ids || (bombs inv ids && containsCount 3 EnergyTank inv )
        Hard -> (spider inv ids && heatResist inv ids) || sj inv ids || (bombs inv ids && containsCount 2 EnergyTank inv )
        VeryHard -> heatResist inv ids || sj inv ids || containsCount 2 EnergyTank inv 
        Expert -> heatResist inv ids || sj inv ids || containsCount 2 EnergyTank inv 

crossTwinFires :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
crossTwinFires diff inv ids =
    case diff of
        Easy -> sj inv ids && contains inv WaveBeam
        Medium -> sj inv ids && contains inv WaveBeam
        Hard -> sj inv ids && contains inv WaveBeam
        VeryHard -> (sj inv ids || missile inv ids) && contains inv WaveBeam
        Expert -> (sj inv ids || missile inv ids) && contains inv WaveBeam

crossNorthCoreTunnel :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
crossNorthCoreTunnel diff inv ids =
    case diff of
        Easy -> containsAll inv [Missile, SpaceJumpBoots, WaveBeam]
        Medium -> containsAll inv [Missile, SpaceJumpBoots, WaveBeam]
        Hard -> containsAll inv [SpaceJumpBoots, WaveBeam]
        VeryHard -> contains inv WaveBeam && (missile inv ids || sj inv ids)
        Expert -> contains inv WaveBeam && (missile inv ids || sj inv ids)

workstationTunnel :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
workstationTunnel _ inv _ = containsAll inv [IceBeam, PowerBomb, MorphBall]

workstationItem :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
workstationItem _ inv _ = containsAll inv [MorphBall, WaveBeam]

workstationWaveDoor :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
workstationWaveDoor _ inv ids = sjOrBombs inv ids && contains inv WaveBeam

workstationSw :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
workstationSw diff inv ids =
    case diff of
        Easy -> False
        Medium -> False
        Hard -> False
        VeryHard -> sj inv ids
        Expert -> sj inv ids || (ice inv ids && bombs inv ids)

geoCore :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
geoCore diff inv _ =
    case diff of
        Easy -> containsAll inv [SpaceJumpBoots, GrappleBeam, SpiderBall, MorphBall, MorphBallBomb, BoostBall, IceBeam]
        Medium -> containsAll inv [SpaceJumpBoots, GrappleBeam, SpiderBall, MorphBall, MorphBallBomb, BoostBall, IceBeam]
        Hard -> containsAll inv [SpaceJumpBoots, MorphBall, MorphBallBomb, BoostBall, IceBeam]
        VeryHard -> containsAll inv [SpaceJumpBoots, MorphBall, MorphBallBomb, BoostBall, IceBeam]
        Expert -> containsAll inv [SpaceJumpBoots, MorphBall, MorphBallBomb, IceBeam]

-- Phendrana Predicates
iceBarrier :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
iceBarrier _ inv _ = containsAny inv [Missile, ChargeBeam]

shorelinesTower :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
shorelinesTower diff inv ids =
    case diff of
        Easy -> containsAll inv [MorphBall, SpaceJumpBoots, SpiderBall, ChargeBeam, SuperMissile, Missile]
        Medium -> containsAll inv [MorphBall, SpaceJumpBoots, SpiderBall, ChargeBeam, SuperMissile, Missile]
        Hard -> containsAll inv [MorphBall, SpaceJumpBoots, SpiderBall, ChargeBeam, SuperMissile, Missile]
        VeryHard -> containsAll inv [MorphBall, SpiderBall, ChargeBeam, SuperMissile, Missile] && sjOrBombs inv ids
        Expert -> containsAll inv [MorphBall, SpiderBall, ChargeBeam, SuperMissile, Missile] && sjOrBombs inv ids

shorelinesItem :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
shorelinesItem diff inv ids =
    case diff of
        Easy -> plasma inv ids
        Medium -> plasma inv ids
        Hard -> plasma inv ids || (wave inv ids && boost inv ids && sj inv ids && bombs inv ids) -- Infinite Speed
        VeryHard -> plasma inv ids || (wave inv ids && boost inv ids && sj inv ids && bombs inv ids)
        Expert -> plasma inv ids || (wave inv ids && boost inv ids && sj inv ids && bombs inv ids)

iceTempleClimb :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
iceTempleClimb diff inv _ =
    case diff of
        Easy -> containsAll inv [SpaceJumpBoots, MorphBall, MorphBallBomb, Missile]
        Medium -> containsAll inv [SpaceJumpBoots, MorphBall, MorphBallBomb, Missile]
        Hard -> containsAll inv [MorphBall, MorphBallBomb, Missile]
        VeryHard -> containsAll inv [MorphBall, MorphBallBomb, Missile]
        Expert -> containsAll inv [MorphBall, MorphBallBomb, Missile]

iceTempleItem :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
iceTempleItem diff inv ids =
    case diff of
        Easy -> containsAll inv [SpaceJumpBoots, MorphBall, MorphBallBomb, PlasmaBeam]
        Medium -> containsAll inv [SpaceJumpBoots, MorphBall, MorphBallBomb, PlasmaBeam]
        Hard -> containsAll inv [MorphBall, MorphBallBomb, PlasmaBeam] || (sj inv ids && boost inv ids && bombs inv ids && missile inv ids) -- Infinite Speed
        VeryHard -> containsAll inv [MorphBall, MorphBallBomb, PlasmaBeam] || (sj inv ids && boost inv ids && bombs inv ids && missile inv ids)
        Expert -> containsAll inv [MorphBall, MorphBallBomb, PlasmaBeam] || (sj inv ids && boost inv ids && bombs inv ids && missile inv ids)

climbShorelines :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
climbShorelines diff inv ids =
    case diff of
        Easy -> sj inv ids
        Medium -> sj inv ids
        Hard -> sj inv ids
        VeryHard -> sjOrBombs inv ids
        Expert -> sjOrBombs inv ids

ireSpiderTrack :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
ireSpiderTrack diff inv ids =
    case diff of
        Easy -> spider inv ids
        Medium -> spider inv ids
        Hard -> spider inv ids
        VeryHard -> spider inv ids || bombs inv ids
        Expert -> spider inv ids || bombs inv ids

irwDoor :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
irwDoor diff inv ids =
    case diff of
        Easy -> containsAll inv [SpaceJumpBoots, WaveBeam]
        Medium -> containsAll inv [SpaceJumpBoots, WaveBeam]
        Hard -> containsAll inv [SpaceJumpBoots, WaveBeam]
        VeryHard -> sjOrBombs inv ids && wave inv ids
        Expert -> sjOrBombs inv ids && wave inv ids

irwItem :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
irwItem diff inv ids =
    case diff of
        Easy -> containsAll inv [SpaceJumpBoots, PlasmaBeam]
        Medium -> containsAll inv [SpaceJumpBoots, PlasmaBeam]
        Hard -> plasma inv ids && (contains inv SpaceJumpBoots || containsAll inv [MorphBall,MorphBallBomb,Missile])
        VeryHard -> plasma inv ids && (contains inv SpaceJumpBoots || containsAll inv [MorphBall,MorphBallBomb,Missile])
        Expert -> plasma inv ids && (contains inv SpaceJumpBoots || containsAll inv [MorphBall,MorphBallBomb,Missile])

irwSw :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
irwSw diff inv ids =
    case diff of
        Easy -> False
        Medium -> False
        Hard -> False
        VeryHard -> bombs inv ids
        Expert -> bombs inv ids

ruinedCourtyardConduit :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
ruinedCourtyardConduit _ inv _ = containsAll inv [ChargeBeam, Missile, SuperMissile, WaveBeam]

ruinedCourtyardSave :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
ruinedCourtyardSave _ inv _ = containsAll inv [SpaceJumpBoots, Missile]

--Might not need bombs if using spider track, but bombs are almost always unrandomized anyway
ruinedCourtyardClimb :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
ruinedCourtyardClimb diff inv ids =
    case diff of
        Easy -> (spider inv ids && sjOrBombs inv ids) || containsAll inv [SpaceJumpBoots, MorphBall, BoostBall, MorphBallBomb]
        Medium -> (spider inv ids && sjOrBombs inv ids) || containsAll inv [SpaceJumpBoots, MorphBall, BoostBall, MorphBallBomb]
        Hard -> (spider inv ids && sjOrBombs inv ids) || sj inv ids
        VeryHard -> sjOrBombs inv ids
        Expert -> sjOrBombs inv ids

ruinedCourtyardSw :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
ruinedCourtyardSw diff inv ids =
    case diff of
        Easy -> False
        Medium -> False
        Hard -> sj inv ids
        VeryHard -> sj inv ids
        Expert -> sj inv ids

quarantineTunnel :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
quarantineTunnel _ inv _ = containsAll inv [MorphBall, WaveBeam]

climbQuarantineCaveEntrance :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
climbQuarantineCaveEntrance diff inv ids =
    case diff of
        Easy -> spider inv ids
        Medium -> spider inv ids
        Hard -> spider inv ids || sj inv ids
        VeryHard -> spider inv ids || sj inv ids
        Expert -> spider inv ids || sj inv ids

climbQuarantineCaveBack :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
climbQuarantineCaveBack diff inv ids =
    case diff of
        Easy -> spider inv ids
        Medium -> spider inv ids || grapple inv ids
        Hard -> spider inv ids || sj inv ids || grapple inv ids
        VeryHard -> spider inv ids || sj inv ids || grapple inv ids
        Expert -> spider inv ids || sj inv ids || grapple inv ids

quarantineMonitor :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
quarantineMonitor diff inv ids =
    case diff of
        Easy -> grapple inv ids
        Medium -> grapple inv ids
        Hard -> grapple inv ids || sj inv ids
        VeryHard -> grapple inv ids || sj inv ids
        Expert -> grapple inv ids || sj inv ids

phenElevatorClimb :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
phenElevatorClimb diff inv ids =
    case diff of
        Easy -> containsAll inv [MorphBall, SpiderBall, IceBeam]
        Medium -> containsAll inv [MorphBall, SpiderBall, IceBeam]
        Hard -> (spider inv ids || sj inv ids) && ice inv ids
        VeryHard -> (spider inv ids || sj inv ids || bombs inv ids) && ice inv ids
        Expert -> (spider inv ids || sj inv ids || bombs inv ids) && ice inv ids

observatoryClimb :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
observatoryClimb diff inv ids =
    case diff of
        Easy -> containsAll inv [MorphBall, BoostBall, MorphBallBomb, SpaceJumpBoots]
        Medium -> containsAll inv [MorphBall, BoostBall, MorphBallBomb, SpaceJumpBoots]
        Hard -> sj inv ids
        VeryHard -> sj inv ids
        Expert -> sj inv ids

observatorySave :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
observatorySave diff inv ids =
    case diff of
        Easy -> sj inv ids && contains inv Missile
        Medium -> sj inv ids && contains inv Missile
        Hard -> sj inv ids && contains inv Missile
        VeryHard -> contains inv Missile
        Expert -> contains inv Missile

observatoryItem :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
observatoryItem diff inv ids =
    case diff of
        Easy -> containsAll inv [MorphBall, BoostBall, MorphBallBomb, SpaceJumpBoots]
        Medium -> sj inv ids
        Hard -> sj inv ids
        VeryHard -> sjOrBombs inv ids
        Expert -> sjOrBombs inv ids

controlTowerItem :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
controlTowerItem diff inv ids =
    case diff of
        Easy -> containsAll inv [MorphBall, PlasmaBeam, Missile] && sjOrBombs inv ids
        Medium -> containsAll inv [MorphBall, PlasmaBeam, Missile] && sjOrBombs inv ids
        Hard -> (bombs inv ids && plasma inv ids && missile inv ids) || (sj inv ids && missile inv ids && morph inv ids)
        VeryHard -> (bombs inv ids && plasma inv ids && missile inv ids) || (sj inv ids && missile inv ids && morph inv ids)
        Expert -> (bombs inv ids && plasma inv ids && missile inv ids) || (sj inv ids && missile inv ids && morph inv ids)

rlaTrack :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
rlaTrack _ inv _ = contains inv MorphBall && containsAny inv [MorphBallBomb, SpaceJumpBoots]

toStorageCave :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
toStorageCave diff inv ids =
    case diff of
        Easy -> containsAll inv [SpaceJumpBoots, GrappleBeam, PlasmaBeam, MorphBall, PowerBomb]
        Medium -> containsAll inv [SpaceJumpBoots, PlasmaBeam, MorphBall, PowerBomb]
        Hard -> containsAll inv [SpaceJumpBoots, PlasmaBeam, MorphBall, PowerBomb]
        VeryHard -> containsAll inv [PlasmaBeam, MorphBall, PowerBomb] && sjOrBombs inv ids
        Expert -> containsAll inv [PlasmaBeam, MorphBall, PowerBomb] && sjOrBombs inv ids

fromStorageCave :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
fromStorageCave _ inv _ = containsAll inv [PlasmaBeam, MorphBall, PowerBomb]

toSecurityCave :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
toSecurityCave diff inv ids =
    case diff of
        Easy -> containsAll inv [SpaceJumpBoots, GrappleBeam, MorphBall]
        Medium -> containsAll inv [SpaceJumpBoots, MorphBall]
        Hard -> containsAll inv [SpaceJumpBoots, MorphBall]
        VeryHard -> morph inv ids && (sj inv ids || (bombs inv ids && grapple inv ids))
        Expert -> morph inv ids && (sj inv ids || (bombs inv ids && grapple inv ids))

phenEdgeLower :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
phenEdgeLower diff inv ids =
    case diff of
        Easy -> containsAll inv [WaveBeam, GravitySuit, SpaceJumpBoots]
        Medium -> containsAll inv [WaveBeam, SpaceJumpBoots]
        Hard -> containsAll inv [WaveBeam, SpaceJumpBoots]
        VeryHard -> wave inv ids && sjOrBombs inv ids
        Expert -> wave inv ids && sjOrBombs inv ids

frozenPikeBottom :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
frozenPikeBottom diff inv ids =
    case diff of
        Easy -> containsAll inv [WaveBeam, GravitySuit, SpaceJumpBoots]
        Medium -> wave inv ids && sjOrBombs inv ids
        Hard -> wave inv ids && sjOrBombs inv ids
        VeryHard -> wave inv ids && sjOrBombs inv ids
        Expert -> wave inv ids && sjOrBombs inv ids

frozenPikeClimb :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
frozenPikeClimb diff inv ids =
    case diff of
        Easy -> containsAll inv [MorphBall, MorphBallBomb, SpaceJumpBoots]
        Medium -> containsAll inv [MorphBall, MorphBallBomb, SpaceJumpBoots]
        Hard -> containsAll inv [MorphBall, MorphBallBomb, SpaceJumpBoots]
        VeryHard -> sjOrBombs inv ids
        Expert -> sjOrBombs inv ids

gravLedge :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
gravLedge diff inv ids =
    case diff of
        Easy -> containsAll inv [PlasmaBeam, GrappleBeam]
        Medium -> containsAll inv [PlasmaBeam, GrappleBeam]
        Hard -> containsAll inv [PlasmaBeam, GrappleBeam] || sj inv ids
        VeryHard -> containsAll inv [PlasmaBeam, GrappleBeam] || sj inv ids
        Expert -> containsAll inv [PlasmaBeam, GrappleBeam] || sj inv ids

climbGravityChamber :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
climbGravityChamber diff inv ids =
    case diff of
        Easy -> contains inv GravitySuit && sjOrBombs inv ids
        Medium -> contains inv GravitySuit && sjOrBombs inv ids
        Hard -> contains inv GravitySuit && sjOrBombs inv ids
        VeryHard -> (contains inv GravitySuit && sjOrBombs inv ids) || sj inv ids
        Expert -> (contains inv GravitySuit && sjOrBombs inv ids) || sj inv ids

gravityChamberToLakeTunnel :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
gravityChamberToLakeTunnel diff inv ids = climbGravityChamber diff inv ids && contains inv WaveBeam

hunterCaveClimb :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
hunterCaveClimb diff inv ids =
    case diff of
        Easy -> contains inv Missile && (contains inv SpaceJumpBoots || bombs inv ids)
        Medium -> sjOrBombs inv ids
        Hard -> sjOrBombs inv ids
        VeryHard -> sjOrBombs inv ids
        Expert -> sjOrBombs inv ids

hunterCaveUpper :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
hunterCaveUpper diff inv ids =
    case diff of
        Easy -> containsAll inv [Missile, GrappleBeam]
        Medium -> containsAll inv [Missile, GrappleBeam]
        Hard -> sj inv ids || containsAll inv [Missile, GrappleBeam]
        VeryHard -> sj inv ids || containsAll inv [Missile, GrappleBeam]
        Expert -> sj inv ids || containsAll inv [Missile, GrappleBeam]

hunterCaveLower :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
hunterCaveLower _ inv ids = contains inv Missile && (contains inv SpaceJumpBoots || bombs inv ids)

frostCaveAccess :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
frostCaveAccess _ inv _ = containsAll inv [MorphBall, WaveBeam]

frostCaveDoor :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
frostCaveDoor _ inv ids = containsAll inv [Missile, WaveBeam] && (contains inv SpaceJumpBoots || bombs inv ids)

frostCaveItem :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
frostCaveItem diff inv ids =
    case diff of
        Easy -> containsAll inv [GrappleBeam, Missile]
        Medium -> containsAll inv [GrappleBeam, Missile]
        Hard -> missile inv ids && (sj inv ids || grapple inv ids)
        VeryHard -> missile inv ids && (sj inv ids || bombs inv ids || grapple inv ids)
        Expert -> missile inv ids && (sj inv ids || bombs inv ids || grapple inv ids)

frostCaveToTunnel :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
frostCaveToTunnel _ inv ids = containsAll inv [Missile, WaveBeam, MorphBall] && (contains inv SpaceJumpBoots || bombs inv ids)

frostCaveSw :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
frostCaveSw diff inv ids =
    case diff of
        Easy -> False
        Medium -> False
        Hard -> sj inv ids
        VeryHard -> sj inv ids || (ice inv ids && wave inv ids && bombs inv ids)
        Expert -> sj inv ids || (ice inv ids && wave inv ids && bombs inv ids)

transportAccessItemOob :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
transportAccessItemOob diff inv ids =
    case diff of
        Easy -> False
        Medium -> False
        Hard -> False
        VeryHard -> bombs inv ids && boost inv ids -- This is annoying but possible with boost
        Expert -> bombs inv ids && boost inv ids

-- Mines Predicates
quarrySave :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
quarrySave diff inv ids =
    case diff of
        Easy -> containsAll inv [SpiderBall, MorphBall, WaveBeam]
        Medium -> wave inv ids && (spider inv ids || sj inv ids)
        Hard -> wave inv ids && (spider inv ids || sj inv ids)
        VeryHard -> wave inv ids && (spider inv ids || sj inv ids || bombs inv ids)
        Expert -> wave inv ids && (spider inv ids || sj inv ids || bombs inv ids)

quarryItem :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
quarryItem diff inv ids =
    case diff of
        Easy -> containsAll inv [SpaceJumpBoots, WaveBeam, MorphBall, SpiderBall]
        Medium -> containsAll inv [SpaceJumpBoots, WaveBeam, MorphBall, SpiderBall]
        Hard -> wave inv ids && (spider inv ids || sj inv ids)
        VeryHard -> wave inv ids && (spider inv ids || sj inv ids)
        Expert -> wave inv ids && (spider inv ids || sj inv ids)

reachWasteDisposal :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
reachWasteDisposal diff inv ids =
    case diff of
        Easy -> containsAll inv [SpaceJumpBoots, WaveBeam, IceBeam, GrappleBeam]
        Medium -> containsAll inv [WaveBeam, IceBeam, GrappleBeam] && sjOrBombs inv ids
        Hard -> ice inv ids && sjOrBombs inv ids
        VeryHard -> ice inv ids && sjOrBombs inv ids
        Expert -> ice inv ids && sjOrBombs inv ids

oreProcessingClimb :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
oreProcessingClimb diff inv ids =
    case diff of
        Easy -> containsAll inv [MorphBall, SpiderBall, MorphBallBomb, IceBeam]
        Medium -> containsAll inv [MorphBall, SpiderBall, MorphBallBomb, IceBeam]
        Hard -> ice inv ids && (containsAll inv [MorphBall, SpiderBall, MorphBallBomb] || sj inv ids)
        VeryHard -> ice inv ids && (containsAll inv [MorphBall, SpiderBall, MorphBallBomb] || sj inv ids)
        Expert -> ice inv ids && (containsAll inv [MorphBall, SpiderBall, MorphBallBomb] || sj inv ids)

oreProcessingTop :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
oreProcessingTop diff inv ids =
    case diff of
        Easy -> containsAll inv [MorphBall, SpiderBall, MorphBallBomb, PowerBomb, IceBeam]
        Medium -> containsAll inv [MorphBall, SpiderBall, MorphBallBomb, PowerBomb, IceBeam]
        Hard -> ice inv ids && (containsAll inv [MorphBall, SpiderBall, MorphBallBomb, PowerBomb] || sj inv ids)
        VeryHard -> ice inv ids && (containsAll inv [MorphBall, SpiderBall, MorphBallBomb, PowerBomb] || sj inv ids)
        Expert -> ice inv ids && (containsAll inv [MorphBall, SpiderBall, MorphBallBomb, PowerBomb] || sj inv ids)

oreProcessingCrossTop :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
oreProcessingCrossTop diff inv _ =
    case diff of
        Easy -> containsAll inv [GrappleBeam, IceBeam]
        Medium -> contains inv IceBeam
        Hard -> contains inv IceBeam
        VeryHard -> contains inv IceBeam
        Expert -> contains inv IceBeam
    
oreProcessingTopFromRocks :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
oreProcessingTopFromRocks diff inv _ =
    case diff of
        Easy -> False
        Medium -> False
        Hard -> contains inv SpaceJumpBoots
        VeryHard -> containsAny inv [SpaceJumpBoots, GrappleBeam]
        Expert -> containsAny inv [SpaceJumpBoots, GrappleBeam]

oreProcessingTopFromEaa :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
oreProcessingTopFromEaa diff inv ids =
    case diff of
        Easy -> False
        Medium -> containsAll inv [IceBeam, GrappleBeam]
        Hard -> ice inv ids && containsAny inv [SpaceJumpBoots, GrappleBeam]
        VeryHard -> ice inv ids && containsAny inv [SpaceJumpBoots, GrappleBeam]
        Expert -> ice inv ids && containsAny inv [SpaceJumpBoots, GrappleBeam]

dashFromPbRocks :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
dashFromPbRocks diff inv _ =
    case diff of
        Easy -> False
        Medium -> False
        Hard -> containsAll inv [SpaceJumpBoots,IceBeam]
        VeryHard -> contains inv IceBeam
        Expert -> contains inv IceBeam

wasteDisposalTraversal :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
wasteDisposalTraversal _ inv _ = containsAll inv [MorphBall, MorphBallBomb, IceBeam]

shaftClimb1 :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
shaftClimb1 diff inv ids =
    case diff of
        Easy -> containsAll inv [MorphBall, SpiderBall, IceBeam]
        Medium -> containsAll inv [MorphBall, SpiderBall, IceBeam]
        Hard -> ice inv ids && (spider inv ids || (bombs inv ids && sj inv ids))
        VeryHard -> ice inv ids && (spider inv ids || (bombs inv ids && sj inv ids))
        Expert -> ice inv ids && (spider inv ids || (bombs inv ids && sj inv ids))

-- This assumes you need boost to get through the wall in the next room
shaftClimb2 :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
shaftClimb2 diff inv ids =
    case diff of
        Easy -> containsAll inv [MorphBall, SpiderBall, BoostBall, IceBeam]
        Medium -> containsAll inv [MorphBall, SpiderBall, BoostBall, IceBeam]
        Hard -> ice inv ids && boost inv ids && (spider inv ids || sj inv ids)
        VeryHard -> ice inv ids && boost inv ids && (spider inv ids || sj inv ids)
        Expert -> ice inv ids && boost inv ids && (spider inv ids || sj inv ids)

storageDepotABarrier :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
storageDepotABarrier _ inv _ = contains inv StorageDepotABarrier

securityAccessBSw :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
securityAccessBSw diff inv ids =
    case diff of
        Easy -> False
        Medium -> False
        Hard -> False
        VeryHard -> bombs inv ids
        Expert -> bombs inv ids

maintTunnel :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
maintTunnel _ inv _ = containsAll inv [MorphBall, IceBeam, PowerBomb]

ppcClimb :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
ppcClimb diff inv ids =
    case diff of
        Easy -> containsAll inv [MorphBall, SpiderBall, SpaceJumpBoots, IceBeam]
        Medium -> containsAll inv [MorphBall, SpiderBall, SpaceJumpBoots, IceBeam]
        Hard -> ice inv ids && sj inv ids
        VeryHard -> ice inv ids && sjOrBombs inv ids
        Expert -> ice inv ids && sjOrBombs inv ids

toMinesElevator :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
toMinesElevator diff inv ids =
    case diff of
        Easy -> containsAll inv [GrappleBeam, IceBeam]
        Medium -> ice inv ids && (grapple inv ids || sj inv ids)
        Hard -> ice inv ids
        VeryHard -> ice inv ids
        Expert -> ice inv ids

centralDynamoClimb :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
centralDynamoClimb diff inv ids =
    case diff of
        Easy -> contains inv IceBeam && sj inv ids
        Medium -> contains inv IceBeam && sj inv ids
        Hard -> contains inv IceBeam && sj inv ids
        VeryHard -> contains inv IceBeam && sjOrBombs inv ids
        Expert -> contains inv IceBeam && sjOrBombs inv ids

mqaItem :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
mqaItem diff inv ids =
    case diff of
        Easy -> containsAll inv [SpaceJumpBoots, XRayVisor, MorphBall, PowerBomb]
        Medium -> containsAll inv [SpaceJumpBoots, MorphBall, PowerBomb]
        Hard -> sj inv ids
        VeryHard -> sj inv ids || (bombs inv ids && pb inv ids)
        Expert -> sj inv ids || (bombs inv ids && pb inv ids)

mqaTraversal :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
mqaTraversal diff inv ids =
    case diff of
        Easy -> containsAll inv [SpaceJumpBoots, XRayVisor, MorphBall, SpiderBall, IceBeam]
        Medium -> containsAll inv [SpaceJumpBoots, MorphBall, SpiderBall, IceBeam]
        Hard -> ice inv ids && sj inv ids && (pb inv ids || spider inv ids)
        VeryHard -> ice inv ids && sjOrBombs inv ids && (pb inv ids || spider inv ids)
        Expert -> ice inv ids && sjOrBombs inv ids && (pb inv ids || spider inv ids)

ecaItem :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
ecaItem diff inv ids =
    case diff of
        Easy -> containsAll inv [SpaceJumpBoots, MorphBall, MorphBallBomb]
        Medium -> containsAll inv [SpaceJumpBoots, MorphBall, MorphBallBomb]
        Hard -> containsAll inv [SpaceJumpBoots, MorphBall, MorphBallBomb]
        VeryHard -> (morph inv ids && sj inv ids) || bombs inv ids
        Expert -> (morph inv ids && sj inv ids) || bombs inv ids

eliteResearchPirate :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
eliteResearchPirate _ inv ids = Data.Set.member CentralDynamo ids && pb inv ids

eliteResearchTopItem :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
eliteResearchTopItem diff inv ids =
    case diff of
        Easy -> containsAll inv [SpaceJumpBoots, MorphBall, BoostBall]
        Medium -> containsAll inv [SpaceJumpBoots, MorphBall, BoostBall]
        Hard -> containsAll inv [SpaceJumpBoots, MorphBall, BoostBall]
        VeryHard -> sjOrBombs inv ids
        Expert -> sjOrBombs inv ids

eliteResearchDoor :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
eliteResearchDoor diff inv ids =
    case diff of
        Easy -> containsAll inv [SpaceJumpBoots, MorphBall, BoostBall, IceBeam]
        Medium -> containsAll inv [SpaceJumpBoots, MorphBall, BoostBall, IceBeam]
        Hard -> containsAll inv [SpaceJumpBoots, MorphBall, BoostBall, IceBeam]
        VeryHard -> sjOrBombs inv ids && ice inv ids
        Expert -> sjOrBombs inv ids && ice inv ids

toStorageDepotA :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
toStorageDepotA diff inv _ =
    case diff of
        Easy -> containsAll inv [WaveBeam, MorphBall, PowerBomb, PlasmaBeam]
        Medium -> containsAll inv [MorphBall, PowerBomb, PlasmaBeam]
        Hard -> containsAll inv [MorphBall, PowerBomb, PlasmaBeam]
        VeryHard -> containsAll inv [MorphBall, PowerBomb, PlasmaBeam]
        Expert -> containsAll inv [MorphBall, PowerBomb, PlasmaBeam]

climbFungalHallAccess :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
climbFungalHallAccess diff inv ids =
    case diff of
        Easy -> containsAll inv [SpaceJumpBoots, PlasmaBeam]
        Medium -> containsAll inv [SpaceJumpBoots, PlasmaBeam]
        Hard -> containsAll inv [SpaceJumpBoots, PlasmaBeam]
        VeryHard -> sjOrBombs inv ids && plasma inv ids
        Expert -> sjOrBombs inv ids && plasma inv ids

fungalHallATraversal :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
fungalHallATraversal diff inv ids =
    case diff of
        Easy -> containsAll inv [SpaceJumpBoots, GrappleBeam, IceBeam]
        Medium -> containsAny inv [SpaceJumpBoots, GrappleBeam] && ice inv ids
        Hard -> containsAny inv [SpaceJumpBoots, GrappleBeam] && ice inv ids
        VeryHard -> sjOrBombs inv ids && ice inv ids
        Expert -> sjOrBombs inv ids && ice inv ids

miningTunnelTraversal :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
miningTunnelTraversal _ inv _ = containsAll inv [MorphBall, MorphBallBomb, PlasmaBeam]

-- High difficulties require farming for health. Varia isn't considered. This assumes you only need to survive until you warp.
miningTunnelItem :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
miningTunnelItem diff inv ids =
    case diff of
        Easy -> containsAll inv [MorphBall, MorphBallBomb, PhazonSuit]
        Medium -> containsAll inv [MorphBall, MorphBallBomb, PhazonSuit]
        Hard -> containsAll inv [MorphBall, MorphBallBomb] && (contains inv PhazonSuit || (contains inv GravitySuit && containsCount 10 EnergyTank inv && boost inv ids))
        VeryHard ->
            containsAll inv [MorphBall, MorphBallBomb] &&
            (contains inv PhazonSuit || (((contains inv GravitySuit && containsCount 6 EnergyTank inv ) || containsCount 10 EnergyTank inv ) && boost inv ids))
        Expert -> containsAll inv [MorphBall, MorphBallBomb] && (contains inv PhazonSuit || (containsCount 6 EnergyTank inv && boost inv ids))

quarantineAccessBTraversal :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
quarantineAccessBTraversal diff inv ids =
    case diff of
        Easy -> containsAll inv [SpaceJumpBoots, PlasmaBeam]
        Medium -> plasma inv ids
        Hard -> plasma inv ids
        VeryHard -> plasma inv ids
        Expert -> plasma inv ids

fungalHallBTraversal :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
fungalHallBTraversal diff inv ids =
    case diff of
        Easy -> containsAll inv [SpaceJumpBoots, GrappleBeam, PlasmaBeam]
        Medium -> containsAny inv [SpaceJumpBoots, GrappleBeam] && plasma inv ids
        Hard -> containsAny inv [SpaceJumpBoots, GrappleBeam] && plasma inv ids
        VeryHard -> sjOrBombs inv ids && plasma inv ids
        Expert -> sjOrBombs inv ids && plasma inv ids

mqbTraversal :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
mqbTraversal diff inv ids =
    case diff of
        Easy -> containsAll inv [SpiderBall, MorphBall, SpaceJumpBoots, GrappleBeam]
        Medium -> containsAll inv [SpiderBall, MorphBall, SpaceJumpBoots]
        Hard -> containsAll inv [SpiderBall, MorphBall, GrappleBeam] || sj inv ids
        VeryHard -> containsAll inv [SpiderBall, MorphBall, GrappleBeam] || sjOrBombs inv ids
        Expert -> containsAll inv [SpiderBall, MorphBall, GrappleBeam] || sjOrBombs inv ids

ppcBottomClimb :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
ppcBottomClimb diff inv ids =
    case diff of
        Easy -> containsAll inv [SpaceJumpBoots, PlasmaBeam, SpiderBall, MorphBall]
        Medium -> containsAll inv [SpaceJumpBoots, PlasmaBeam, SpiderBall, MorphBall]
        Hard -> containsAll inv [SpaceJumpBoots, PlasmaBeam]
        VeryHard -> sjOrBombs inv ids && plasma inv ids
        Expert -> sjOrBombs inv ids && plasma inv ids

eliteQuarters :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
eliteQuarters _ inv _ = contains inv XRayVisor

eliteQuartersPlasma :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
eliteQuartersPlasma diff inv ids = contains inv PlasmaBeam && eliteQuarters diff inv ids

eliteQuartersTop :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
eliteQuartersTop _ inv ids = contains inv PlasmaBeam && member EliteQuarters ids

mqbBackClimb :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
mqbBackClimb diff inv ids =
    case diff of
        Easy -> containsAll inv [SpaceJumpBoots, PlasmaBeam]
        Medium -> plasma inv ids && sjOrBombs inv ids
        Hard -> plasma inv ids && sjOrBombs inv ids
        VeryHard -> plasma inv ids && sjOrBombs inv ids
        Expert -> plasma inv ids && sjOrBombs inv ids

mqbSw :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
mqbSw diff inv ids =
    case diff of
        Easy -> False
        Medium -> False
        Hard -> sj inv ids && bombs inv ids
        VeryHard -> bombs inv ids
        Expert -> bombs inv ids
