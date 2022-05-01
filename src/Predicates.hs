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
complete x _ = containsCount 12 Artifact x && contains x PhazonSuit && contains x PlasmaBeam

morph :: Map ItemName Int -> Set ItemId -> Bool
morph x _ = contains x MorphBall

sj :: Map ItemName Int -> Set ItemId -> Bool
sj x _ = contains x SpaceJumpBoots

sjOrBombs :: Map ItemName Int -> Set ItemId -> Bool
sjOrBombs x ids = contains x SpaceJumpBoots || bombs x ids

missile :: Map ItemName Int -> Set ItemId -> Bool
missile x _ = contains x Missile

bombs :: Map ItemName Int -> Set ItemId -> Bool
bombs x _ = containsAll x [MorphBall, MorphBallBomb]

pb :: Map ItemName Int -> Set ItemId -> Bool
pb x _ = containsAll x [PowerBomb, MorphBall]

boost :: Map ItemName Int -> Set ItemId -> Bool
boost x _ = containsAll x [MorphBall, BoostBall]

spider :: Map ItemName Int -> Set ItemId -> Bool
spider x _ = containsAll x [MorphBall, SpiderBall]

waveIce :: Map ItemName Int -> Set ItemId -> Bool
waveIce x _ = containsAll x [WaveBeam, PlasmaBeam]

grapple :: Map ItemName Int -> Set ItemId -> Bool
grapple x _ = contains x GrappleBeam

boostBombs :: Map ItemName Int -> Set ItemId -> Bool
boostBombs x _ = containsAll x [MorphBall, BoostBall, MorphBallBomb]

morphMissile :: Map ItemName Int -> Set ItemId -> Bool
morphMissile x _ = containsAll x [MorphBall, Missile]

bombsPbs :: Map ItemName Int -> Set ItemId -> Bool
bombsPbs x _ = containsAll x [MorphBall, MorphBallBomb, PowerBomb]

wave :: Map ItemName Int -> Set ItemId -> Bool
wave x _ = contains x WaveBeam

ice :: Map ItemName Int -> Set ItemId -> Bool
ice x _ = contains x IceBeam

plasma :: Map ItemName Int -> Set ItemId -> Bool
plasma x _ = contains x PlasmaBeam

supers :: Map ItemName Int -> Set ItemId -> Bool
supers x _ = containsAll x [Missile, SuperMissile, ChargeBeam]

gravSpace :: Map ItemName Int -> Set ItemId -> Bool
gravSpace x _ = containsAll x [GravitySuit, SpaceJumpBoots]

wavePb :: Map ItemName Int -> Set ItemId -> Bool
wavePb x ids = wave x ids && pb x ids

heatResist :: Map ItemName Int -> Set ItemId -> Bool
heatResist x _ = containsAny x [VariaSuit, GravitySuit, PhazonSuit]

floaty :: Map ItemName Int -> Set ItemId -> Bool
floaty x _ = not $ contains x GravitySuit

-- A bit of an obscure trick, you can use infinite speed in landing site to unload the room
tallonFloaty :: Map ItemName Int -> Set ItemId -> Bool
tallonFloaty x ids = boost x ids && floaty x ids

noVines :: Map ItemName Int -> Set ItemId -> Bool
noVines _ ids = not (member SunchamberFlaahgra ids) || member SunchamberGhosts ids

mainQuarryBarrierIce :: Map ItemName Int -> Set ItemId -> Bool
mainQuarryBarrierIce x _ = containsAll x [MainQuarryBarriers, IceBeam]

mainQuarryBarrierWave :: Map ItemName Int -> Set ItemId -> Bool
mainQuarryBarrierWave x _ = containsAll x [MainQuarryBarriers, WaveBeam]

chozoIceTempleBarrier :: Map ItemName Int -> Set ItemId -> Bool
chozoIceTempleBarrier x _ = contains x ChozoIceTempleBarrier

researchLabHydraBarrier :: Map ItemName Int -> Set ItemId -> Bool
researchLabHydraBarrier x _ = contains x ResearchLabHydraBarrier

eliteControlBarrier :: Map ItemName Int -> Set ItemId -> Bool
eliteControlBarrier x _ = contains x EliteControlBarrier

mqaBarrier :: Map ItemName Int -> Set ItemId -> Bool 
mqaBarrier x _ = contains x MetroidQuarantineABarrier

mqbBarrier :: Map ItemName Int -> Set ItemId -> Bool
mqbBarrier x _ = contains x MetroidQuarantineBBarrier

wallcrawl :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
wallcrawl diff x ids =
    case diff of
        Easy -> False
        Medium -> False
        Hard -> bombs x ids
        VeryHard -> bombs x ids
        Expert -> bombs x ids

longWallcrawl :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
longWallcrawl diff x ids = bombs x ids && diff == Expert

-- Tallon Predicates
sjf :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
sjf diff x ids =
    case diff of
        Easy -> sj x ids
        Medium -> sj x ids
        Hard -> True
        VeryHard -> True
        Expert -> True

tallonCanyonSw :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
tallonCanyonSw diff x ids =
    case diff of
        Easy -> False
        Medium -> False
        Hard -> False
        VeryHard -> boost x ids && bombs x ids
        Expert -> bombs x ids

rootCaveItem :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
rootCaveItem diff x _ =
    case diff of
        Easy -> containsAll x [SpaceJumpBoots, GrappleBeam, XRayVisor]
        Medium -> containsAll x [SpaceJumpBoots, GrappleBeam]
        Hard -> contains x SpaceJumpBoots
        VeryHard -> contains x SpaceJumpBoots
        Expert -> contains x SpaceJumpBoots

arbor :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
arbor diff x _ =
    case diff of
        Easy -> containsAll x [SpaceJumpBoots, GrappleBeam, XRayVisor, PlasmaBeam]
        Medium -> containsAll x [SpaceJumpBoots, GrappleBeam, PlasmaBeam]
        Hard -> containsAll x [SpaceJumpBoots] -- You can wallcrawl around the plasma door
        VeryHard -> containsAll x [SpaceJumpBoots]
        Expert -> containsAll x [SpaceJumpBoots]

fcsClimb :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
fcsClimb diff x ids =
    case diff of
        Easy -> False
        Medium -> contains x SpaceJumpBoots && contains x IceBeam
        Hard -> contains x SpaceJumpBoots && contains x IceBeam
        VeryHard -> sjOrBombs x ids && contains x IceBeam
        Expert -> sjOrBombs x ids && contains x IceBeam

frigatePowerDoor :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
frigatePowerDoor diff x ids =
    case diff of
        Easy -> contains x FrigatePowerDoor
        Medium -> contains x FrigatePowerDoor
        Hard -> contains x FrigatePowerDoor
        VeryHard -> bombs x ids || contains x FrigatePowerDoor
        Expert -> bombs x ids || contains x FrigatePowerDoor

fcsEntry :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
fcsEntry diff x ids =
    case diff of
        Easy -> contains x IceBeam && (contains x GrappleBeam || (contains x MorphBall && (contains x SpaceJumpBoots || contains x GravitySuit)))
        Medium -> contains x IceBeam && (contains x GrappleBeam || contains x MorphBall)
        Hard -> contains x IceBeam && (contains x GrappleBeam || contains x MorphBall || contains x SpaceJumpBoots)
        VeryHard -> contains x IceBeam && (contains x GrappleBeam || contains x MorphBall || contains x SpaceJumpBoots)
        Expert -> contains x IceBeam && (contains x GrappleBeam || contains x MorphBall || contains x SpaceJumpBoots) || (contains x SpaceJumpBoots && tallonFloaty x ids)

fcsItem :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
fcsItem diff x ids =
    case diff of
        Easy -> sj x ids && contains x GravitySuit
        Medium -> sjOrBombs x ids && contains x GravitySuit
        Hard -> True
        VeryHard -> True
        Expert -> True

climbFrigateMvs :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
climbFrigateMvs diff x ids =
    case diff of
        Easy -> sj x ids
        Medium -> sj x ids
        Hard -> sjOrBombs x ids
        VeryHard -> sjOrBombs x ids
        Expert -> sjOrBombs x ids

climbReactorCore :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
climbReactorCore diff x ids =
    case diff of
        Easy -> sj x ids
        Medium -> sj x ids
        Hard -> sj x ids || containsAll x [GravitySuit, MorphBall, MorphBallBomb]
        VeryHard -> sj x ids || containsAll x [GravitySuit, MorphBall, MorphBallBomb]
        Expert -> sj x ids || containsAll x [GravitySuit, MorphBall, MorphBallBomb]

cargoFreightLift :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
cargoFreightLift diff x ids =
    case diff of
        Easy -> containsAll x [WaveBeam, GravitySuit, SpaceJumpBoots]
        Medium -> contains x WaveBeam && ((bombs x ids && not (contains x GravitySuit)) || containsAll x [GravitySuit, SpaceJumpBoots])
        Hard -> contains x WaveBeam && ((bombs x ids && not (contains x GravitySuit)) || containsAll x [GravitySuit, SpaceJumpBoots])
        VeryHard -> contains x WaveBeam && (bombs x ids || containsAll x [GravitySuit, SpaceJumpBoots])
        Expert -> contains x WaveBeam && (bombs x ids || containsAll x [GravitySuit, SpaceJumpBoots])

biohazard :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
biohazard _ x _ = contains x WaveBeam

climbBiohazard :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
climbBiohazard diff x ids =
    case diff of
        Easy -> containsAll x [GravitySuit, SpaceJumpBoots]
        Medium -> sj x ids || (contains x GravitySuit && bombs x ids)
        Hard -> sj x ids || contains x GravitySuit || bombs x ids
        VeryHard -> sj x ids || contains x GravitySuit || bombs x ids
        Expert -> sj x ids || contains x GravitySuit || bombs x ids

biotech :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
biotech diff x ids =
    case diff of
        Easy -> containsAll x [GravitySuit, SpaceJumpBoots, WaveBeam]
        Medium -> contains x WaveBeam && (sj x ids || (contains x GravitySuit && bombs x ids))
        Hard -> contains x WaveBeam
        VeryHard -> contains x WaveBeam
        Expert -> contains x WaveBeam

biotechReverse :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
biotechReverse diff x ids =
    case diff of
        Easy -> containsAll x [GravitySuit, SpaceJumpBoots]
        Medium -> sj x ids || (contains x GravitySuit && bombs x ids)
        Hard -> sjOrBombs x ids
        VeryHard -> sjOrBombs x ids -- You probably don't need bombs but you will likely have them anyway
        Expert -> sjOrBombs x ids

lgUnderWater :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
lgUnderWater diff x ids =
    case diff of
        Easy -> containsAll x [MorphBall, BoostBall, PowerBomb, SpaceJumpBoots]
        Medium -> containsAll x [MorphBall, BoostBall, PowerBomb] && sjOrBombs x ids
        Hard -> containsAll x [MorphBall, BoostBall, PowerBomb] && sjOrBombs x ids
        VeryHard -> pb x ids && sjOrBombs x ids
        Expert -> pb x ids && sjOrBombs x ids

hydroTunnel :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
hydroTunnel diff x ids =
    case diff of
        Easy -> containsAll x [GravitySuit, MorphBall, MorphBallBomb]
        Medium -> containsAll x [GravitySuit, MorphBall, MorphBallBomb]
        Hard -> containsAll x [GravitySuit, MorphBall, MorphBallBomb] || boost x ids
        VeryHard -> containsAll x [GravitySuit, MorphBall, MorphBallBomb] || boost x ids
        Expert -> containsAll x [GravitySuit, MorphBall, MorphBallBomb] || boost x ids

gthClimb :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
gthClimb diff x _ =
    case diff of
        Easy -> containsAll x [SpaceJumpBoots, BoostBall, MorphBall]
        Medium -> containsAll x [SpaceJumpBoots, BoostBall, MorphBall]
        Hard -> containsAll x [SpaceJumpBoots, MorphBall] && containsAny x [BoostBall, MorphBallBomb]
        VeryHard -> containsAll x [SpaceJumpBoots, MorphBall] && containsAny x [BoostBall, MorphBallBomb]
        Expert -> contains x MorphBall && containsAny x [BoostBall, MorphBallBomb]

bars :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
bars diff x ids =
    case diff of
        Easy -> False
        Medium -> False
        Hard -> bombs x ids
        VeryHard -> bombs x ids
        Expert -> bombs x ids

lifeGroveTunnel :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
lifeGroveTunnel diff x _ =
    case diff of
        Easy -> containsAll x [PowerBomb, MorphBall, BoostBall]
        Medium -> containsAll x [PowerBomb, MorphBall, BoostBall]
        Hard -> containsAll x [PowerBomb, MorphBall, BoostBall]
        VeryHard -> containsAll x [PowerBomb, MorphBall] && containsAny x [MorphBallBomb, BoostBall]
        Expert -> containsAll x [PowerBomb, MorphBall] && containsAny x [MorphBallBomb, BoostBall]

lifeGroveTunnelItem :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
lifeGroveTunnelItem diff x _ =
    case diff of
        Easy -> containsAll x [PowerBomb, MorphBall, BoostBall, MorphBallBomb]
        Medium -> containsAll x [PowerBomb, MorphBall, BoostBall, MorphBallBomb]
        Hard -> containsAll x [PowerBomb, MorphBall, BoostBall, MorphBallBomb]
        VeryHard -> containsAll x [PowerBomb, MorphBall, MorphBallBomb]
        Expert -> containsAll x [PowerBomb, MorphBall, MorphBallBomb]

lifeGroveSw :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
lifeGroveSw diff x ids =
    case diff of
        Easy -> False
        Medium -> False
        Hard -> pb x ids && sj x ids
        VeryHard -> pb x ids && sj x ids
        Expert -> pb x ids && sj x ids

gthSpiderTrack :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
gthSpiderTrack diff x ids =
    case diff of
        Easy -> containsAll x [SpiderBall, MorphBall, SpaceJumpBoots]
        Medium -> containsAll x [SpiderBall, MorphBall, SpaceJumpBoots]
        Hard -> containsAll x [SpiderBall, MorphBall, SpaceJumpBoots]
        VeryHard -> containsAll x [SpiderBall, MorphBall, SpaceJumpBoots]
        Expert -> containsAll x [SpiderBall, MorphBall, SpaceJumpBoots] || bombs x ids

gtcEnter :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
gtcEnter diff x ids =
    case diff of
        Easy -> sj x ids
        Medium -> sj x ids
        Hard -> sj x ids
        VeryHard -> sjOrBombs x ids
        Expert -> sjOrBombs x ids

gtcSw :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
gtcSw diff x ids =
    case diff of
        Easy -> False
        Medium -> False
        Hard -> False
        VeryHard -> bombs x ids
        Expert -> bombs x ids

-- You can get past Connection Elevator to Deck Beta without gravity by wallcrawling
wallcrawlIntoFrigate :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
wallcrawlIntoFrigate diff x ids =
    case diff of
        Easy -> False
        Medium -> False
        Hard -> False
        VeryHard -> bombs x ids
        Expert -> bombs x ids

-- Chozo Predicates
mainPipe :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
mainPipe diff x ids =
    case diff of
        Easy -> sj x ids || boost x ids
        Medium -> sj x ids || boost x ids
        Hard -> sj x ids || boost x ids
        VeryHard -> sj x ids || boost x ids || bombs x ids
        Expert -> sj x ids || boost x ids || bombs x ids

mainPlazaGrappleLedge :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
mainPlazaGrappleLedge diff x _ =
    case diff of
        Easy -> contains x GrappleBeam
        Medium -> containsAny x [GrappleBeam, SpaceJumpBoots]
        Hard -> containsAny x [GrappleBeam, SpaceJumpBoots]
        VeryHard -> containsAny x [GrappleBeam, SpaceJumpBoots]
        Expert -> containsAny x [GrappleBeam, SpaceJumpBoots]

mainPlazaLedge :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
mainPlazaLedge diff x _ =
    case diff of
        Easy -> False
        Medium -> contains x SpaceJumpBoots
        Hard -> contains x SpaceJumpBoots
        VeryHard -> contains x SpaceJumpBoots
        Expert -> contains x SpaceJumpBoots

mainPlazaSw :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
mainPlazaSw diff x ids =
    case diff of
        Easy -> False
        Medium -> False
        Hard -> sj x ids && bombs x ids
        VeryHard -> sj x ids && bombs x ids
        Expert -> sj x ids && bombs x ids

ruinedFountainItem :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
ruinedFountainItem diff x ids =
    case diff of
        Easy -> Data.Set.member SunchamberFlaahgra ids && contains x SpiderBall
        Medium -> Data.Set.member SunchamberFlaahgra ids && contains x SpiderBall
        Hard -> (Data.Set.member SunchamberFlaahgra ids || sj x ids) && contains x SpiderBall
        VeryHard -> (Data.Set.member SunchamberFlaahgra ids || sj x ids) && contains x SpiderBall
        Expert -> (Data.Set.member SunchamberFlaahgra ids || sj x ids) && contains x SpiderBall

leaveRuinedFountainItem :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
leaveRuinedFountainItem _ x ids = Data.Set.member RuinedFountain ids && spider x ids

towerChamber :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
towerChamber diff x _ =
    case diff of
        Easy -> containsAll x [GravitySuit, SpaceJumpBoots, WaveBeam]
        Medium -> containsAll x [SpaceJumpBoots, WaveBeam]
        Hard -> contains x WaveBeam
        VeryHard -> contains x WaveBeam
        Expert -> contains x WaveBeam

rsHalf :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
rsHalf diff x ids =
    case diff of
        Easy -> boost x ids
        Medium -> boost x ids || containsAll x [SpaceJumpBoots, MorphBall]
        Hard -> boost x ids || containsAll x [SpaceJumpBoots, MorphBall]
        VeryHard -> contains x MorphBall
        Expert -> contains x MorphBall

tolAccess :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
tolAccess diff x _ =
    case diff of
        Easy -> containsAll x [MorphBall, BoostBall, SpiderBall, WaveBeam]
        Medium -> containsAll x [MorphBall, BoostBall, SpiderBall, WaveBeam] || containsAll x [SpaceJumpBoots, WaveBeam]
        Hard -> containsAll x [MorphBall, BoostBall, SpiderBall, WaveBeam] || containsAll x [SpaceJumpBoots, WaveBeam]
        VeryHard -> contains x WaveBeam
        Expert -> contains x WaveBeam

towerOfLight :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
towerOfLight diff x ids =
    case diff of
        Easy -> containsCount 8 Missile x && sj x ids
        Medium -> containsCount 8 Missile x && sj x ids
        Hard -> sj x ids
        VeryHard -> (containsCount 8 Missile x && bombs x ids) || sj x ids
        Expert -> (containsCount 8 Missile x && bombs x ids) || sj x ids

crossMagmaPool :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
crossMagmaPool diff x ids =
    case diff of
        Easy -> heatResist x ids && containsAll x [GrappleBeam, WaveBeam]
        Medium -> heatResist x ids && containsAll x [GrappleBeam, WaveBeam]
        Hard -> ((heatResist x ids && contains x GrappleBeam) || sj x ids) && wave x ids
        VeryHard -> ((heatResist x ids && contains x GrappleBeam) || sj x ids) && wave x ids
        Expert -> ((heatResist x ids && contains x GrappleBeam) || sj x ids) && wave x ids

magmaPoolItem :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
magmaPoolItem diff x ids =
    case diff of
        Easy -> heatResist x ids && containsAll x [GrappleBeam, MorphBall, PowerBomb]
        Medium -> heatResist x ids && containsAll x [GrappleBeam, MorphBall, PowerBomb]
        Hard -> heatResist x ids && ((containsAny x [GrappleBeam, SpaceJumpBoots] && pb x ids) || boost x ids)
        VeryHard ->
            (grapple x ids && pb x ids && heatResist x ids) ||
            (sj x ids && pb x ids && (contains x EnergyTank || heatResist x ids)) || (boost x ids && (containsCount 5 EnergyTank x || heatResist x ids))
        Expert -> (grapple x ids && pb x ids && heatResist x ids) || (sj x ids && pb x ids) || (boost x ids && (containsCount 5 EnergyTank x || heatResist x ids))

tcItem :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
tcItem _ x _ = containsAll x [MorphBall, BoostBall, MorphBallBomb, SpiderBall]

tcTunnel :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
tcTunnel _ x ids = boost x ids && bombs x ids

climbSunTower :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
climbSunTower _ x _ = containsAll x [MorphBall, SpiderBall, MorphBallBomb, Missile, SuperMissile, ChargeBeam]

sunchamberghost :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
sunchamberghost _ _ = Data.Set.member SunchamberFlaahgra

gatheringHallSw :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
gatheringHallSw diff x ids =
    case diff of
        Easy -> False
        Medium -> False
        Hard -> sj x ids && bombs x ids
        VeryHard -> bombs x ids
        Expert -> bombs x ids

wateryHallSw :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
wateryHallSw diff x ids =
    case diff of
        Easy -> False
        Medium -> False
        Hard -> sj x ids && bombs x ids
        VeryHard -> bombs x ids
        Expert -> bombs x ids

wateryHallTraverse :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
wateryHallTraverse _ x _ = containsAll x [MorphBall, MorphBallBomb, Missile]

wateryHallWater :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
wateryHallWater diff x ids =
    case diff of
        Easy -> contains x GravitySuit && (contains x SpaceJumpBoots || bombs x ids)
        Medium -> contains x GravitySuit && (contains x SpaceJumpBoots || bombs x ids)
        Hard -> True
        VeryHard -> True
        Expert -> True

furnaceTraverse :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
furnaceTraverse diff x _ =
    case diff of
        Easy -> containsAll x [MorphBall, MorphBallBomb, SpiderBall]
        Medium -> containsAll x [MorphBall, MorphBallBomb]
        Hard -> containsAll x [MorphBall, MorphBallBomb]
        VeryHard -> containsAll x [MorphBall, MorphBallBomb]
        Expert -> containsAll x [MorphBall, MorphBallBomb]

furnaceItem :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
furnaceItem diff x ids =
    case diff of
        Easy -> containsAll x [MorphBall, MorphBallBomb, SpiderBall, PowerBomb, BoostBall]
        Medium -> containsAll x [MorphBall, MorphBallBomb, SpiderBall, PowerBomb, BoostBall]
        Hard -> containsAll x [MorphBall, MorphBallBomb, SpiderBall] && (containsAll x [PowerBomb, BoostBall] || sj x ids)
        VeryHard -> containsAll x [MorphBall, MorphBallBomb, SpiderBall] && (containsAll x [PowerBomb, BoostBall] || sj x ids)
        Expert -> containsAll x [MorphBall, MorphBallBomb, SpiderBall] && (containsAll x [PowerBomb, BoostBall] || sj x ids)

crosswayInfiniteSpeed :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
crosswayInfiniteSpeed diff x _ = diff == Expert && containsAll x [MorphBall, BoostBall, Missile]

crosswayTraverse :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
crosswayTraverse diff x ids =
    case diff of
        Easy -> containsAll x [MorphBall, BoostBall, Missile]
        Medium -> contains x Missile && (boost x ids || sj x ids)
        Hard -> contains x Missile && (boost x ids || sj x ids)
        VeryHard -> contains x Missile && (boost x ids || sj x ids || bombs x ids)
        Expert -> contains x Missile && (boost x ids || sj x ids || bombs x ids)

crosswayItem :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
crosswayItem diff x ids =
    case diff of
        Easy -> containsAll x [MorphBall, BoostBall, Missile, SuperMissile, ChargeBeam, SpiderBall, MorphBallBomb]
        Medium -> containsAll x [MorphBall, BoostBall, Missile, SuperMissile, ChargeBeam, SpiderBall, MorphBallBomb]
        Hard -> (sj x ids && contains x MorphBall) || containsAll x [MorphBall, BoostBall, Missile, SuperMissile, ChargeBeam, SpiderBall, MorphBallBomb]
        VeryHard -> (sj x ids && contains x MorphBall) || containsAll x [MorphBall, BoostBall, Missile, SuperMissile, ChargeBeam, SpiderBall, MorphBallBomb]
        Expert -> (sj x ids && contains x MorphBall) || containsAll x [MorphBall, BoostBall, Missile, SuperMissile, ChargeBeam, SpiderBall, MorphBallBomb]

hoteWave :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
hoteWave diff x ids =
    case diff of
        Easy -> containsAll x [MorphBall, SpiderBall, MorphBallBomb, WaveBeam]
        Medium -> containsAll x [MorphBall, MorphBallBomb, WaveBeam] && (spider x ids || sj x ids)
        Hard -> containsAll x [MorphBall, MorphBallBomb, WaveBeam] && (spider x ids || sj x ids)
        VeryHard -> containsAll x [MorphBall, MorphBallBomb] -- We don't need to even use the wave slot using an HBJ
        Expert -> containsAll x [MorphBall, MorphBallBomb]

hoteIce :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
hoteIce diff x ids =
    case diff of
        Easy -> containsAll x [MorphBall, SpiderBall, MorphBallBomb, IceBeam]
        Medium -> containsAll x [MorphBall, MorphBallBomb, IceBeam] && (spider x ids || sj x ids)
        Hard -> containsAll x [MorphBall, MorphBallBomb, IceBeam] && (spider x ids || sj x ids)
        VeryHard -> containsAll x [MorphBall, MorphBallBomb, IceBeam]
        Expert -> containsAll x [MorphBall, MorphBallBomb, IceBeam]

hotePlasma :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
hotePlasma diff x ids =
    case diff of
        Easy -> containsAll x [MorphBall, SpiderBall, MorphBallBomb, IceBeam, PlasmaBeam]
        Medium -> containsAll x [MorphBall, MorphBallBomb, IceBeam, PlasmaBeam] && (spider x ids || sj x ids)
        Hard -> containsAll x [MorphBall, MorphBallBomb, IceBeam, PlasmaBeam] && (spider x ids || sj x ids)
        VeryHard -> containsAll x [MorphBall, MorphBallBomb, IceBeam, PlasmaBeam]
        Expert -> containsAll x [MorphBall, MorphBallBomb, IceBeam, PlasmaBeam]

reflectPoolSave :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
reflectPoolSave diff x ids = reflectPoolAntechamber diff x ids && contains x Missile

reflectPoolIceDoor :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
reflectPoolIceDoor diff x ids = reflectPoolAntechamber diff x ids && contains x IceBeam

reflectPoolAntechamber :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
reflectPoolAntechamber diff x ids =
    case diff of
        Easy -> containsAll x [MorphBall, MorphBallBomb, BoostBall]
        Medium -> containsAll x [MorphBall, MorphBallBomb, BoostBall] || sj x ids
        Hard -> containsAll x [MorphBall, MorphBallBomb, BoostBall] || sj x ids
        VeryHard -> bombs x ids || sj x ids
        Expert -> bombs x ids || sj x ids

-- Magmoor Predicates
vmr1Tank :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
vmr1Tank diff x ids =
    case diff of
        Easy -> heatResist x ids
        Medium -> heatResist x ids
        Hard -> heatResist x ids || containsCount 2 EnergyTank x && sj x ids
        VeryHard -> heatResist x ids || (containsCount 1 EnergyTank x && sj x ids) || containsCount 2 EnergyTank x
        Expert -> heatResist x ids || (containsCount 1 EnergyTank x && sj x ids) || containsCount 2 EnergyTank x

vmr2Tank :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
vmr2Tank diff x ids =
    case diff of
        Easy -> heatResist x ids
        Medium -> heatResist x ids
        Hard -> heatResist x ids || containsCount 3 EnergyTank x && sj x ids
        VeryHard -> heatResist x ids || (containsCount 2 EnergyTank x && sj x ids) || containsCount 3 EnergyTank x
        Expert -> heatResist x ids || (containsCount 2 EnergyTank x && sj x ids) || containsCount 3 EnergyTank x

vmr3Tank :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
vmr3Tank diff x ids =
    case diff of
        Easy -> heatResist x ids
        Medium -> heatResist x ids
        Hard -> heatResist x ids || containsCount 4 EnergyTank x && sj x ids
        VeryHard -> heatResist x ids || (containsCount 3 EnergyTank x && sj x ids) || containsCount 4 EnergyTank x
        Expert -> heatResist x ids || (containsCount 3 EnergyTank x && sj x ids) || containsCount 4 EnergyTank x

vmr4Tank :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
vmr4Tank diff x ids =
    case diff of
        Easy -> heatResist x ids
        Medium -> heatResist x ids
        Hard -> heatResist x ids || containsCount 5 EnergyTank x && sj x ids
        VeryHard -> heatResist x ids || (containsCount 4 EnergyTank x && sj x ids) || containsCount 5 EnergyTank x
        Expert -> heatResist x ids || (containsCount 4 EnergyTank x && sj x ids) || containsCount 5 EnergyTank x

heatResistOr8Etanks :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
heatResistOr8Etanks diff x ids =
    case diff of
        Easy -> heatResist x ids
        Medium -> heatResist x ids
        Hard -> heatResist x ids || containsCount 8 EnergyTank x
        VeryHard -> heatResist x ids || containsCount 8 EnergyTank x
        Expert -> heatResist x ids || containsCount 8 EnergyTank x

burningTrailSw :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
burningTrailSw diff x ids =
    case diff of
        Easy -> False
        Medium -> False
        Hard -> False
        VeryHard -> bombs x ids
        Expert -> bombs x ids

magmoorFrontWallcrawl :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
magmoorFrontWallcrawl diff x ids =
    case diff of
        Easy -> False
        Medium -> False
        Hard -> False
        VeryHard -> False
        Expert -> heatResist x ids && bombs x ids

lavaLakeTraversal :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
lavaLakeTraversal diff x ids = vmr4Tank diff x ids && bombs x ids

lavaLakeReverseTraversal :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
lavaLakeReverseTraversal diff x ids = vmr2Tank diff x ids && bombs x ids

lavaLakeItem :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
lavaLakeItem diff x ids =
    case diff of
        Easy -> missile x ids && sj x ids && heatResist x ids
        Medium -> missile x ids && sj x ids && heatResist x ids
        Hard -> missile x ids && (heatResist x ids || (sj x ids && containsCount 2 EnergyTank x))
        VeryHard -> missile x ids && (heatResist x ids || (sj x ids && contains x EnergyTank) || containsCount 2 EnergyTank x)
        Expert -> missile x ids && (heatResist x ids || (sj x ids && contains x EnergyTank) || containsCount 2 EnergyTank x)

pitTunnel :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
pitTunnel diff x ids = vmr2Tank diff x ids && contains x MorphBall

pitTunnelReverse :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
pitTunnelReverse diff x ids = vmr3Tank diff x ids && contains x MorphBall

triclopsPitItem :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
triclopsPitItem diff x ids =
    case diff of
        Easy -> containsAll x [SpaceJumpBoots, Missile] && heatResist x ids
        Medium -> containsAll x [SpaceJumpBoots, Missile] && heatResist x ids
        Hard -> missile x ids && vmr1Tank diff x ids
        VeryHard -> missile x ids && vmr1Tank diff x ids
        Expert -> missile x ids && vmr1Tank diff x ids

storageCavern :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
storageCavern diff x ids = morph x ids && vmr1Tank diff x ids

toTransportTunnelA :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
toTransportTunnelA diff x ids = bombs x ids && vmr1Tank diff x ids

monitorStationClimb :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
monitorStationClimb diff x ids =
    case diff of
        Easy -> heatResist x ids && containsAll x [SpaceJumpBoots, MorphBall, BoostBall]
        Medium -> heatResist x ids && containsAll x [SpaceJumpBoots, MorphBall, BoostBall]
        Hard -> vmr3Tank diff x ids && (sj x ids || bombs x ids)
        VeryHard -> vmr3Tank diff x ids && (sj x ids || bombs x ids)
        Expert -> vmr3Tank diff x ids && (sj x ids || bombs x ids)

warriorShrineTunnel :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
warriorShrineTunnel diff x ids = vmr4Tank diff x ids && pb x ids && bombs x ids

crossTft :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
crossTft diff x ids =
    case diff of
        Easy -> spider x ids && heatResist x ids
        Medium -> spider x ids && heatResist x ids
        Hard -> (spider x ids && heatResist x ids) || sj x ids || (contains x GravitySuit && bombs x ids && containsCount 2 EnergyTank x)
        VeryHard -> (spider x ids && heatResist x ids) || sj x ids || (contains x GravitySuit && bombs x ids && containsCount 2 EnergyTank x)
        Expert -> (spider x ids && heatResist x ids) || sj x ids || (contains x GravitySuit && bombs x ids && containsCount 2 EnergyTank x)

crossTftReverse :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
crossTftReverse diff x ids =
    case diff of
        Easy -> spider x ids && heatResist x ids
        Medium -> (spider x ids && heatResist x ids) || sj x ids || (bombs x ids && containsCount 3 EnergyTank x)
        Hard -> (spider x ids && heatResist x ids) || sj x ids || (bombs x ids && containsCount 2 EnergyTank x)
        VeryHard -> heatResist x ids || sj x ids || containsCount 2 EnergyTank x
        Expert -> heatResist x ids || sj x ids || containsCount 2 EnergyTank x

crossTwinFires :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
crossTwinFires diff x ids =
    case diff of
        Easy -> sj x ids && contains x WaveBeam
        Medium -> sj x ids && contains x WaveBeam
        Hard -> sj x ids && contains x WaveBeam
        VeryHard -> (sj x ids || missile x ids) && contains x WaveBeam
        Expert -> (sj x ids || missile x ids) && contains x WaveBeam

crossNorthCoreTunnel :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
crossNorthCoreTunnel diff x ids =
    case diff of
        Easy -> containsAll x [Missile, SpaceJumpBoots, WaveBeam]
        Medium -> containsAll x [Missile, SpaceJumpBoots, WaveBeam]
        Hard -> containsAll x [SpaceJumpBoots, WaveBeam]
        VeryHard -> contains x WaveBeam && (missile x ids || sj x ids)
        Expert -> contains x WaveBeam && (missile x ids || sj x ids)

workstationTunnel :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
workstationTunnel _ x _ = containsAll x [IceBeam, PowerBomb, MorphBall]

workstationItem :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
workstationItem _ x _ = containsAll x [MorphBall, WaveBeam]

workstationWaveDoor :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
workstationWaveDoor _ x ids = sjOrBombs x ids && contains x WaveBeam

workstationSw :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
workstationSw diff x ids =
    case diff of
        Easy -> False
        Medium -> False
        Hard -> False
        VeryHard -> sj x ids
        Expert -> sj x ids || (ice x ids && bombs x ids)

geoCore :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
geoCore diff x _ =
    case diff of
        Easy -> containsAll x [SpaceJumpBoots, GrappleBeam, SpiderBall, MorphBall, MorphBallBomb, BoostBall, IceBeam]
        Medium -> containsAll x [SpaceJumpBoots, GrappleBeam, SpiderBall, MorphBall, MorphBallBomb, BoostBall, IceBeam]
        Hard -> containsAll x [SpaceJumpBoots, MorphBall, MorphBallBomb, BoostBall, IceBeam]
        VeryHard -> containsAll x [SpaceJumpBoots, MorphBall, MorphBallBomb, BoostBall, IceBeam]
        Expert -> containsAll x [SpaceJumpBoots, MorphBall, MorphBallBomb, IceBeam]

-- Phendrana Predicates
iceBarrier :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
iceBarrier _ x _ = containsAny x [Missile, ChargeBeam]

shorelinesTower :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
shorelinesTower diff x ids =
    case diff of
        Easy -> containsAll x [MorphBall, SpaceJumpBoots, SpiderBall, ChargeBeam, SuperMissile, Missile]
        Medium -> containsAll x [MorphBall, SpaceJumpBoots, SpiderBall, ChargeBeam, SuperMissile, Missile]
        Hard -> containsAll x [MorphBall, SpaceJumpBoots, SpiderBall, ChargeBeam, SuperMissile, Missile]
        VeryHard -> containsAll x [MorphBall, SpiderBall, ChargeBeam, SuperMissile, Missile] && sjOrBombs x ids
        Expert -> containsAll x [MorphBall, SpiderBall, ChargeBeam, SuperMissile, Missile] && sjOrBombs x ids

shorelinesItem :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
shorelinesItem diff x ids =
    case diff of
        Easy -> plasma x ids
        Medium -> plasma x ids
        Hard -> plasma x ids || (wave x ids && boost x ids && sj x ids && bombs x ids) -- Infinite Speed
        VeryHard -> plasma x ids || (wave x ids && boost x ids && sj x ids && bombs x ids)
        Expert -> plasma x ids || (wave x ids && boost x ids && sj x ids && bombs x ids)

iceTempleClimb :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
iceTempleClimb diff x _ =
    case diff of
        Easy -> containsAll x [SpaceJumpBoots, MorphBall, MorphBallBomb, Missile]
        Medium -> containsAll x [SpaceJumpBoots, MorphBall, MorphBallBomb, Missile]
        Hard -> containsAll x [MorphBall, MorphBallBomb, Missile]
        VeryHard -> containsAll x [MorphBall, MorphBallBomb, Missile]
        Expert -> containsAll x [MorphBall, MorphBallBomb, Missile]

iceTempleItem :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
iceTempleItem diff x ids =
    case diff of
        Easy -> containsAll x [SpaceJumpBoots, MorphBall, MorphBallBomb, PlasmaBeam]
        Medium -> containsAll x [SpaceJumpBoots, MorphBall, MorphBallBomb, PlasmaBeam]
        Hard -> containsAll x [MorphBall, MorphBallBomb, PlasmaBeam] || (sj x ids && boost x ids && bombs x ids && missile x ids) -- Infinite Speed
        VeryHard -> containsAll x [MorphBall, MorphBallBomb, PlasmaBeam] || (sj x ids && boost x ids && bombs x ids && missile x ids)
        Expert -> containsAll x [MorphBall, MorphBallBomb, PlasmaBeam] || (sj x ids && boost x ids && bombs x ids && missile x ids)

climbShorelines :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
climbShorelines diff x ids =
    case diff of
        Easy -> sj x ids
        Medium -> sj x ids
        Hard -> sj x ids
        VeryHard -> sjOrBombs x ids
        Expert -> sjOrBombs x ids

ireSpiderTrack :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
ireSpiderTrack diff x ids =
    case diff of
        Easy -> spider x ids
        Medium -> spider x ids
        Hard -> spider x ids
        VeryHard -> spider x ids || bombs x ids
        Expert -> spider x ids || bombs x ids

irwDoor :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
irwDoor diff x ids =
    case diff of
        Easy -> containsAll x [SpaceJumpBoots, WaveBeam]
        Medium -> containsAll x [SpaceJumpBoots, WaveBeam]
        Hard -> containsAll x [SpaceJumpBoots, WaveBeam]
        VeryHard -> sjOrBombs x ids && wave x ids
        Expert -> sjOrBombs x ids && wave x ids

irwItem :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
irwItem diff x _ =
    case diff of
        Easy -> containsAll x [SpaceJumpBoots, Missile, PlasmaBeam]
        Medium -> containsAll x [SpaceJumpBoots, Missile, PlasmaBeam]
        Hard -> containsAll x [Missile, PlasmaBeam]
        VeryHard -> containsAll x [Missile, PlasmaBeam]
        Expert -> containsAll x [Missile, PlasmaBeam]

irwSw :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
irwSw diff x ids =
    case diff of
        Easy -> False
        Medium -> False
        Hard -> False
        VeryHard -> bombs x ids
        Expert -> bombs x ids

ruinedCourtyardConduit :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
ruinedCourtyardConduit _ x _ = containsAll x [ChargeBeam, Missile, SuperMissile, WaveBeam]

ruinedCourtyardSave :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
ruinedCourtyardSave _ x _ = containsAll x [SpaceJumpBoots, Missile]

--Might not need bombs if using spider track, but bombs are almost always unrandomized anyway
ruinedCourtyardClimb :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
ruinedCourtyardClimb diff x ids =
    case diff of
        Easy -> (spider x ids && sjOrBombs x ids) || containsAll x [SpaceJumpBoots, MorphBall, BoostBall, MorphBallBomb]
        Medium -> (spider x ids && sjOrBombs x ids) || containsAll x [SpaceJumpBoots, MorphBall, BoostBall, MorphBallBomb]
        Hard -> (spider x ids && sjOrBombs x ids) || sj x ids
        VeryHard -> sjOrBombs x ids
        Expert -> sjOrBombs x ids

ruinedCourtyardSw :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
ruinedCourtyardSw diff x ids =
    case diff of
        Easy -> False
        Medium -> False
        Hard -> sj x ids
        VeryHard -> sj x ids
        Expert -> sj x ids

quarantineTunnel :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
quarantineTunnel _ x _ = containsAll x [MorphBall, WaveBeam]

climbQuarantineCaveEntrance :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
climbQuarantineCaveEntrance diff x ids =
    case diff of
        Easy -> spider x ids
        Medium -> spider x ids
        Hard -> spider x ids || sj x ids
        VeryHard -> spider x ids || sj x ids
        Expert -> spider x ids || sj x ids

climbQuarantineCaveBack :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
climbQuarantineCaveBack diff x ids =
    case diff of
        Easy -> spider x ids
        Medium -> spider x ids || (sj x ids && grapple x ids)
        Hard -> spider x ids || sj x ids || grapple x ids
        VeryHard -> spider x ids || sj x ids || grapple x ids
        Expert -> spider x ids || sj x ids || grapple x ids

quarantineMonitor :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
quarantineMonitor diff x ids =
    case diff of
        Easy -> grapple x ids
        Medium -> grapple x ids
        Hard -> grapple x ids || sj x ids
        VeryHard -> grapple x ids || sj x ids
        Expert -> grapple x ids || sj x ids

phenElevatorClimb :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
phenElevatorClimb diff x ids =
    case diff of
        Easy -> containsAll x [MorphBall, SpiderBall, IceBeam]
        Medium -> containsAll x [MorphBall, SpiderBall, IceBeam]
        Hard -> (spider x ids || sj x ids) && ice x ids
        VeryHard -> (spider x ids || sj x ids || bombs x ids) && ice x ids
        Expert -> (spider x ids || sj x ids || bombs x ids) && ice x ids

observatoryClimb :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
observatoryClimb diff x ids =
    case diff of
        Easy -> containsAll x [MorphBall, BoostBall, MorphBallBomb, SpaceJumpBoots]
        Medium -> containsAll x [MorphBall, BoostBall, MorphBallBomb, SpaceJumpBoots]
        Hard -> sj x ids
        VeryHard -> sj x ids
        Expert -> sj x ids

observatorySave :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
observatorySave diff x ids =
    case diff of
        Easy -> sj x ids && contains x Missile
        Medium -> sj x ids && contains x Missile
        Hard -> sj x ids && contains x Missile
        VeryHard -> contains x Missile
        Expert -> contains x Missile

observatoryItem :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
observatoryItem diff x ids =
    case diff of
        Easy -> containsAll x [MorphBall, BoostBall, MorphBallBomb, SpaceJumpBoots]
        Medium -> sj x ids
        Hard -> sj x ids
        VeryHard -> sjOrBombs x ids
        Expert -> sjOrBombs x ids

controlTowerItem :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
controlTowerItem diff x ids =
    case diff of
        Easy -> containsAll x [MorphBall, PlasmaBeam, Missile] && sjOrBombs x ids
        Medium -> containsAll x [MorphBall, PlasmaBeam, Missile] && sjOrBombs x ids
        Hard -> (bombs x ids && plasma x ids && missile x ids) || (sj x ids && missile x ids && morph x ids)
        VeryHard -> (bombs x ids && plasma x ids && missile x ids) || (sj x ids && missile x ids && morph x ids)
        Expert -> (bombs x ids && plasma x ids && missile x ids) || (sj x ids && missile x ids && morph x ids)

rlaTrack :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
rlaTrack _ x _ = contains x MorphBall && containsAny x [MorphBallBomb, SpaceJumpBoots]

toStorageCave :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
toStorageCave diff x ids =
    case diff of
        Easy -> containsAll x [SpaceJumpBoots, GrappleBeam, PlasmaBeam, MorphBall, PowerBomb]
        Medium -> containsAll x [SpaceJumpBoots, PlasmaBeam, MorphBall, PowerBomb]
        Hard -> containsAll x [SpaceJumpBoots, PlasmaBeam, MorphBall, PowerBomb]
        VeryHard -> containsAll x [PlasmaBeam, MorphBall, PowerBomb] && sjOrBombs x ids
        Expert -> containsAll x [PlasmaBeam, MorphBall, PowerBomb] && sjOrBombs x ids

fromStorageCave :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
fromStorageCave _ x _ = containsAll x [PlasmaBeam, MorphBall, PowerBomb]

toSecurityCave :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
toSecurityCave diff x ids =
    case diff of
        Easy -> containsAll x [SpaceJumpBoots, GrappleBeam, MorphBall]
        Medium -> containsAll x [SpaceJumpBoots, MorphBall]
        Hard -> containsAll x [SpaceJumpBoots, MorphBall]
        VeryHard -> morph x ids && (sj x ids || (bombs x ids && grapple x ids))
        Expert -> morph x ids && (sj x ids || (bombs x ids && grapple x ids))

phenEdgeLower :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
phenEdgeLower diff x ids =
    case diff of
        Easy -> containsAll x [WaveBeam, GravitySuit, SpaceJumpBoots]
        Medium -> containsAll x [WaveBeam, SpaceJumpBoots]
        Hard -> containsAll x [WaveBeam, SpaceJumpBoots]
        VeryHard -> wave x ids && sjOrBombs x ids
        Expert -> wave x ids && sjOrBombs x ids

frozenPikeBottom :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
frozenPikeBottom diff x ids =
    case diff of
        Easy -> containsAll x [WaveBeam, GravitySuit, SpaceJumpBoots]
        Medium -> wave x ids && sjOrBombs x ids
        Hard -> wave x ids && sjOrBombs x ids
        VeryHard -> wave x ids && sjOrBombs x ids
        Expert -> wave x ids && sjOrBombs x ids

frozenPikeClimb :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
frozenPikeClimb diff x ids =
    case diff of
        Easy -> containsAll x [MorphBall, MorphBallBomb, SpaceJumpBoots]
        Medium -> containsAll x [MorphBall, MorphBallBomb, SpaceJumpBoots]
        Hard -> containsAll x [MorphBall, MorphBallBomb, SpaceJumpBoots]
        VeryHard -> sjOrBombs x ids
        Expert -> sjOrBombs x ids

gravLedge :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
gravLedge diff x ids =
    case diff of
        Easy -> containsAll x [PlasmaBeam, GrappleBeam]
        Medium -> containsAll x [PlasmaBeam, GrappleBeam]
        Hard -> containsAll x [PlasmaBeam, GrappleBeam] || sj x ids
        VeryHard -> containsAll x [PlasmaBeam, GrappleBeam] || sj x ids
        Expert -> containsAll x [PlasmaBeam, GrappleBeam] || sj x ids

climbGravityChamber :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
climbGravityChamber diff x ids =
    case diff of
        Easy -> contains x GravitySuit && sjOrBombs x ids
        Medium -> contains x GravitySuit && sjOrBombs x ids
        Hard -> contains x GravitySuit && sjOrBombs x ids
        VeryHard -> (contains x GravitySuit && sjOrBombs x ids) || sj x ids
        Expert -> (contains x GravitySuit && sjOrBombs x ids) || sj x ids

gravityChamberToLakeTunnel :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
gravityChamberToLakeTunnel diff x ids = climbGravityChamber diff x ids && contains x WaveBeam

hunterCaveClimb :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
hunterCaveClimb diff x ids =
    case diff of
        Easy -> contains x Missile && (contains x SpaceJumpBoots || bombs x ids)
        Medium -> sjOrBombs x ids
        Hard -> sjOrBombs x ids
        VeryHard -> sjOrBombs x ids
        Expert -> sjOrBombs x ids

hunterCaveUpper :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
hunterCaveUpper diff x ids =
    case diff of
        Easy -> containsAll x [Missile, GrappleBeam]
        Medium -> containsAll x [Missile, GrappleBeam]
        Hard -> sj x ids || containsAll x [Missile, GrappleBeam]
        VeryHard -> sj x ids || containsAll x [Missile, GrappleBeam]
        Expert -> sj x ids || containsAll x [Missile, GrappleBeam]

hunterCaveLower :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
hunterCaveLower _ x ids = contains x Missile && (contains x SpaceJumpBoots || bombs x ids)

frostCaveAccess :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
frostCaveAccess _ x _ = containsAll x [MorphBall, WaveBeam]

frostCaveDoor :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
frostCaveDoor _ x ids = containsAll x [Missile, WaveBeam] && (contains x SpaceJumpBoots || bombs x ids)

frostCaveItem :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
frostCaveItem diff x ids =
    case diff of
        Easy -> containsAll x [GrappleBeam, Missile]
        Medium -> containsAll x [GrappleBeam, Missile]
        Hard -> missile x ids && (sj x ids || grapple x ids)
        VeryHard -> missile x ids && (sj x ids || bombs x ids || grapple x ids)
        Expert -> missile x ids && (sj x ids || bombs x ids || grapple x ids)

frostCaveToTunnel :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
frostCaveToTunnel _ x ids = containsAll x [Missile, WaveBeam, MorphBall] && (contains x SpaceJumpBoots || bombs x ids)

frostCaveSw :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
frostCaveSw diff x ids =
    case diff of
        Easy -> False
        Medium -> False
        Hard -> sj x ids
        VeryHard -> sjOrBombs x ids
        Expert -> sjOrBombs x ids

transportAccessItemOob :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
transportAccessItemOob diff x ids =
    case diff of
        Easy -> False
        Medium -> False
        Hard -> False
        VeryHard -> bombs x ids && boost x ids -- This is annoying but possible with boost
        Expert -> bombs x ids && boost x ids

-- Mines Predicates
quarrySave :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
quarrySave diff x ids =
    case diff of
        Easy -> containsAll x [SpiderBall, MorphBall, WaveBeam]
        Medium -> wave x ids && (spider x ids || sj x ids)
        Hard -> wave x ids && (spider x ids || sj x ids)
        VeryHard -> wave x ids && (spider x ids || sj x ids || bombs x ids)
        Expert -> wave x ids && (spider x ids || sj x ids || bombs x ids)

quarryItem :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
quarryItem diff x ids =
    case diff of
        Easy -> containsAll x [SpaceJumpBoots, WaveBeam, MorphBall, SpiderBall]
        Medium -> containsAll x [SpaceJumpBoots, WaveBeam, MorphBall, SpiderBall]
        Hard -> wave x ids && (spider x ids || sj x ids)
        VeryHard -> wave x ids && (spider x ids || sj x ids)
        Expert -> wave x ids && (spider x ids || sj x ids)

reachWasteDisposal :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
reachWasteDisposal diff x ids =
    case diff of
        Easy -> containsAll x [SpaceJumpBoots, WaveBeam, IceBeam, GrappleBeam]
        Medium -> containsAll x [WaveBeam, IceBeam, GrappleBeam] && sjOrBombs x ids
        Hard -> ice x ids && sjOrBombs x ids
        VeryHard -> ice x ids && sjOrBombs x ids
        Expert -> ice x ids && sjOrBombs x ids

oreProcessingClimb :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
oreProcessingClimb diff x ids =
    case diff of
        Easy -> containsAll x [MorphBall, SpiderBall, MorphBallBomb, IceBeam]
        Medium -> containsAll x [MorphBall, SpiderBall, MorphBallBomb, IceBeam]
        Hard -> ice x ids && (containsAll x [MorphBall, SpiderBall, MorphBallBomb] || sj x ids)
        VeryHard -> ice x ids && (containsAll x [MorphBall, SpiderBall, MorphBallBomb] || sj x ids)
        Expert -> ice x ids && (containsAll x [MorphBall, SpiderBall, MorphBallBomb] || sj x ids)

oreProcessingTop :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
oreProcessingTop diff x ids =
    case diff of
        Easy -> containsAll x [MorphBall, SpiderBall, MorphBallBomb, PowerBomb, IceBeam]
        Medium -> containsAll x [MorphBall, SpiderBall, MorphBallBomb, PowerBomb, IceBeam]
        Hard -> ice x ids && (containsAll x [MorphBall, SpiderBall, MorphBallBomb, PowerBomb] || sj x ids)
        VeryHard -> ice x ids && (containsAll x [MorphBall, SpiderBall, MorphBallBomb, PowerBomb] || sj x ids)
        Expert -> ice x ids && (containsAll x [MorphBall, SpiderBall, MorphBallBomb, PowerBomb] || sj x ids)

oreProcessingCrossTop :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
oreProcessingCrossTop diff x _ =
    case diff of
        Easy -> containsAll x [GrappleBeam, IceBeam]
        Medium -> contains x IceBeam
        Hard -> contains x IceBeam
        VeryHard -> contains x IceBeam
        Expert -> contains x IceBeam

wasteDisposalTraversal :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
wasteDisposalTraversal _ x _ = containsAll x [MorphBall, MorphBallBomb, IceBeam]

shaftClimb1 :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
shaftClimb1 diff x ids =
    case diff of
        Easy -> containsAll x [MorphBall, SpiderBall, IceBeam]
        Medium -> containsAll x [MorphBall, SpiderBall, IceBeam]
        Hard -> ice x ids && (spider x ids || (bombs x ids && sj x ids))
        VeryHard -> ice x ids && (spider x ids || (bombs x ids && sj x ids))
        Expert -> ice x ids && (spider x ids || (bombs x ids && sj x ids))

shaftClimb2 :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
shaftClimb2 diff x ids =
    case diff of
        Easy -> containsAll x [MorphBall, SpiderBall, BoostBall, IceBeam]
        Medium -> containsAll x [MorphBall, SpiderBall, BoostBall, IceBeam]
        Hard -> ice x ids && boost x ids && (spider x ids || sj x ids)
        VeryHard -> ice x ids && boost x ids && (spider x ids || sj x ids)
        Expert -> ice x ids && boost x ids && (spider x ids || sj x ids)

storageDepotABarrier :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
storageDepotABarrier _ x _ = contains x StorageDepotABarrier

securityAccessBSw :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
securityAccessBSw diff x ids =
    case diff of
        Easy -> False
        Medium -> False
        Hard -> False
        VeryHard -> bombs x ids
        Expert -> bombs x ids

maintTunnel :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
maintTunnel _ x _ = containsAll x [MorphBall, IceBeam, PowerBomb]

ppcClimb :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
ppcClimb diff x ids =
    case diff of
        Easy -> containsAll x [MorphBall, SpiderBall, SpaceJumpBoots, IceBeam]
        Medium -> containsAll x [MorphBall, SpiderBall, SpaceJumpBoots, IceBeam]
        Hard -> ice x ids && sj x ids
        VeryHard -> ice x ids && sjOrBombs x ids
        Expert -> ice x ids && sjOrBombs x ids

toMinesElevator :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
toMinesElevator diff x ids =
    case diff of
        Easy -> containsAll x [GrappleBeam, IceBeam]
        Medium -> ice x ids && (grapple x ids || sj x ids)
        Hard -> ice x ids
        VeryHard -> ice x ids
        Expert -> ice x ids

centralDynamoClimb :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
centralDynamoClimb diff x ids =
    case diff of
        Easy -> contains x IceBeam && sj x ids
        Medium -> contains x IceBeam && sj x ids
        Hard -> contains x IceBeam && sj x ids
        VeryHard -> contains x IceBeam && sjOrBombs x ids
        Expert -> contains x IceBeam && sjOrBombs x ids

mqaItem :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
mqaItem diff x ids =
    case diff of
        Easy -> containsAll x [SpaceJumpBoots, XRayVisor, MorphBall, PowerBomb]
        Medium -> containsAll x [SpaceJumpBoots, MorphBall, PowerBomb]
        Hard -> sj x ids
        VeryHard -> sj x ids || (bombs x ids && pb x ids)
        Expert -> sj x ids || (bombs x ids && pb x ids)

mqaTraversal :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
mqaTraversal diff x ids =
    case diff of
        Easy -> containsAll x [SpaceJumpBoots, XRayVisor, MorphBall, SpiderBall, IceBeam]
        Medium -> containsAll x [SpaceJumpBoots, MorphBall, SpiderBall, IceBeam]
        Hard -> ice x ids && sj x ids && (pb x ids || spider x ids)
        VeryHard -> ice x ids && sjOrBombs x ids && (pb x ids || spider x ids)
        Expert -> ice x ids && sjOrBombs x ids && (pb x ids || spider x ids)

ecaItem :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
ecaItem diff x ids =
    case diff of
        Easy -> containsAll x [SpaceJumpBoots, MorphBall, MorphBallBomb]
        Medium -> containsAll x [SpaceJumpBoots, MorphBall, MorphBallBomb]
        Hard -> containsAll x [SpaceJumpBoots, MorphBall, MorphBallBomb]
        VeryHard -> (morph x ids && sj x ids) || bombs x ids
        Expert -> (morph x ids && sj x ids) || bombs x ids

eliteResearchPirate :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
eliteResearchPirate _ x ids = Data.Set.member CentralDynamo ids && pb x ids

eliteResearchTopItem :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
eliteResearchTopItem diff x ids =
    case diff of
        Easy -> containsAll x [SpaceJumpBoots, MorphBall, BoostBall]
        Medium -> containsAll x [SpaceJumpBoots, MorphBall, BoostBall]
        Hard -> containsAll x [SpaceJumpBoots, MorphBall, BoostBall]
        VeryHard -> sjOrBombs x ids
        Expert -> sjOrBombs x ids

eliteResearchDoor :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
eliteResearchDoor diff x ids =
    case diff of
        Easy -> containsAll x [SpaceJumpBoots, MorphBall, BoostBall, IceBeam]
        Medium -> containsAll x [SpaceJumpBoots, MorphBall, BoostBall, IceBeam]
        Hard -> containsAll x [SpaceJumpBoots, MorphBall, BoostBall, IceBeam]
        VeryHard -> sjOrBombs x ids && ice x ids
        Expert -> sjOrBombs x ids && ice x ids

toStorageDepotA :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
toStorageDepotA diff x _ =
    case diff of
        Easy -> containsAll x [WaveBeam, MorphBall, PowerBomb, PlasmaBeam]
        Medium -> containsAll x [MorphBall, PowerBomb, PlasmaBeam]
        Hard -> containsAll x [MorphBall, PowerBomb, PlasmaBeam]
        VeryHard -> containsAll x [MorphBall, PowerBomb, PlasmaBeam]
        Expert -> containsAll x [MorphBall, PowerBomb, PlasmaBeam]

climbFungalHallAccess :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
climbFungalHallAccess diff x ids =
    case diff of
        Easy -> containsAll x [SpaceJumpBoots, PlasmaBeam]
        Medium -> containsAll x [SpaceJumpBoots, PlasmaBeam]
        Hard -> containsAll x [SpaceJumpBoots, PlasmaBeam]
        VeryHard -> sjOrBombs x ids && plasma x ids
        Expert -> sjOrBombs x ids && plasma x ids

fungalHallATraversal :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
fungalHallATraversal diff x ids =
    case diff of
        Easy -> containsAll x [SpaceJumpBoots, GrappleBeam, IceBeam]
        Medium -> containsAll x [SpaceJumpBoots, IceBeam]
        Hard -> containsAll x [SpaceJumpBoots, IceBeam]
        VeryHard -> sjOrBombs x ids && ice x ids
        Expert -> sjOrBombs x ids && ice x ids

miningTunnelTraversal :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
miningTunnelTraversal _ x _ = containsAll x [MorphBall, MorphBallBomb, PlasmaBeam]

-- High difficulties require farming for health. Varia isn't considered. This assumes you only need to survive until you warp.
miningTunnelItem :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
miningTunnelItem diff x ids =
    case diff of
        Easy -> containsAll x [MorphBall, MorphBallBomb, PhazonSuit]
        Medium -> containsAll x [MorphBall, MorphBallBomb, PhazonSuit]
        Hard -> containsAll x [MorphBall, MorphBallBomb] && (contains x PhazonSuit || (contains x GravitySuit && containsCount 10 EnergyTank x && boost x ids))
        VeryHard ->
            containsAll x [MorphBall, MorphBallBomb] &&
            (contains x PhazonSuit || (((contains x GravitySuit && containsCount 6 EnergyTank x) || containsCount 10 EnergyTank x) && boost x ids))
        Expert -> containsAll x [MorphBall, MorphBallBomb] && (contains x PhazonSuit || (containsCount 6 EnergyTank x && boost x ids))

quarantineAccessBTraversal :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
quarantineAccessBTraversal diff x ids =
    case diff of
        Easy -> containsAll x [SpaceJumpBoots, PlasmaBeam]
        Medium -> plasma x ids
        Hard -> plasma x ids
        VeryHard -> plasma x ids
        Expert -> plasma x ids

fungalHallBTraversal :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
fungalHallBTraversal diff x ids =
    case diff of
        Easy -> containsAll x [SpaceJumpBoots, GrappleBeam, PlasmaBeam]
        Medium -> containsAll x [SpaceJumpBoots, PlasmaBeam]
        Hard -> containsAll x [SpaceJumpBoots, PlasmaBeam]
        VeryHard -> sjOrBombs x ids && plasma x ids
        Expert -> sjOrBombs x ids && plasma x ids

mqbTraversal :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
mqbTraversal diff x ids =
    case diff of
        Easy -> containsAll x [SpiderBall, MorphBall, SpaceJumpBoots, GrappleBeam]
        Medium -> containsAll x [SpiderBall, MorphBall, SpaceJumpBoots]
        Hard -> containsAll x [SpiderBall, MorphBall, GrappleBeam] || sj x ids
        VeryHard -> containsAll x [SpiderBall, MorphBall, GrappleBeam] || sjOrBombs x ids
        Expert -> containsAll x [SpiderBall, MorphBall, GrappleBeam] || sjOrBombs x ids

ppcBottomClimb :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
ppcBottomClimb diff x ids =
    case diff of
        Easy -> containsAll x [SpaceJumpBoots, PlasmaBeam, SpiderBall, MorphBall]
        Medium -> containsAll x [SpaceJumpBoots, PlasmaBeam, SpiderBall, MorphBall]
        Hard -> containsAll x [SpaceJumpBoots, PlasmaBeam]
        VeryHard -> sjOrBombs x ids && plasma x ids
        Expert -> sjOrBombs x ids && plasma x ids

eliteQuarters :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
eliteQuarters _ x _ = contains x XRayVisor

eliteQuartersPlasma :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
eliteQuartersPlasma diff x ids = contains x PlasmaBeam && eliteQuarters diff x ids

mqbBackClimb :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
mqbBackClimb diff x ids =
    case diff of
        Easy -> containsAll x [SpaceJumpBoots, PlasmaBeam]
        Medium -> plasma x ids && sjOrBombs x ids
        Hard -> plasma x ids && sjOrBombs x ids
        VeryHard -> plasma x ids && sjOrBombs x ids
        Expert -> plasma x ids && sjOrBombs x ids

mqbSw :: Difficulty -> Map ItemName Int -> Set ItemId -> Bool
mqbSw diff x ids =
    case diff of
        Easy -> False
        Medium -> False
        Hard -> sj x ids && bombs x ids
        VeryHard -> bombs x ids
        Expert -> bombs x ids
