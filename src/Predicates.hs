module Predicates where

import Node
import Util

import Data.Map (Map)
import qualified Data.Map as Map

-- Basic Predicates
noReq :: Map ItemName Int -> Bool
noReq _ = True 

blocked :: Map ItemName Int -> Bool
blocked _ = False

complete :: Map ItemName Int -> Bool
complete = containsCount 12 Artifact

morph :: Map ItemName Int -> Bool
morph x = contains x MorphBall

sj :: Map ItemName Int -> Bool
sj x = contains x SpaceJumpBoots

sjOrBombs :: Map ItemName Int -> Bool
sjOrBombs x = contains x SpaceJumpBoots || bombs x

missile :: Map ItemName Int -> Bool
missile x = contains x Missile

bombs :: Map ItemName Int -> Bool
bombs x = containsAll x [MorphBall, MorphBallBomb]

pb :: Map ItemName Int -> Bool
pb x = containsAll x [PowerBomb, MorphBall]

boost :: Map ItemName Int -> Bool
boost x = containsAll x [MorphBall, BoostBall]

spider :: Map ItemName Int -> Bool
spider x = containsAll x [MorphBall, SpiderBall]

waveIce :: Map ItemName Int -> Bool
waveIce x = containsAll x [WaveBeam, PlasmaBeam]

grapple :: Map ItemName Int -> Bool
grapple x = contains x GrappleBeam

boostBombs :: Map ItemName Int -> Bool
boostBombs x = containsAll x [MorphBall, BoostBall, MorphBallBomb]

morphMissile :: Map ItemName Int -> Bool
morphMissile x = containsAll x [MorphBall, Missile]

bombsPbs :: Map ItemName Int -> Bool
bombsPbs x = containsAll x [MorphBall, MorphBallBomb, PowerBomb]

wave :: Map ItemName Int -> Bool
wave x = contains x WaveBeam

ice :: Map ItemName Int -> Bool
ice x = contains x IceBeam

plasma :: Map ItemName Int -> Bool
plasma x = contains x PlasmaBeam

spiderIce :: Map ItemName Int -> Bool
spiderIce x = containsAll x [IceBeam, MorphBall, SpiderBall]

supers :: Map ItemName Int -> Bool
supers x = containsAll x [Missile, SuperMissile, ChargeBeam]

gravSpace :: Map ItemName Int -> Bool
gravSpace x = containsAll x [GravitySuit, SpaceJumpBoots]

wavePb :: Map ItemName Int -> Bool
wavePb x = wave x && pb x

heatResist :: Map ItemName Int -> Bool
heatResist x  = containsAny x [VariaSuit, GravitySuit, PhazonSuit]

floaty :: Map ItemName Int -> Bool
floaty x = not $ contains x GravitySuit

-- A bit of an obscure trick, you can use infinite speed in landing site to unload the room
tallonFloaty :: Map ItemName Int -> Bool
tallonFloaty x = boost x && floaty x

mainQuarryBarrierIce :: Map ItemName Int -> Bool
mainQuarryBarrierIce x = containsAll x [MainQuarryBarriers, IceBeam]

mainQuarryBarrierWave :: Map ItemName Int -> Bool
mainQuarryBarrierWave x = containsAll x [MainQuarryBarriers, WaveBeam]

chozoIceTempleBarrier :: Map ItemName Int -> Bool
chozoIceTempleBarrier x = contains x ChozoIceTempleBarrier

wallcrawl :: Difficulty -> Map ItemName Int -> Bool
wallcrawl diff x = case diff of 
    Easy -> False
    Medium -> False
    Hard -> bombs x
    VeryHard -> bombs x
    Expert -> bombs x

longWallcrawl :: Difficulty -> Map ItemName Int -> Bool
longWallcrawl diff x = bombs x && diff == Expert

-- Tallon Predicates
sjf :: Difficulty -> Map ItemName Int -> Bool
sjf diff x = case diff of 
    Easy -> sj x
    Medium -> sj x
    Hard -> True
    VeryHard -> True
    Expert -> True

tallonCanyonSw :: Difficulty -> Map ItemName Int -> Bool
tallonCanyonSw diff x = case diff of 
    Easy -> False
    Medium -> False
    Hard -> False
    VeryHard -> boost x && bombs x
    Expert -> bombs x

rootCaveItem :: Difficulty -> Map ItemName Int -> Bool
rootCaveItem diff x = case diff of 
    Easy -> containsAll x [SpaceJumpBoots, GrappleBeam, XRayVisor]
    Medium -> containsAll x [SpaceJumpBoots, GrappleBeam]
    Hard -> contains x SpaceJumpBoots
    VeryHard -> contains x SpaceJumpBoots
    Expert -> contains x SpaceJumpBoots

arbor :: Difficulty -> Map ItemName Int -> Bool
arbor diff x = case diff of 
    Easy -> containsAll x [SpaceJumpBoots, GrappleBeam, XRayVisor, PlasmaBeam]
    Medium -> containsAll x [SpaceJumpBoots, GrappleBeam, PlasmaBeam]
    Hard -> containsAll x [SpaceJumpBoots, PlasmaBeam]
    VeryHard -> contains x PlasmaBeam
    Expert -> contains x PlasmaBeam

fcsClimb :: Difficulty -> Map ItemName Int -> Bool
fcsClimb diff x = case diff of 
    Easy -> False
    Medium -> contains x SpaceJumpBoots && contains x IceBeam
    Hard -> contains x SpaceJumpBoots && contains x IceBeam
    VeryHard -> sjOrBombs x && contains x IceBeam
    Expert -> sjOrBombs x && contains x IceBeam

frigatePowerDoor :: Difficulty -> Map ItemName Int -> Bool
frigatePowerDoor diff x = case diff of 
    Easy -> contains x FrigatePowerDoor
    Medium -> contains x FrigatePowerDoor
    Hard -> contains x FrigatePowerDoor
    VeryHard -> bombs x || contains x FrigatePowerDoor
    Expert -> bombs x || contains x FrigatePowerDoor

fcsEntry :: Difficulty -> Map ItemName Int -> Bool
fcsEntry diff x = case diff of 
    Easy -> contains x IceBeam && (contains x GrappleBeam || (contains x MorphBall && (contains x SpaceJumpBoots || contains x GravitySuit)))
    Medium -> contains x IceBeam && (contains x GrappleBeam || contains x MorphBall)
    Hard -> contains x IceBeam && (contains x GrappleBeam || contains x MorphBall || contains x SpaceJumpBoots)
    VeryHard -> contains x IceBeam && (contains x GrappleBeam || contains x MorphBall || contains x SpaceJumpBoots)
    Expert -> contains x IceBeam && (contains x GrappleBeam || contains x MorphBall || contains x SpaceJumpBoots) || (contains x SpaceJumpBoots && tallonFloaty x)
    
fcsItem :: Difficulty -> Map ItemName Int -> Bool
fcsItem diff x = case diff of 
    Easy -> sj x && contains x GravitySuit
    Medium -> sjOrBombs x && contains x GravitySuit
    Hard -> True
    VeryHard -> True
    Expert -> True
    
climbFrigateMvs :: Difficulty -> Map ItemName Int -> Bool
climbFrigateMvs diff x = case diff of
    Easy -> sj x
    Medium -> sj x
    Hard -> sjOrBombs x
    VeryHard -> sjOrBombs x
    Expert -> sjOrBombs x

climbReactorCore :: Difficulty -> Map ItemName Int -> Bool
climbReactorCore diff x = case diff of
    Easy -> sj x
    Medium -> sj x
    Hard -> sj x || containsAll x [GravitySuit, MorphBall, MorphBallBomb]
    VeryHard -> sj x || containsAll x [GravitySuit, MorphBall, MorphBallBomb]
    Expert -> sj x || containsAll x [GravitySuit, MorphBall, MorphBallBomb]

cargoFreightLift :: Difficulty -> Map ItemName Int -> Bool
cargoFreightLift diff x = case diff of 
    Easy -> containsAll x [WaveBeam, GravitySuit, SpaceJumpBoots]
    Medium -> contains x WaveBeam && (bombs x || containsAll x [GravitySuit, SpaceJumpBoots])
    Hard -> contains x WaveBeam && (bombs x || containsAll x [GravitySuit, SpaceJumpBoots])
    VeryHard -> contains x WaveBeam && (bombs x || containsAll x [GravitySuit, SpaceJumpBoots])
    Expert -> contains x WaveBeam && (bombs x || containsAll x [GravitySuit, SpaceJumpBoots])

biohazard :: Difficulty -> Map ItemName Int -> Bool
biohazard _ x = contains x WaveBeam

climbBiohazard :: Difficulty -> Map ItemName Int -> Bool
climbBiohazard diff x = case diff of 
    Easy -> containsAll x [GravitySuit, SpaceJumpBoots]
    Medium -> sj x || (contains x GravitySuit && bombs x)
    Hard -> sj x || contains x GravitySuit || bombs x
    VeryHard -> sj x ||  contains x GravitySuit || bombs x
    Expert -> sj x || contains x GravitySuit || bombs x

biotech :: Difficulty -> Map ItemName Int -> Bool
biotech diff x = case diff of 
    Easy -> containsAll x [GravitySuit, SpaceJumpBoots, WaveBeam]
    Medium -> contains x WaveBeam && (sj x || (contains x GravitySuit && bombs x))
    Hard -> contains x WaveBeam
    VeryHard -> contains x WaveBeam
    Expert -> contains x WaveBeam

biotechReverse :: Difficulty -> Map ItemName Int -> Bool
biotechReverse diff x = case diff of 
    Easy -> containsAll x [GravitySuit, SpaceJumpBoots]
    Medium -> sj x || (contains x GravitySuit && bombs x)
    Hard -> sjOrBombs x
    VeryHard -> sjOrBombs x -- You probably don't need bombs but you will likely have them anyway
    Expert -> sjOrBombs x

lgUnderWater :: Difficulty -> Map ItemName Int -> Bool
lgUnderWater diff x = case diff of 
    Easy -> containsAll x [MorphBall, BoostBall, PowerBomb, SpaceJumpBoots]
    Medium -> containsAll x [MorphBall, BoostBall, PowerBomb] && sjOrBombs x
    Hard -> containsAll x [MorphBall, BoostBall, PowerBomb] && sjOrBombs x
    VeryHard -> pb x && sjOrBombs x
    Expert -> pb x && sjOrBombs x

hydroTunnel ::  Difficulty -> Map ItemName Int -> Bool
hydroTunnel diff x = case diff of 
    Easy -> containsAll x [GravitySuit, MorphBall, MorphBallBomb]
    Medium -> containsAll x [GravitySuit, MorphBall, MorphBallBomb]
    Hard -> containsAll x [GravitySuit, MorphBall, MorphBallBomb] || boost x
    VeryHard -> containsAll x [GravitySuit, MorphBall, MorphBallBomb] || boost x
    Expert -> containsAll x [GravitySuit, MorphBall, MorphBallBomb] || boost x

gthClimb :: Difficulty -> Map ItemName Int -> Bool
gthClimb diff x = case diff of 
    Easy -> containsAll x [SpaceJumpBoots, BoostBall, MorphBall]
    Medium -> containsAll x [SpaceJumpBoots, BoostBall, MorphBall]
    Hard -> containsAll x [SpaceJumpBoots, MorphBall] && containsAny x [BoostBall, MorphBallBomb]
    VeryHard -> containsAll x [SpaceJumpBoots, MorphBall] && containsAny x [BoostBall, MorphBallBomb]
    Expert -> contains x MorphBall && containsAny x [BoostBall, MorphBallBomb]

bars :: Difficulty -> Map ItemName Int -> Bool
bars diff x = case diff of 
    Easy -> False
    Medium -> False
    Hard -> bombs x
    VeryHard -> bombs x
    Expert -> bombs x

lifeGroveTunnel :: Difficulty -> Map ItemName Int -> Bool
lifeGroveTunnel diff x = case diff of 
    Easy -> containsAll x [PowerBomb, MorphBall, BoostBall]
    Medium -> containsAll x [PowerBomb, MorphBall, BoostBall]
    Hard -> containsAll x [PowerBomb, MorphBall, BoostBall]
    VeryHard -> containsAll x [PowerBomb, MorphBall] && containsAny x [MorphBallBomb, BoostBall]
    Expert -> containsAll x [PowerBomb, MorphBall] && containsAny x [MorphBallBomb, BoostBall]

lifeGroveTunnelItem :: Difficulty -> Map ItemName Int -> Bool
lifeGroveTunnelItem diff x = case diff of 
    Easy -> containsAll x [PowerBomb, MorphBall, BoostBall, MorphBallBomb]
    Medium -> containsAll x [PowerBomb, MorphBall, BoostBall, MorphBallBomb]
    Hard -> containsAll x [PowerBomb, MorphBall, BoostBall, MorphBallBomb]
    VeryHard -> containsAll x [PowerBomb, MorphBall, MorphBallBomb]
    Expert -> containsAll x [PowerBomb, MorphBall, MorphBallBomb]

lifeGroveSw :: Difficulty -> Map ItemName Int -> Bool
lifeGroveSw diff x = case diff of 
    Easy -> False
    Medium -> False
    Hard -> pb x && sj x
    VeryHard -> pb x && sj x
    Expert -> pb x && sj x

gthSpiderTrack ::  Difficulty -> Map ItemName Int -> Bool
gthSpiderTrack diff x = case diff of 
    Easy -> containsAll x [SpiderBall, MorphBall, SpaceJumpBoots]
    Medium -> containsAll x [SpiderBall, MorphBall, SpaceJumpBoots]
    Hard -> containsAll x [SpiderBall, MorphBall, SpaceJumpBoots] 
    VeryHard -> containsAll x [SpiderBall, MorphBall, SpaceJumpBoots] 
    Expert -> containsAll x [SpiderBall, MorphBall, SpaceJumpBoots]  || bombs x

gtcEnter ::  Difficulty -> Map ItemName Int -> Bool
gtcEnter diff x = case diff of 
    Easy -> sj x
    Medium -> sj x
    Hard -> sjOrBombs x
    VeryHard -> sjOrBombs x
    Expert -> sjOrBombs x

gtcSw :: Difficulty -> Map ItemName Int -> Bool
gtcSw diff x = case diff of
    Easy -> False
    Medium -> False
    Hard -> False
    VeryHard -> bombs x
    Expert -> bombs x

-- You can get past Connection Elevator to Deck Beta without gravity by wallcrawling
wallcrawlIntoFrigate :: Difficulty -> Map ItemName Int -> Bool
wallcrawlIntoFrigate diff x = case diff of
    Easy -> False
    Medium -> False
    Hard -> False
    VeryHard -> bombs x
    Expert -> bombs x

-- Chozo Predicates
mainPipe :: Difficulty -> Map ItemName Int -> Bool
mainPipe diff x = case diff of 
    Easy -> sj x || boost x 
    Medium -> sj x || boost x 
    Hard -> sj x || boost x 
    VeryHard -> sj x || boost x || bombs x
    Expert -> sj x || boost x || bombs x

mainPlazaGrappleLedge :: Difficulty -> Map ItemName Int -> Bool
mainPlazaGrappleLedge diff x = case diff of 
    Easy -> contains x GrappleBeam
    Medium -> containsAny x [GrappleBeam, SpaceJumpBoots]
    Hard -> containsAny x [GrappleBeam, SpaceJumpBoots]
    VeryHard -> containsAny x [GrappleBeam, SpaceJumpBoots]
    Expert -> containsAny x [GrappleBeam, SpaceJumpBoots]

mainPlazaLedge :: Difficulty -> Map ItemName Int -> Bool
mainPlazaLedge diff x = case diff of 
    Easy -> False
    Medium -> contains x SpaceJumpBoots
    Hard -> contains x SpaceJumpBoots
    VeryHard -> contains x SpaceJumpBoots
    Expert -> contains x SpaceJumpBoots

mainPlazaSw :: Difficulty -> Map ItemName Int -> Bool
mainPlazaSw diff x = case diff of
    Easy -> False
    Medium -> False
    Hard -> sj x && bombs x
    VeryHard -> sj x && bombs x
    Expert -> sj x && bombs x

ruinedFountainItem :: Difficulty -> Map ItemName Int -> Bool
ruinedFountainItem _ x = contains x SpiderBall

towerChamber :: Difficulty -> Map ItemName Int -> Bool
towerChamber diff x = case diff of 
    Easy -> containsAll x [GravitySuit, SpaceJumpBoots, WaveBeam]
    Medium -> containsAll x [SpaceJumpBoots, WaveBeam]
    Hard -> contains x WaveBeam
    VeryHard -> contains x WaveBeam
    Expert -> contains x WaveBeam
     
rsHalf :: Difficulty -> Map ItemName Int -> Bool
rsHalf diff x = case diff of 
    Easy -> boost x
    Medium -> boost x || containsAll x [SpaceJumpBoots, MorphBall]
    Hard -> boost x || containsAll x [SpaceJumpBoots, MorphBall]
    VeryHard -> contains x MorphBall
    Expert -> contains x MorphBall

tolAccess :: Difficulty -> Map ItemName Int -> Bool
tolAccess diff x = case diff of 
    Easy -> containsAll x [MorphBall, BoostBall, SpiderBall, WaveBeam]
    Medium -> containsAll x [MorphBall, BoostBall, SpiderBall, WaveBeam] || containsAll x [SpaceJumpBoots, WaveBeam]
    Hard -> containsAll x [MorphBall, BoostBall, SpiderBall, WaveBeam] || containsAll x [SpaceJumpBoots, WaveBeam]
    VeryHard -> contains x WaveBeam
    Expert -> contains x WaveBeam

towerOfLight :: Difficulty -> Map ItemName Int -> Bool
towerOfLight diff x = case diff of 
    Easy -> containsCount 8 Missile x && sj x
    Medium -> containsCount 8 Missile x && sj x
    Hard -> sj x
    VeryHard -> (containsCount 8 Missile x && bombs x) || sj x
    Expert -> (containsCount 8 Missile x && bombs x) || sj x

crossMagmaPool :: Difficulty -> Map ItemName Int -> Bool
crossMagmaPool diff x  = case diff of 
    Easy -> heatResist x && containsAll x [GrappleBeam,WaveBeam]
    Medium -> heatResist x && containsAll x [GrappleBeam,WaveBeam]
    Hard -> ((heatResist x && contains x GrappleBeam) || sj x) && wave x
    VeryHard -> ((heatResist x && contains x GrappleBeam) || sj x) && wave x
    Expert -> ((heatResist x && contains x GrappleBeam) || sj x) && wave x

--TODO check how many etanks this requires
magmaPoolItem :: Difficulty -> Map ItemName Int -> Bool
magmaPoolItem diff x  = case diff of 
    Easy -> heatResist x && containsAll x [GrappleBeam,MorphBall,PowerBomb]
    Medium -> heatResist x && containsAll x [GrappleBeam,MorphBall,PowerBomb]
    Hard -> heatResist x && ((containsAny x [GrappleBeam,SpaceJumpBoots] && pb x) || boost x)
    VeryHard -> (grapple x && pb x && heatResist x) || (sj x && pb x && (containsCount 3 EnergyTank x ||heatResist x)) || (boost x && (containsCount 7 EnergyTank x || heatResist x))
    Expert -> (grapple x && pb x && heatResist x) || (sj x && pb x && (containsCount 3 EnergyTank x ||heatResist x)) || (boost x && (containsCount 7 EnergyTank x || heatResist x))

tcItem :: Difficulty -> Map ItemName Int -> Bool
tcItem _ x = containsAll x [MorphBall, BoostBall, MorphBallBomb, SpiderBall]

tcTunnel :: Difficulty -> Map ItemName Int -> Bool
tcTunnel _ = boostBombs

climbSunTower :: Difficulty -> Map ItemName Int -> Bool
climbSunTower _ x = containsAll x [MorphBall, SpiderBall, MorphBallBomb, Missile, SuperMissile, ChargeBeam]

sunchamberghost :: Difficulty -> Map ItemName Int -> Bool
sunchamberghost = climbSunTower

gatheringHallSw :: Difficulty -> Map ItemName Int -> Bool
gatheringHallSw diff x = case diff of
    Easy -> False
    Medium -> False
    Hard -> sj x && bombs x
    VeryHard -> bombs x
    Expert -> bombs x

wateryHallSw :: Difficulty -> Map ItemName Int -> Bool
wateryHallSw diff x = case diff of
    Easy -> False
    Medium -> False
    Hard -> sj x && bombs x
    VeryHard -> bombs x
    Expert -> bombs x

wateryHallTraverse :: Difficulty -> Map ItemName Int -> Bool 
wateryHallTraverse diff x = containsAll x [MorphBall, MorphBallBomb, Missile]

wateryHallWater :: Difficulty -> Map ItemName Int -> Bool 
wateryHallWater diff x = case diff of 
    Easy -> contains x GravitySuit && (contains x SpaceJumpBoots || bombs x) 
    Medium -> contains x GravitySuit && (contains x SpaceJumpBoots || bombs x) 
    Hard -> True
    VeryHard ->  True
    Expert ->  True

furnaceTraverse :: Difficulty -> Map ItemName Int -> Bool 
furnaceTraverse diff x = case diff of 
    Easy -> containsAll x [MorphBall, MorphBallBomb, SpiderBall]
    Medium -> containsAll x [MorphBall, MorphBallBomb]
    Hard -> containsAll x [MorphBall, MorphBallBomb]
    VeryHard ->  containsAll x [MorphBall, MorphBallBomb]
    Expert ->  containsAll x [MorphBall, MorphBallBomb]

furnaceItem :: Difficulty -> Map ItemName Int -> Bool 
furnaceItem diff x = case diff of 
    Easy ->containsAll x [MorphBall, MorphBallBomb, SpiderBall, PowerBomb, BoostBall]
    Medium -> containsAll x [MorphBall, MorphBallBomb, SpiderBall, PowerBomb, BoostBall]
    Hard -> containsAll x [MorphBall, MorphBallBomb, SpiderBall] && (containsAll x [PowerBomb, BoostBall] || sj x)
    VeryHard ->  containsAll x [MorphBall, MorphBallBomb, SpiderBall] && (containsAll x [PowerBomb, BoostBall] || sj x)
    Expert ->  containsAll x [MorphBall, MorphBallBomb, SpiderBall] && (containsAll x [PowerBomb, BoostBall] || sj x)

crosswayInfiniteSpeed :: Difficulty -> Map ItemName Int -> Bool 
crosswayInfiniteSpeed diff x = diff == Expert && containsAll x [MorphBall, BoostBall, Missile]

crosswayTraverse :: Difficulty -> Map ItemName Int -> Bool 
crosswayTraverse diff x = case diff of 
    Easy -> containsAll x [MorphBall, BoostBall, Missile]
    Medium -> contains x Missile && (boost x || sj x)
    Hard -> contains x Missile && (boost x || sj x)
    VeryHard -> contains x Missile && (boost x || sj x || bombs x)
    Expert -> contains x Missile && (boost x || sj x || bombs x)

crosswayItem :: Difficulty -> Map ItemName Int -> Bool 
crosswayItem diff x = case diff of 
    Easy -> containsAll x [MorphBall, BoostBall, Missile, SuperMissile, ChargeBeam, SpiderBall, MorphBallBomb]
    Medium -> containsAll x [MorphBall, BoostBall, Missile, SuperMissile, ChargeBeam, SpiderBall, MorphBallBomb]
    Hard -> (sj x && contains x MorphBall) || containsAll x [MorphBall, BoostBall, Missile, SuperMissile, ChargeBeam, SpiderBall, MorphBallBomb]
    VeryHard -> (sj x && contains x MorphBall) || containsAll x [MorphBall, BoostBall, Missile, SuperMissile, ChargeBeam, SpiderBall, MorphBallBomb]
    Expert -> (sj x && contains x MorphBall) || containsAll x [MorphBall, BoostBall, Missile, SuperMissile, ChargeBeam, SpiderBall, MorphBallBomb]

hoteWave :: Difficulty -> Map ItemName Int -> Bool 
hoteWave diff x = case diff of 
    Easy -> containsAll x [MorphBall, SpiderBall, MorphBallBomb, WaveBeam]
    Medium -> containsAll x [MorphBall, MorphBallBomb, WaveBeam] && (spider x || sj x)
    Hard -> containsAll x [MorphBall, MorphBallBomb, WaveBeam] && (spider x || sj x)
    VeryHard -> containsAll x [MorphBall, MorphBallBomb, WaveBeam]
    Expert -> containsAll x [MorphBall, MorphBallBomb, WaveBeam]

hoteIce :: Difficulty -> Map ItemName Int -> Bool 
hoteIce diff x = case diff of 
    Easy -> containsAll x [MorphBall, SpiderBall, MorphBallBomb, IceBeam]
    Medium -> containsAll x [MorphBall, MorphBallBomb, IceBeam] && (spider x || sj x)
    Hard -> containsAll x [MorphBall, MorphBallBomb, IceBeam] && (spider x || sj x)
    VeryHard -> containsAll x [MorphBall, MorphBallBomb, IceBeam]
    Expert -> containsAll x [MorphBall, MorphBallBomb, IceBeam]

hotePlasma :: Difficulty -> Map ItemName Int -> Bool 
hotePlasma diff x = case diff of 
    Easy -> containsAll x [MorphBall, SpiderBall, MorphBallBomb, IceBeam, PlasmaBeam]
    Medium -> containsAll x [MorphBall, MorphBallBomb, IceBeam, PlasmaBeam] && (spider x || sj x)
    Hard -> containsAll x [MorphBall, MorphBallBomb, IceBeam, PlasmaBeam] && (spider x || sj x)
    VeryHard -> containsAll x [MorphBall, MorphBallBomb, IceBeam, PlasmaBeam]
    Expert -> containsAll x [MorphBall, MorphBallBomb, IceBeam, PlasmaBeam]

reflectPoolSave :: Difficulty -> Map ItemName Int -> Bool 
reflectPoolSave diff x = reflectPoolAntechamber diff x && contains x Missile

reflectPoolIceDoor :: Difficulty -> Map ItemName Int -> Bool 
reflectPoolIceDoor diff x = reflectPoolAntechamber diff x && contains x IceBeam

reflectPoolAntechamber :: Difficulty -> Map ItemName Int -> Bool 
reflectPoolAntechamber diff x = case diff of 
    Easy -> containsAll x [MorphBall, MorphBallBomb, BoostBall]
    Medium -> containsAll x [MorphBall, MorphBallBomb, BoostBall] || sj x
    Hard -> containsAll x [MorphBall, MorphBallBomb, BoostBall] || sj x
    VeryHard -> bombs x || sj x
    Expert -> bombs x || sj x

-- Magmoor Predicates
vmr1Tank :: Difficulty -> Map ItemName Int -> Bool 
vmr1Tank diff x = case diff of 
    Easy -> heatResist x
    Medium -> heatResist x
    Hard -> heatResist x || containsCount 2 EnergyTank x && sj x
    VeryHard -> heatResist x || (containsCount 1 EnergyTank x && sj x) || containsCount 2 EnergyTank x
    Expert -> heatResist x || (containsCount 1 EnergyTank x && sj x) || containsCount 2 EnergyTank x

vmr2Tank :: Difficulty -> Map ItemName Int -> Bool 
vmr2Tank diff x = case diff of 
    Easy -> heatResist x
    Medium -> heatResist x
    Hard -> heatResist x || containsCount 3 EnergyTank x && sj x
    VeryHard -> heatResist x || (containsCount 2 EnergyTank x && sj x) || containsCount 3 EnergyTank x
    Expert -> heatResist x || (containsCount 2 EnergyTank x && sj x) || containsCount 3 EnergyTank x

vmr3Tank :: Difficulty -> Map ItemName Int -> Bool 
vmr3Tank diff x = case diff of 
    Easy -> heatResist x
    Medium -> heatResist x
    Hard -> heatResist x || containsCount 4 EnergyTank x && sj x
    VeryHard -> heatResist x || (containsCount 3 EnergyTank x && sj x) || containsCount 4 EnergyTank x
    Expert -> heatResist x || (containsCount 3 EnergyTank x && sj x) || containsCount 4 EnergyTank x

vmr4Tank :: Difficulty -> Map ItemName Int -> Bool 
vmr4Tank diff x = case diff of 
    Easy -> heatResist x
    Medium -> heatResist x
    Hard -> heatResist x || containsCount 5 EnergyTank x && sj x
    VeryHard -> heatResist x || (containsCount 4 EnergyTank x && sj x) || containsCount 5 EnergyTank x
    Expert -> heatResist x || (containsCount 4 EnergyTank x && sj x) || containsCount 5 EnergyTank x

heatResistOr8Etanks :: Difficulty -> Map ItemName Int -> Bool 
heatResistOr8Etanks diff x = case diff of 
    Easy -> heatResist x
    Medium -> heatResist x
    Hard -> heatResist x || containsCount 8 EnergyTank x
    VeryHard -> heatResist x || containsCount 8 EnergyTank x
    Expert -> heatResist x || containsCount 8 EnergyTank x

burningTrailSw :: Difficulty -> Map ItemName Int -> Bool 
burningTrailSw diff x = case diff of 
    Easy -> False
    Medium -> False
    Hard -> False
    VeryHard -> bombs x
    Expert -> bombs x

magmoorFrontWallcrawl :: Difficulty -> Map ItemName Int -> Bool 
magmoorFrontWallcrawl diff x = case diff of 
    Easy -> False
    Medium -> False
    Hard -> False
    VeryHard -> False
    Expert -> heatResist x && bombs x

lavaLakeTraversal :: Difficulty -> Map ItemName Int -> Bool 
lavaLakeTraversal diff x = vmr4Tank diff x && bombs x

lavaLakeReverseTraversal :: Difficulty -> Map ItemName Int -> Bool 
lavaLakeReverseTraversal diff x = vmr2Tank diff x && bombs x

lavaLakeItem :: Difficulty -> Map ItemName Int -> Bool 
lavaLakeItem diff x = case diff of 
    Easy -> missile x && sj x && heatResist x
    Medium -> missile x && sj x && heatResist x
    Hard -> missile x && (heatResist x || (sj x && containsCount 2 EnergyTank x))
    VeryHard -> missile x && (heatResist x || (sj x && contains x EnergyTank) || containsCount 2 EnergyTank x)
    Expert -> missile x && (heatResist x || (sj x && contains x EnergyTank) || containsCount 2 EnergyTank x)

pitTunnel :: Difficulty -> Map ItemName Int -> Bool 
pitTunnel diff x = vmr2Tank diff x && contains x MorphBall

pitTunnelReverse :: Difficulty -> Map ItemName Int -> Bool 
pitTunnelReverse diff x = vmr3Tank diff x && contains x MorphBall

triclopsPitItem :: Difficulty -> Map ItemName Int -> Bool 
triclopsPitItem diff x = case diff of 
    Easy -> containsAll x [SpaceJumpBoots, Missile] && heatResist x
    Medium -> containsAll x [SpaceJumpBoots, Missile] && heatResist x
    Hard -> missile x && vmr1Tank diff x
    VeryHard -> missile x && vmr1Tank diff x
    Expert -> missile x && vmr1Tank diff x

storageCavern :: Difficulty -> Map ItemName Int -> Bool 
storageCavern diff x = morph x && vmr1Tank diff x

toTransportTunnelA :: Difficulty -> Map ItemName Int -> Bool 
toTransportTunnelA diff x = bombs x && vmr1Tank diff x

monitorStationClimb :: Difficulty -> Map ItemName Int -> Bool 
monitorStationClimb diff x = case diff of 
    Easy ->heatResist x && containsAll x [SpaceJumpBoots,MorphBall,BoostBall]
    Medium -> heatResist x && containsAll x [SpaceJumpBoots,MorphBall,BoostBall]
    Hard -> vmr3Tank diff x && (sj x || bombs x)
    VeryHard -> vmr3Tank diff x && (sj x || bombs x)
    Expert -> vmr3Tank diff x && (sj x || bombs x)

warriorShrineTunnel :: Difficulty -> Map ItemName Int -> Bool
warriorShrineTunnel diff x = vmr4Tank diff x && pb x && bombs x

-- TODO can you use spider without heat resistance?
crossTft :: Difficulty -> Map ItemName Int -> Bool
crossTft diff x = case diff of 
    Easy -> spider x
    Medium -> spider x
    Hard -> spider x || sj x || (contains x GravitySuit && bombs x && containsCount 2 EnergyTank x)
    VeryHard -> spider x || sj x || (contains x GravitySuit && bombs x && containsCount 2 EnergyTank x)
    Expert -> spider x || sj x || (contains x GravitySuit && bombs x && containsCount 2 EnergyTank x)

crossTftReverse :: Difficulty -> Map ItemName Int -> Bool
crossTftReverse diff x = case diff of 
    Easy -> spider x
    Medium -> spider x || sj x
    Hard -> spider x || sj x || (contains x GravitySuit && bombs x && containsCount 2 EnergyTank x)
    VeryHard -> spider x || sj x || heatResist x
    Expert -> spider x || sj x || heatResist x

crossTwinFires :: Difficulty -> Map ItemName Int -> Bool
crossTwinFires diff x = case diff of 
    Easy -> sj x && contains x WaveBeam
    Medium -> sj x && contains x WaveBeam
    Hard -> sj x  && contains x WaveBeam
    VeryHard -> (sj x || missile x) && contains x WaveBeam
    Expert -> (sj x || missile x) && contains x WaveBeam

crossNorthCoreTunnel :: Difficulty -> Map ItemName Int -> Bool
crossNorthCoreTunnel diff x = case diff of 
    Easy -> containsAll x [Missile, SpaceJumpBoots, WaveBeam]
    Medium -> containsAll x [Missile, SpaceJumpBoots, WaveBeam]
    Hard -> containsAll x [SpaceJumpBoots, WaveBeam]
    VeryHard -> contains x WaveBeam && (missile x || sj x)
    Expert -> contains x WaveBeam && (missile x || sj x)

workstationTunnel :: Difficulty -> Map ItemName Int -> Bool
workstationTunnel diff x = containsAll x [IceBeam, PowerBomb, MorphBall]

workstationItem :: Difficulty -> Map ItemName Int -> Bool
workstationItem diff x = containsAll x [MorphBall, WaveBeam]

workstationWaveDoor :: Difficulty -> Map ItemName Int -> Bool
workstationWaveDoor diff x = sjOrBombs x && contains x WaveBeam

workstationSw :: Difficulty -> Map ItemName Int -> Bool
workstationSw diff x =  case diff of 
    Easy -> False
    Medium -> False
    Hard -> False
    VeryHard -> sj x
    Expert -> sj x || (ice x && bombs x)

geoCore :: Difficulty -> Map ItemName Int -> Bool
geoCore diff x = case diff of 
    Easy -> containsAll x [SpaceJumpBoots, GrappleBeam, SpiderBall, MorphBall, MorphBallBomb, BoostBall, IceBeam]
    Medium -> containsAll x [SpaceJumpBoots, GrappleBeam, SpiderBall, MorphBall, MorphBallBomb, BoostBall, IceBeam]
    Hard -> containsAll x [SpaceJumpBoots, MorphBall, MorphBallBomb, BoostBall, IceBeam]
    VeryHard -> containsAll x [SpaceJumpBoots, MorphBall, MorphBallBomb, BoostBall, IceBeam]
    Expert -> containsAll x [SpaceJumpBoots, MorphBall, MorphBallBomb, BoostBall, IceBeam]

-- Phendrana Predicates
iceBarrier :: Difficulty -> Map ItemName Int -> Bool
iceBarrier _ x = containsAny x [Missile, ChargeBeam]

shorelinesTower :: Difficulty -> Map ItemName Int -> Bool
shorelinesTower diff x = case diff of 
    Easy -> containsAll x [MorphBall, SpaceJumpBoots, SpiderBall, ChargeBeam, SuperMissile, Missile]
    Medium -> containsAll x [MorphBall, SpaceJumpBoots, SpiderBall, ChargeBeam, SuperMissile, Missile]
    Hard -> containsAll x [MorphBall, SpaceJumpBoots, SpiderBall, ChargeBeam, SuperMissile, Missile]
    VeryHard -> containsAll x [MorphBall, SpiderBall, ChargeBeam, SuperMissile, Missile] && sjOrBombs x
    Expert -> containsAll x [MorphBall, SpiderBall, ChargeBeam, SuperMissile, Missile] && sjOrBombs x

shorelinesItem:: Difficulty -> Map ItemName Int -> Bool
shorelinesItem diff x = case diff of 
    Easy -> plasma x
    Medium -> plasma x
    Hard -> plasma x || (wave x && boost x && sj x && bombs x) -- Infinite Speed
    VeryHard -> plasma x || (wave x && boost x && sj x && bombs x)
    Expert -> plasma x || (wave x && boost x && sj x && bombs x)


iceTempleClimb :: Difficulty -> Map ItemName Int -> Bool
iceTempleClimb diff x = case diff of 
    Easy -> containsAll x [SpaceJumpBoots, MorphBall, MorphBallBomb, Missile]
    Medium -> containsAll x [SpaceJumpBoots, MorphBall, MorphBallBomb, Missile]
    Hard -> containsAll x [MorphBall, MorphBallBomb, Missile]
    VeryHard -> containsAll x [MorphBall, MorphBallBomb, Missile]
    Expert -> containsAll x [MorphBall, MorphBallBomb, Missile]

iceTempleItem :: Difficulty -> Map ItemName Int -> Bool
iceTempleItem diff x = case diff of 
    Easy -> containsAll x [SpaceJumpBoots, MorphBall, MorphBallBomb, PlasmaBeam]
    Medium ->containsAll x [SpaceJumpBoots, MorphBall, MorphBallBomb, PlasmaBeam]
    Hard -> containsAll x [MorphBall, MorphBallBomb, PlasmaBeam] || (sj x && boost x && bombs x && missile x) -- Infinite Speed
    VeryHard -> containsAll x [MorphBall, MorphBallBomb, PlasmaBeam] || (sj x && boost x && bombs x && missile x)
    Expert -> containsAll x [MorphBall, MorphBallBomb, PlasmaBeam] || (sj x && boost x && bombs x && missile x)

climbShorelines :: Difficulty -> Map ItemName Int -> Bool 
climbShorelines diff x = case diff of 
    Easy -> sj x
    Medium -> sj x
    Hard -> sj x
    VeryHard -> sjOrBombs x
    Expert -> sjOrBombs x

ireSpiderTrack :: Difficulty -> Map ItemName Int -> Bool 
ireSpiderTrack diff x = case diff of 
    Easy -> spider x
    Medium -> spider x
    Hard -> spider x
    VeryHard -> spider x || bombs x
    Expert -> spider x || bombs x

irwDoor :: Difficulty -> Map ItemName Int -> Bool
irwDoor diff x = case diff of 
    Easy -> containsAll x [SpaceJumpBoots, WaveBeam]
    Medium -> containsAll x [SpaceJumpBoots, WaveBeam]
    Hard -> containsAll x [SpaceJumpBoots, WaveBeam]
    VeryHard -> sjOrBombs x && wave x
    Expert -> sjOrBombs x && wave x

irwItem :: Difficulty -> Map ItemName Int -> Bool
irwItem diff x = case diff of 
    Easy -> containsAll x [SpaceJumpBoots, Missile, PlasmaBeam]
    Medium -> containsAll x [SpaceJumpBoots, Missile, PlasmaBeam]
    Hard -> containsAll x [Missile, PlasmaBeam]
    VeryHard -> containsAll x [Missile, PlasmaBeam]
    Expert -> containsAll x [Missile, PlasmaBeam]

irwSw:: Difficulty -> Map ItemName Int -> Bool
irwSw diff x = case diff of 
    Easy -> False
    Medium -> False
    Hard -> False
    VeryHard -> bombs x
    Expert -> bombs x

ruinedCourtyardConduit :: Difficulty -> Map ItemName Int -> Bool
ruinedCourtyardConduit _ x = containsAll x [ChargeBeam, Missile, SuperMissile, WaveBeam]

ruinedCourtyardSave :: Difficulty -> Map ItemName Int -> Bool
ruinedCourtyardSave diff x = containsAll x [SpaceJumpBoots, Missile]

--Might not need bombs if using spider track, but bombs are almost always unrandomized anyway
ruinedCourtyardClimb :: Difficulty -> Map ItemName Int -> Bool
ruinedCourtyardClimb diff x = case diff of 
    Easy -> (spider x && sjOrBombs x) || containsAll x [SpaceJumpBoots, MorphBall, BoostBall, MorphBallBomb]
    Medium -> (spider x && sjOrBombs x) || containsAll x [SpaceJumpBoots, MorphBall, BoostBall, MorphBallBomb]
    Hard -> (spider x && sjOrBombs x) || sj x
    VeryHard -> sjOrBombs x
    Expert -> sjOrBombs x

ruinedCourtyardSw :: Difficulty -> Map ItemName Int -> Bool
ruinedCourtyardSw diff x = case diff of 
    Easy -> False
    Medium -> False
    Hard -> sj x
    VeryHard -> sj x
    Expert -> sj x

quarantineTunnel :: Difficulty -> Map ItemName Int -> Bool
quarantineTunnel _ x = containsAll x [MorphBall, WaveBeam]

climbQuarantineCaveEntrance :: Difficulty -> Map ItemName Int -> Bool
climbQuarantineCaveEntrance diff x = case diff of 
    Easy -> spider x
    Medium -> spider x
    Hard -> spider x || sj x 
    VeryHard -> spider x || sj x
    Expert -> spider x || sj x

climbQuarantineCaveBack :: Difficulty -> Map ItemName Int -> Bool
climbQuarantineCaveBack diff x = case diff of 
    Easy -> spider x 
    Medium -> spider x || (sj x && grapple x)
    Hard -> spider x || sj x || grapple x
    VeryHard -> spider x || sj x || grapple x
    Expert -> spider x || sj x || grapple x

quarantineMonitor :: Difficulty -> Map ItemName Int -> Bool
quarantineMonitor diff x = case diff of 
    Easy -> grapple x 
    Medium -> grapple x 
    Hard -> grapple x || sj x
    VeryHard -> grapple x || sj x
    Expert -> grapple x || sj x

phenElevatorClimb :: Difficulty -> Map ItemName Int -> Bool
phenElevatorClimb diff x = case diff of 
    Easy -> containsAll x [MorphBall, SpiderBall, IceBeam]
    Medium -> containsAll x [MorphBall, SpiderBall, IceBeam]
    Hard -> (spider x || sj x) && ice x
    VeryHard -> (spider x || sj x || bombs x) && ice x
    Expert -> (spider x || sj x || bombs x) && ice x

observatoryClimb :: Difficulty -> Map ItemName Int -> Bool
observatoryClimb diff x = case diff of 
    Easy -> containsAll x [MorphBall, BoostBall, MorphBallBomb, SpaceJumpBoots]
    Medium -> containsAll x [MorphBall, BoostBall, MorphBallBomb, SpaceJumpBoots]
    Hard -> sj x
    VeryHard -> sj x
    Expert -> sj x

observatorySave :: Difficulty -> Map ItemName Int -> Bool
observatorySave diff x = case diff of 
    Easy -> sj x && contains x Missile
    Medium -> sj x && contains x Missile
    Hard -> sj x && contains x Missile
    VeryHard -> contains x Missile
    Expert -> contains x Missile

observatoryItem :: Difficulty -> Map ItemName Int -> Bool
observatoryItem diff x = case diff of 
    Easy -> containsAll x [MorphBall, BoostBall, MorphBallBomb, SpaceJumpBoots]
    Medium -> sj x
    Hard -> sj x
    VeryHard -> sjOrBombs x
    Expert -> sjOrBombs x

controlTowerItem :: Difficulty -> Map ItemName Int -> Bool
controlTowerItem diff x = case diff of 
    Easy -> containsAll x [MorphBall, PlasmaBeam, Missile] && sjOrBombs x
    Medium -> containsAll x [MorphBall, PlasmaBeam, Missile] && sjOrBombs x
    Hard -> (bombs x && plasma x && missile x) || (sj x && missile x && morph x)
    VeryHard -> (bombs x && plasma x && missile x) || (sj x && missile x && morph x)
    Expert -> (bombs x && plasma x && missile x) || (sj x && missile x && morph x)

rlaTrack :: Difficulty -> Map ItemName Int -> Bool
rlaTrack _ x = contains x MorphBall && containsAny x [MorphBallBomb, SpaceJumpBoots]

toStorageCave :: Difficulty -> Map ItemName Int -> Bool
toStorageCave diff x =  case diff of 
    Easy -> containsAll x [SpaceJumpBoots, GrappleBeam, PlasmaBeam, MorphBall, PowerBomb]
    Medium -> containsAll x [SpaceJumpBoots, PlasmaBeam, MorphBall, PowerBomb]
    Hard ->containsAll x [SpaceJumpBoots, PlasmaBeam, MorphBall, PowerBomb]
    VeryHard -> containsAll x [PlasmaBeam, MorphBall, PowerBomb] && sjOrBombs x
    Expert -> containsAll x [PlasmaBeam, MorphBall, PowerBomb] && sjOrBombs x

fromStorageCave :: Difficulty -> Map ItemName Int -> Bool
fromStorageCave _ x = containsAll x [PlasmaBeam, MorphBall, PowerBomb]

toSecurityCave :: Difficulty -> Map ItemName Int -> Bool
toSecurityCave diff x = case diff of 
    Easy -> containsAll x [SpaceJumpBoots, GrappleBeam, MorphBall]
    Medium -> containsAll x [SpaceJumpBoots, MorphBall]
    Hard -> containsAll x [SpaceJumpBoots, MorphBall]
    VeryHard -> morph x && (sjOrBombs x || grapple x)
    Expert -> morph x && (sjOrBombs x || grapple x)

phenEdgeLower :: Difficulty -> Map ItemName Int -> Bool
phenEdgeLower diff x = case diff of 
    Easy -> containsAll x [WaveBeam, GravitySuit, SpaceJumpBoots]
    Medium -> containsAll x [WaveBeam, SpaceJumpBoots]
    Hard -> containsAll x [WaveBeam, SpaceJumpBoots]
    VeryHard -> wave x && sjOrBombs x
    Expert -> wave x && sjOrBombs x

frozenPikeBottom :: Difficulty -> Map ItemName Int -> Bool
frozenPikeBottom diff x = case diff of 
    Easy -> containsAll x [WaveBeam, GravitySuit, SpaceJumpBoots]
    Medium -> wave x && sjOrBombs x
    Hard ->  wave x && sjOrBombs x
    VeryHard ->  wave x && sjOrBombs x
    Expert ->  wave x && sjOrBombs x

frozenPikeClimb :: Difficulty -> Map ItemName Int -> Bool
frozenPikeClimb diff x = case diff of 
    Easy -> containsAll x [MorphBall,MorphBallBomb,SpaceJumpBoots]
    Medium -> containsAll x [MorphBall,MorphBallBomb,SpaceJumpBoots]
    Hard -> containsAll x [MorphBall,MorphBallBomb,SpaceJumpBoots]
    VeryHard -> sjOrBombs x
    Expert -> sjOrBombs x

gravLedge :: Difficulty -> Map ItemName Int -> Bool
gravLedge diff x = case diff of 
    Easy -> containsAll x [PlasmaBeam, GrappleBeam]
    Medium -> containsAll x [PlasmaBeam, GrappleBeam]
    Hard -> containsAll x [PlasmaBeam, GrappleBeam] || sj x
    VeryHard -> containsAll x [PlasmaBeam, GrappleBeam] || sj x
    Expert -> containsAll x [PlasmaBeam, GrappleBeam] || sj x

climbGravityChamber :: Difficulty -> Map ItemName Int -> Bool
climbGravityChamber diff x = case diff of 
    Easy -> contains x GravitySuit && sjOrBombs x
    Medium -> contains x GravitySuit && sjOrBombs x
    Hard -> contains x GravitySuit && sjOrBombs x
    VeryHard -> (contains x GravitySuit && sjOrBombs x) || sj x
    Expert -> (contains x GravitySuit && sjOrBombs x) || sj x

gravityChamberToLakeTunnel :: Difficulty -> Map ItemName Int -> Bool
gravityChamberToLakeTunnel diff x = climbGravityChamber diff x && contains x WaveBeam

hunterCaveClimb :: Difficulty -> Map ItemName Int -> Bool
hunterCaveClimb diff x = case diff of 
    Easy -> contains x Missile && (contains x SpaceJumpBoots || bombs x)
    Medium -> sjOrBombs x
    Hard -> sjOrBombs x
    VeryHard -> sjOrBombs x
    Expert -> sjOrBombs x

hunterCaveUpper :: Difficulty -> Map ItemName Int -> Bool
hunterCaveUpper diff x = case diff of 
    Easy -> containsAll x [Missile, GrappleBeam]
    Medium -> containsAll x [Missile, GrappleBeam]
    Hard -> sj x || containsAll x [Missile, GrappleBeam]
    VeryHard -> sj x || containsAll x [Missile, GrappleBeam]
    Expert -> sj x || containsAll x [Missile, GrappleBeam]

hunterCaveLower :: Difficulty -> Map ItemName Int -> Bool
hunterCaveLower _ x = contains x Missile && (contains x SpaceJumpBoots || bombs x)

frostCaveAccess :: Difficulty -> Map ItemName Int -> Bool
frostCaveAccess _ x = containsAll x [MorphBall, WaveBeam]

frostCaveDoor :: Difficulty -> Map ItemName Int -> Bool
frostCaveDoor _ x = containsAll x [Missile,WaveBeam] && (contains x SpaceJumpBoots || bombs x)

frostCaveItem :: Difficulty -> Map ItemName Int -> Bool
frostCaveItem diff x = case diff of 
    Easy -> containsAll x [GrappleBeam, Missile]
    Medium -> containsAll x [GrappleBeam, Missile]
    Hard -> missile x && (sj x || grapple x)
    VeryHard -> missile x && (sj x || bombs x || grapple x)
    Expert -> missile x && (sj x || bombs x || grapple x)

frostCaveToTunnel :: Difficulty -> Map ItemName Int -> Bool
frostCaveToTunnel _ x = containsAll x [Missile,WaveBeam,MorphBall] && (contains x SpaceJumpBoots || bombs x)

frostCaveSw :: Difficulty -> Map ItemName Int -> Bool
frostCaveSw diff x = case diff of 
    Easy -> False
    Medium -> False
    Hard -> sj x
    VeryHard -> sjOrBombs x 
    Expert -> sjOrBombs x 

-- Mines Predicates
quarrySave :: Difficulty -> Map ItemName Int -> Bool
quarrySave diff x = case diff of 
    Easy -> containsAll x [SpiderBall, MorphBall, WaveBeam]
    Medium -> wave x && (spider x || sj x)
    Hard -> wave x && (spider x || sj x)
    VeryHard -> wave x && (spider x || sj x || bombs x)
    Expert -> wave x && (spider x || sj x || bombs x)

quarryItem :: Difficulty -> Map ItemName Int -> Bool
quarryItem diff x = case diff of 
    Easy -> containsAll x [SpaceJumpBoots, WaveBeam, MorphBall, SpiderBall]
    Medium -> containsAll x [SpaceJumpBoots, WaveBeam, MorphBall, SpiderBall]
    Hard -> wave x && (spider x || sj x)
    VeryHard -> wave x && (spider x || sj x)
    Expert -> wave x && (spider x || sj x)

reachWasteDisposal :: Difficulty -> Map ItemName Int -> Bool
reachWasteDisposal diff x = case diff of
    Easy -> containsAll x [SpaceJumpBoots,WaveBeam,IceBeam,GrappleBeam]
    Medium -> containsAll x [WaveBeam,IceBeam,GrappleBeam] && sjOrBombs x
    Hard -> ice x && sjOrBombs x
    VeryHard ->  ice x && sjOrBombs x
    Expert -> ice x && sjOrBombs x

oreProcessingClimb :: Difficulty -> Map ItemName Int -> Bool
oreProcessingClimb diff x = case diff of
    Easy -> containsAll x [MorphBall, SpiderBall, MorphBallBomb, IceBeam]
    Medium -> containsAll x [MorphBall, SpiderBall, MorphBallBomb, IceBeam]
    Hard -> ice x && (containsAll x [MorphBall, SpiderBall, MorphBallBomb] || sj x)
    VeryHard ->  ice x && (containsAll x [MorphBall, SpiderBall, MorphBallBomb] || sj x)
    Expert -> ice x && (containsAll x [MorphBall, SpiderBall, MorphBallBomb] || sj x)

oreProcessingTop :: Difficulty -> Map ItemName Int -> Bool
oreProcessingTop diff x = case diff of
    Easy -> containsAll x [MorphBall, SpiderBall, MorphBallBomb, PowerBomb, IceBeam]
    Medium -> containsAll x [MorphBall, SpiderBall, MorphBallBomb, PowerBomb, IceBeam]
    Hard -> ice x && (containsAll x [MorphBall, SpiderBall, MorphBallBomb, PowerBomb] || sj x)
    VeryHard ->  ice x && (containsAll x [MorphBall, SpiderBall, MorphBallBomb, PowerBomb] || sj x)
    Expert -> ice x && (containsAll x [MorphBall, SpiderBall, MorphBallBomb, PowerBomb] || sj x)

wasteDisposalTraversal :: Difficulty -> Map ItemName Int -> Bool
wasteDisposalTraversal _ x = containsAll x [MorphBall, MorphBallBomb, IceBeam]

shaftClimb1 :: Difficulty -> Map ItemName Int -> Bool
shaftClimb1 diff x = case diff of
    Easy -> containsAll x [MorphBall, SpiderBall, IceBeam]
    Medium -> containsAll x [MorphBall, SpiderBall, IceBeam]
    Hard -> ice x && (spider x || (bombs x && sj x))
    VeryHard ->  ice x && (spider x || (bombs x && sj x))
    Expert -> ice x && (spider x || (bombs x && sj x))

shaftClimb2 :: Difficulty -> Map ItemName Int -> Bool
shaftClimb2 diff x = case diff of
    Easy -> containsAll x [MorphBall, SpiderBall, BoostBall, IceBeam]
    Medium -> containsAll x [MorphBall, SpiderBall, BoostBall, IceBeam]
    Hard -> ice x && boost x && (spider x || sj x)
    VeryHard ->  ice x && boost x && (spider x || sj x)
    Expert -> ice x && boost x && (spider x || sj x)

storageDepotABarrier :: Difficulty -> Map ItemName Int -> Bool
storageDepotABarrier _ x = contains x StorageDepotABarrier

securityAccessBSw:: Difficulty -> Map ItemName Int -> Bool
securityAccessBSw diff x = case diff of
    Easy -> False
    Medium -> False
    Hard -> False
    VeryHard -> bombs x
    Expert -> bombs x

maintTunnel :: Difficulty -> Map ItemName Int -> Bool
maintTunnel _ x = containsAll x [MorphBall, IceBeam, PowerBomb]

ppcClimb :: Difficulty -> Map ItemName Int -> Bool
ppcClimb diff x = case diff of
    Easy -> containsAll x [MorphBall, SpiderBall, SpaceJumpBoots, IceBeam]
    Medium -> containsAll x [MorphBall, SpiderBall, SpaceJumpBoots, IceBeam]
    Hard -> ice x && sj x
    VeryHard -> ice x && sjOrBombs x
    Expert -> ice x && sjOrBombs x

toMinesElevator :: Difficulty -> Map ItemName Int -> Bool
toMinesElevator diff x = case diff of
    Easy -> containsAll x [GrappleBeam, IceBeam]
    Medium -> ice x && (grapple x || sj x)
    Hard -> ice x 
    VeryHard -> ice x 
    Expert -> ice x 

centralDynamoClimb :: Difficulty -> Map ItemName Int -> Bool
centralDynamoClimb diff x = case diff of
    Easy -> contains x IceBeam && sj x
    Medium -> contains x IceBeam && sj x
    Hard -> contains x IceBeam && sj x
    VeryHard -> contains x IceBeam && sjOrBombs x
    Expert -> contains x IceBeam && sjOrBombs x

mqaItem :: Difficulty -> Map ItemName Int -> Bool
mqaItem diff x = case diff of
    Easy -> containsAll x [SpaceJumpBoots, XRayVisor, MorphBall, PowerBomb]
    Medium -> containsAll x [SpaceJumpBoots, MorphBall, PowerBomb]
    Hard -> sj x
    VeryHard -> sj x || (bombs x && pb x)
    Expert -> sj x || (bombs x && pb x)

mqaTraversal :: Difficulty -> Map ItemName Int -> Bool
mqaTraversal diff x = case diff of
    Easy -> containsAll x [SpaceJumpBoots, XRayVisor, MorphBall, SpiderBall, IceBeam]
    Medium -> containsAll x [SpaceJumpBoots, MorphBall, SpiderBall, IceBeam]
    Hard -> ice x && sj x && (pb x || spider x)
    VeryHard -> ice x && sjOrBombs x && (pb x || spider x)
    Expert -> ice x && sjOrBombs x && (pb x || spider x)

ecaItem :: Difficulty -> Map ItemName Int -> Bool
ecaItem diff x = case diff of
    Easy -> containsAll x [SpaceJumpBoots, MorphBall, MorphBallBomb]
    Medium -> containsAll x [SpaceJumpBoots, MorphBall, MorphBallBomb]
    Hard -> containsAll x [SpaceJumpBoots, MorphBall, MorphBallBomb]
    VeryHard -> (morph x && sj x) || bombs x
    Expert -> (morph x && sj x) || bombs x

eliteResearchTopItem :: Difficulty -> Map ItemName Int -> Bool
eliteResearchTopItem diff x = case diff of
    Easy -> containsAll x [SpaceJumpBoots, MorphBall, BoostBall]
    Medium -> containsAll x [SpaceJumpBoots, MorphBall, BoostBall]
    Hard -> containsAll x [SpaceJumpBoots, MorphBall, BoostBall]
    VeryHard -> sjOrBombs x
    Expert -> sjOrBombs x

eliteResearchDoor :: Difficulty -> Map ItemName Int -> Bool
eliteResearchDoor diff x = case diff of
    Easy -> containsAll x [SpaceJumpBoots, MorphBall, BoostBall, IceBeam]
    Medium -> containsAll x [SpaceJumpBoots, MorphBall, BoostBall, IceBeam]
    Hard -> containsAll x [SpaceJumpBoots, MorphBall, BoostBall, IceBeam]
    VeryHard -> sjOrBombs x && ice x
    Expert -> sjOrBombs x && ice x

toStorageDepotA :: Difficulty -> Map ItemName Int -> Bool
toStorageDepotA diff x = case diff of
    Easy -> containsAll x [WaveBeam, MorphBall, PowerBomb, PlasmaBeam]
    Medium -> containsAll x [MorphBall, PowerBomb, PlasmaBeam]
    Hard -> containsAll x [MorphBall, PowerBomb, PlasmaBeam]
    VeryHard -> containsAll x [MorphBall, PowerBomb, PlasmaBeam]
    Expert -> containsAll x [MorphBall, PowerBomb, PlasmaBeam]

climbFungalHallAccess :: Difficulty -> Map ItemName Int -> Bool
climbFungalHallAccess diff x = case diff of
    Easy -> containsAll x [SpaceJumpBoots, PlasmaBeam]
    Medium -> containsAll x [SpaceJumpBoots, PlasmaBeam]
    Hard -> containsAll x [SpaceJumpBoots, PlasmaBeam]
    VeryHard -> sjOrBombs x && plasma x
    Expert -> sjOrBombs x && plasma x

fungalHallATraversal :: Difficulty -> Map ItemName Int -> Bool
fungalHallATraversal diff x = case diff of
    Easy -> containsAll x [SpaceJumpBoots, GrappleBeam, IceBeam]
    Medium -> containsAll x [SpaceJumpBoots, IceBeam]
    Hard -> containsAll x [SpaceJumpBoots, IceBeam]
    VeryHard -> sjOrBombs x && ice x
    Expert -> sjOrBombs x && ice x

miningTunnelTraversal :: Difficulty -> Map ItemName Int -> Bool
miningTunnelTraversal _ x = containsAll x [MorphBall, MorphBallBomb, PlasmaBeam]

-- TODO Double check e-tank requirements
miningTunnelItem :: Difficulty -> Map ItemName Int -> Bool
miningTunnelItem diff x = case diff of
    Easy -> containsAll x [MorphBall, MorphBallBomb, PhazonSuit]
    Medium -> containsAll x [MorphBall, MorphBallBomb, PhazonSuit]
    Hard -> containsAll x [MorphBall, MorphBallBomb] && (contains x PhazonSuit || (containsCount 10 EnergyTank x && boost x))
    VeryHard -> containsAll x [MorphBall, MorphBallBomb] && (contains x PhazonSuit || (containsCount 6 EnergyTank x && boost x))
    Expert -> containsAll x [MorphBall, MorphBallBomb] && (contains x PhazonSuit || (containsCount 6 EnergyTank x && boost x))

quarantineAccessBTraversal :: Difficulty -> Map ItemName Int -> Bool
quarantineAccessBTraversal diff x = case diff of
    Easy -> containsAll x [SpaceJumpBoots, PlasmaBeam]
    Medium -> plasma x
    Hard -> plasma x
    VeryHard -> plasma x
    Expert -> plasma x

fungalHallBTraversal :: Difficulty -> Map ItemName Int -> Bool
fungalHallBTraversal diff x = case diff of
    Easy -> containsAll x [SpaceJumpBoots, GrappleBeam, PlasmaBeam]
    Medium -> containsAll x [SpaceJumpBoots, PlasmaBeam]
    Hard -> containsAll x [SpaceJumpBoots, PlasmaBeam]
    VeryHard -> sjOrBombs x && plasma x
    Expert -> sjOrBombs x && plasma x

mqbTraversal :: Difficulty -> Map ItemName Int -> Bool
mqbTraversal diff x = case diff of
    Easy -> containsAll x [SpiderBall, MorphBall, SpaceJumpBoots, GrappleBeam]
    Medium -> containsAll x [SpiderBall, MorphBall, GrappleBeam]
    Hard -> containsAll x [SpiderBall, MorphBall, GrappleBeam] || sj x
    VeryHard -> containsAll x [SpiderBall, MorphBall, GrappleBeam] || sjOrBombs x
    Expert -> containsAll x [SpiderBall, MorphBall, GrappleBeam] || sjOrBombs x

ppcBottomClimb :: Difficulty -> Map ItemName Int -> Bool
ppcBottomClimb diff x = case diff of
    Easy -> containsAll x [SpaceJumpBoots, PlasmaBeam, SpiderBall, MorphBall]
    Medium -> containsAll x [SpaceJumpBoots, PlasmaBeam, SpiderBall, MorphBall]
    Hard -> containsAll x [SpaceJumpBoots, PlasmaBeam]
    VeryHard -> sjOrBombs x && plasma x
    Expert -> sjOrBombs x && plasma x

eliteQuarters :: Difficulty -> Map ItemName Int -> Bool
eliteQuarters _ x = contains x XRayVisor

eliteQuartersPlasma :: Difficulty -> Map ItemName Int -> Bool
eliteQuartersPlasma diff x = contains x PlasmaBeam && eliteQuarters diff x

mqbBackClimb :: Difficulty -> Map ItemName Int -> Bool
mqbBackClimb diff x = case diff of
    Easy -> containsAll x [SpaceJumpBoots, PlasmaBeam]
    Medium -> plasma x && sjOrBombs x
    Hard -> plasma x && sjOrBombs x
    VeryHard -> plasma x && sjOrBombs x
    Expert -> plasma x && sjOrBombs x

mqbSw :: Difficulty -> Map ItemName Int -> Bool
mqbSw diff x = case diff of
    Easy -> False
    Medium -> False
    Hard -> sj x && bombs x
    VeryHard -> bombs x
    Expert -> bombs x