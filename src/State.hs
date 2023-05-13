module State where

import Node
import Util
import Data.List
import Data.Map (Map)
import Data.Set (Set)

data State = State {inventory :: Map ItemName Int, currentNode :: Int, collectedItems :: Set Int}
            deriving (Show)
data CandidateState = CandidateState{state :: State, depth :: Int, newItems :: [ItemName]}
            deriving (Show)

{-- Sorts from best to worst, so the better candidate is actually the "lesser" one --}
instance Ord CandidateState where
    compare (CandidateState _ b c) (CandidateState _ e f)
        | countOf [WaveBeam, IceBeam, PlasmaBeam] c > countOf [WaveBeam, IceBeam, PlasmaBeam] f = LT
        | countOf [WaveBeam, IceBeam, PlasmaBeam] c < countOf [WaveBeam, IceBeam, PlasmaBeam] f = GT
        | countOf upgrades c > countOf upgrades f = LT
        | countOf upgrades c < countOf upgrades f = GT
        | b < e = LT
        | b > e = GT
        | otherwise = compare (Data.List.minimum c) (Data.List.minimum f)

instance Eq CandidateState where 
    p == q = compare p q == EQ 

upgrades :: [ItemName]
upgrades = [MorphBall,MorphBallBomb,SpaceJumpBoots,GrappleBeam,WaveBeam,IceBeam,PlasmaBeam,SpiderBall,BoostBall,ChargeBeam,SuperMissile,XRayVisor,PhazonSuit,GravitySuit,VariaSuit]