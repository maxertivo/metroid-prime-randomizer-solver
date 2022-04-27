module State where

import Node
import Util
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

data State = State {inventory :: Map ItemName Int, currentNode :: Node, collectedItems :: Set ItemId}
            deriving (Show)
data CandidateState = CandidateState{state :: State, depth :: Int, newPickups :: [Pickup]}
            deriving (Show)

{-- Sorts from best to worst, so the better candidate is actually the "lesser" one --}
instance Ord CandidateState where
    compare p@(CandidateState a b c) q@(CandidateState d e f)
        | countOf (map snd c) [WaveBeam, IceBeam, PlasmaBeam] > countOf (map snd f) [WaveBeam, IceBeam, PlasmaBeam] = LT
        | countOf (map snd c) [WaveBeam, IceBeam, PlasmaBeam] < countOf (map snd f) [WaveBeam, IceBeam, PlasmaBeam] = GT
        | countOf (map snd c) upgrades > countOf (map snd f) upgrades = LT
        | countOf (map snd c) upgrades < countOf (map snd f) upgrades = GT
        | b < e = LT
        | b > e = GT
        | otherwise = compare (Data.List.minimum (map snd c)) (Data.List.minimum (map snd f))

instance Eq CandidateState where 
    p@(CandidateState a b c) == q@(CandidateState d e f) = compare p q == EQ 

upgrades :: [ItemName]
upgrades = [MorphBall, MorphBallBomb,SpaceJumpBoots,GrappleBeam,WaveBeam,IceBeam,PlasmaBeam,SpiderBall,BoostBall,ChargeBeam,SuperMissile,XRayVisor,PhazonSuit,GravitySuit,VariaSuit]