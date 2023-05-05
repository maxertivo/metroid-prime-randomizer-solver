module Solver (isCompletable) where

import Node
import Predicates
import State
import Util
import DirectNode

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Set (Set, empty)
import qualified Data.Set as Set
import Graph

isCompletable ::(IntMap DirectRoom, IntMap DirectItem) -> Bool
isCompletable (directRooms,directItems) =
    let startRoom = intMapLookup (fromEnum OLandingSite) directRooms
        artifactTemple =  intMapLookup (fromEnum ArtifactTemple) directItems
    in  isCompletableHelper startRoom artifactTemple (State Map.empty startRoom Set.empty)

isCompletableHelper :: DirectRoom -> DirectItem -> State -> Bool
isCompletableHelper startRoom artifactTemple currState =
    let newState = collectFreeItems currState
        maybeCandidate = getBestCandidate startRoom newState
     in isComplete newState artifactTemple ||
        case maybeCandidate of
            Nothing -> False
            Just candidate -> isCompletableHelper startRoom artifactTemple (state candidate)

isComplete :: State -> DirectItem -> Bool
isComplete (State inventory room collectedItems) artifactTempleItem =
    complete inventory collectedItems &&
    isAccessible room OArtifactTemple inventory collectedItems &&
    -- Either you collected Artifact Temple or you can collect it and return
    (Set.member ArtifactTemple collectedItems || isAccessible (DirectNode.warp artifactTempleItem) OArtifactTemple inventory collectedItems)

-- Try some warp chains and return the best state we could reach
getBestCandidate :: DirectRoom -> State -> Maybe CandidateState
getBestCandidate startRoom state = 
    let candidates = getAllCandidates (getAccessibleItems state) state 1 [] startRoom
     in case candidates of
        [] -> Nothing
        _ -> Just $ minimum candidates

getAllCandidates :: [DirectItem] -> State -> Int -> [ItemName] -> DirectRoom -> [CandidateState]
getAllCandidates [] _ _ _ _ = []
getAllCandidates (DirectItem itemId itemName warp : rest) currState depth newItems startRoom =
    let State inventory _ collectedItems = currState
        newInventory = addItem itemName inventory
        newIds = Set.insert itemId collectedItems
        newState = State newInventory warp newIds
        accessibleItems = getAccessibleItems newState
        accessibleItemsInaccessibleFromStart = filter (`notElem` getAccessibleItems (State newInventory startRoom newIds)) accessibleItems
        numAccessibleItems = length accessibleItems
        belowDepthLimit = numAccessibleItems <= 4 || depth <= 2
        recurseItemList = getAllCandidates rest currState depth newItems startRoom
        recurseDeeper = getAllCandidates accessibleItems newState (depth + 1) (itemName : newItems) startRoom
        recurseDeeperLimitSearch = getAllCandidates accessibleItemsInaccessibleFromStart newState (depth + 1) (itemName : newItems) startRoom
        candidate = CandidateState newState depth (itemName : newItems)
        warpCanAccessStart = isAccessible warp (DirectNode.roomId startRoom) newInventory newIds
        startCanAccessWarp = isAccessible startRoom (DirectNode.roomId warp) newInventory newIds
     in if warpCanAccessStart && startCanAccessWarp
            then if containsUpgrade (itemName : newItems) newInventory
                      then candidate : recurseItemList   -- We have a candidate and can end this warp chain
                      else recurseItemList               -- Not a valid candidate
            else if warpCanAccessStart && containsUpgrade (itemName : newItems) newInventory
                     then if belowDepthLimit
                               -- We have a candidate, but we can't return here, so continue this warp chain while only checking items we can't reach from start
                               then candidate : (recurseItemList ++ recurseDeeperLimitSearch)
                               -- We have a candidate, but we can't return here, but also the chain is too long so end it anyway
                               else candidate : recurseItemList
                     else if belowDepthLimit
                               -- Not a valid candidate, but we can't return here, so continue this the warp chain
                               then recurseItemList ++ (if warpCanAccessStart then recurseDeeperLimitSearch else recurseDeeper)  
                               -- Not a valid candidate, and the chain is too long, so end it here
                               else recurseItemList                             

containsUpgrade :: [ItemName] -> Map ItemName Int -> Bool
containsUpgrade newItems inventory =
    let previousInventory = removeAll inventory newItems
        emptySet = Data.Set.empty
    in  listContainsAny newItems [MorphBall,SpaceJumpBoots,GrappleBeam,WaveBeam,IceBeam,PlasmaBeam
                            ,ChargeBeam,XRayVisor,PhazonSuit,GravitySuit,Artifact] || -- These items are always an improvement
        listContainsAny newItems pseudoItemNames ||
        (not (spider previousInventory emptySet) && spider inventory emptySet) ||
        (not (bombs previousInventory emptySet) && bombs inventory emptySet) ||
        (not (boost previousInventory emptySet) && boost inventory emptySet) ||
        (not (supers previousInventory emptySet) && supers inventory emptySet) ||
        (not (pb previousInventory emptySet) && pb inventory emptySet) ||
        (not (heatResist previousInventory emptySet) && heatResist inventory emptySet) ||
        (not (contains inventory PhazonSuit) && listContains newItems EnergyTank && not (containsCount 10 EnergyTank previousInventory)) || 
        listContains newItems Missile && not (containsCount 8 Missile previousInventory)

collectFreeItems :: State -> State
collectFreeItems state = collectFreeItemsHelper (getAccessibleItems state) state

collectFreeItemsHelper :: [DirectItem] -> State -> State
collectFreeItemsHelper [] currState = currState
collectFreeItemsHelper (DirectItem itemId itemName warp:rest) currState =
    let (State inventory room collectedItems) = currState
        newInventory = addItem itemName inventory
        newIds = Set.insert itemId collectedItems
        newState = State newInventory warp newIds
     in if isMutuallyAccessible warp room newInventory newIds -- Check to make sure the warp is not useful, so that collecting the item has no cost
            then if itemName `elem` [Missile, EnergyTank, Artifact] && missile inventory collectedItems
                    -- We got a non-upgrade item, and it warped us somewhere mutually accessible, so we don't need to calculate accessible items again
                    then collectFreeItemsHelper rest newState
                    -- Recalculate accessible items since we got an upgrade
                    else collectFreeItemsHelper (getAccessibleItems newState) newState 
            -- Not a free item
            else collectFreeItemsHelper rest currState

isMutuallyAccessible :: DirectRoom -> DirectRoom -> Map ItemName Int -> Set ItemId -> Bool
isMutuallyAccessible room1 room2 inventory itemIds = isAccessible room1 (DirectNode.roomId room2) inventory itemIds && isAccessible room2 (DirectNode.roomId room1) inventory itemIds

isAccessible :: DirectRoom -> RoomId -> Map ItemName Int -> Set ItemId -> Bool
isAccessible fromRoom = isAccessibleHelper [fromRoom] []

isAccessibleHelper :: [DirectRoom] -> [RoomId] -> RoomId -> Map ItemName Int -> Set ItemId -> Bool
isAccessibleHelper [] _ _ _ _ = False
isAccessibleHelper (DirectRoom roomId roomEdges itemEdges:rest) checkedRooms destination inventory itemIds =
    roomId == destination ||
        let predicates = map DirectNode.predicate roomEdges
            rooms = map DirectNode.room roomEdges
            bools = eval2 predicates inventory itemIds
            reachableRooms = checkBools rooms bools
            uncheckedRooms = filter (\x -> DirectNode.roomId x `notElem` checkedRooms) reachableRooms
            in isAccessibleHelper (uncheckedRooms ++ rest) (roomId : checkedRooms) destination inventory itemIds

getAccessibleItems :: State -> [DirectItem]
getAccessibleItems (State inventory room collectedItems) =
    let items = getAccessibleItemsHelper [room] inventory collectedItems [] []
     in nub items -- Remove duplicates

getAccessibleItemsHelper :: [DirectRoom] -> Map ItemName Int -> Set ItemId -> [DirectRoom] -> [DirectItem] -> [DirectItem]
getAccessibleItemsHelper [] _ _ _ result = result
getAccessibleItemsHelper (DirectRoom roomId roomEdges itemEdges : rest) inventory collectedItems checkedRooms result =
            let predicates = map DirectNode.predicate roomEdges
                itemPredicates = map DirectNode.itemPredicate itemEdges
                rooms = map DirectNode.room roomEdges
                items = map DirectNode.item itemEdges
                bools = eval2 predicates inventory collectedItems
                itemBools = eval2 itemPredicates inventory collectedItems
                reachableRooms = checkBools rooms bools
                reachableItems = checkBools items itemBools
                uncheckedRooms = filter (`notElem` checkedRooms) reachableRooms
                uncollectedItems = filter (\x -> DirectNode.itemId x `Set.notMember` collectedItems) reachableItems
             in getAccessibleItemsHelper (uncheckedRooms ++ rest) inventory collectedItems (DirectRoom roomId roomEdges itemEdges : checkedRooms) (uncollectedItems ++ result)
