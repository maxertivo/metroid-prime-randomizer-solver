{-# LANGUAGE BangPatterns #-}
module Solver (isCompletable) where

import Node
import Predicates
import State
import Util

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Set (Set, empty)
import qualified Data.Set as Set
import Graph

isCompletable :: IntMap Room -> IntMap Item -> Bool
isCompletable !roomMap !itemMap =
    isCompletableHelper roomMap itemMap (State Map.empty (fromEnum OLandingSite) Set.empty)

isCompletableHelper :: IntMap Room -> IntMap Item -> State -> Bool
isCompletableHelper roomMap itemMap currState =
    let !newState = collectFreeItems roomMap itemMap currState
        maybeCandidate = getBestCandidate roomMap itemMap newState
     in isComplete roomMap itemMap newState ||
        case maybeCandidate of
            Nothing -> False
            Just candidate -> isCompletableHelper roomMap itemMap (state candidate)

isComplete :: IntMap Room -> IntMap Item -> State -> Bool
isComplete roomMap itemMap (State inventory roomId collectedItems) =
    let artifactTempleItem = getVal (IntMap.lookup (fromEnum ArtifactTemple) itemMap) "Missing Item ArtifactTemple"
     in complete inventory collectedItems &&
        isAccessible roomMap roomId (fromEnum OArtifactTemple) inventory collectedItems &&
        -- Either you collected Artifact Temple or you can collect it and return
        (Set.member (fromEnum ArtifactTemple) collectedItems || isAccessible roomMap (warp artifactTempleItem) (fromEnum OArtifactTemple) inventory collectedItems)

-- Try some warp chains and return the best state we could reach
getBestCandidate :: IntMap Room -> IntMap Item -> State -> Maybe CandidateState
getBestCandidate roomMap itemMap state = 
    let candidates = getAllCandidates roomMap itemMap (getAccessibleItems roomMap itemMap state) state 1 []
     in case candidates of
        [] -> Nothing
        _ -> Just $ minimum candidates

getAllCandidates :: IntMap Room -> IntMap Item -> [Item] -> State -> Int -> [ItemName] -> [CandidateState]
getAllCandidates _ _ [] _ _ _ = []
getAllCandidates roomMap itemMap (item:rest) currState depth newItems =
    let Item itemId itemName warp = item
        State inventory _ collectedItems = currState
        newInventory = addItem itemName inventory
        newIds = Set.insert itemId collectedItems
        newState = State newInventory warp newIds
        accessibleItems = getAccessibleItems roomMap itemMap newState
        accessibleItemsInaccessibleFromStart = filter (`notElem` getAccessibleItems roomMap itemMap (State newInventory (fromEnum OLandingSite) newIds)) accessibleItems
        numAccessibleItems = length accessibleItems
        belowDepthLimit = numAccessibleItems <= 4 || depth <= 2
        recurseItemList = getAllCandidates roomMap itemMap rest currState depth newItems
        recurseDeeper = getAllCandidates roomMap itemMap accessibleItems newState (depth + 1) (itemName : newItems)
        recurseDeeperLimitSearch = getAllCandidates roomMap itemMap accessibleItemsInaccessibleFromStart newState (depth + 1) (itemName : newItems)
        candidate = CandidateState newState depth (itemName : newItems)
        warpCanAccessStart = isAccessible roomMap warp (fromEnum OLandingSite) newInventory newIds
        startCanAccessWarp = isAccessible roomMap (fromEnum OLandingSite) warp newInventory newIds
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

collectFreeItems :: IntMap Room -> IntMap Item -> State -> State
collectFreeItems roomMap itemMap state = collectFreeItemsHelper roomMap itemMap (getAccessibleItems roomMap itemMap state) state

collectFreeItemsHelper :: IntMap Room -> IntMap Item -> [Item] -> State -> State
collectFreeItemsHelper _ _ [] currState = currState
collectFreeItemsHelper roomMap itemMap (item:rest) currState =
    let (State inventory roomId collectedItems) = currState
        Item itemId itemName warp = item
        newInventory = addItem itemName inventory
        newIds = Set.insert itemId collectedItems
        newState = State newInventory warp newIds
     in if isMutuallyAccessible roomMap warp roomId newInventory newIds -- Check to make sure the warp is not useful, so that collecting the item has no cost
            then if itemName `elem` [Missile, EnergyTank, Artifact] && missile inventory collectedItems
                    -- We got a non-upgrade item, and it warped us somewhere mutually accessible, so we don't need to calculate accessible items again
                    then collectFreeItemsHelper roomMap itemMap rest newState
                    -- Recalculate accessible items since we got an upgrade
                    else collectFreeItemsHelper roomMap itemMap (getAccessibleItems roomMap itemMap newState) newState 
            -- Not a free item
            else collectFreeItemsHelper roomMap itemMap rest currState

isMutuallyAccessible :: IntMap Room -> Int -> Int -> Map ItemName Int -> Set Int -> Bool
isMutuallyAccessible !roomMap !room1 !room2 !inventory !itemIds = isAccessible roomMap room1 room2 inventory itemIds && isAccessible roomMap room2 room1 inventory itemIds

isAccessible :: IntMap Room -> Int -> Int -> Map ItemName Int -> Set Int -> Bool
isAccessible !roomMap !fromRoom = isAccessibleHelper roomMap [fromRoom]

isAccessibleHelper :: IntMap Room -> [Int] -> Int -> Map ItemName Int -> Set Int -> Bool
isAccessibleHelper _ [] _ _ _ = False
isAccessibleHelper !roomMap (roomId:rest) !destination !inventory !itemIds =
    roomId == destination ||
    case IntMap.lookup roomId roomMap of
        Just (Room _ edges _) ->
            let predicates = map predicate edges
                roomIds = map room edges
                bools = eval2 predicates inventory itemIds
                reachableRoomIds = checkBools roomIds bools
             in isAccessibleHelper (IntMap.delete roomId roomMap) (reachableRoomIds ++ rest) destination inventory itemIds
        Nothing -> isAccessibleHelper roomMap rest destination inventory itemIds

getAccessibleItems :: IntMap Room -> IntMap Item -> State -> [Item]
getAccessibleItems !roomMap !itemMap (State inventory roomId collectedItems) =
    let itemIds = getAccessibleItemsHelper roomMap [roomId] inventory collectedItems []
        uniqueItemIds = nub itemIds -- Remove duplicates
        maybeItems = mapM ((\x -> x itemMap) . IntMap.lookup) uniqueItemIds
     in case maybeItems of
            Nothing -> error "Missing Item"
            Just items -> items

getAccessibleItemsHelper :: IntMap Room -> [Int] -> Map ItemName Int -> Set Int -> [Int] -> [Int]
getAccessibleItemsHelper _ [] _ _ result = result
getAccessibleItemsHelper !roomMap (roomId:rest) !inventory !collectedItems result =
    case IntMap.lookup roomId roomMap of
        Just (Room _ edges itemEdges) ->
            let predicates = map predicate edges
                itemPredicates = map itemPredicate itemEdges
                roomIds = map room edges
                itemIds = map item itemEdges
                bools = eval2 predicates inventory collectedItems
                itemBools = eval2 itemPredicates inventory collectedItems
                reachableRoomIds = checkBools roomIds bools
                reachableItemIds = checkBools itemIds itemBools
                uncollectedItemIds = removeSet reachableItemIds collectedItems
             in getAccessibleItemsHelper (IntMap.delete roomId roomMap) (reachableRoomIds ++ rest) inventory collectedItems (uncollectedItemIds ++ result)
        Nothing -> getAccessibleItemsHelper roomMap rest inventory collectedItems result
