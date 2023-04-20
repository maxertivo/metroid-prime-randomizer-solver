module Solver where

import Node
import Predicates
import State
import Util

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set, empty)
import qualified Data.Set as Set
import Graph

isCompletable :: Map Id Node -> Bool
isCompletable graph =
    isCompletableHelper graph (State Map.empty OLandingSite Set.empty)

isCompletableHelper :: Map Id Node -> State -> Bool
isCompletableHelper graph currState =
    let newState = collectFreeItems graph currState
        maybeCandidate = getBestCandidate graph newState
     in isComplete graph newState ||
        case maybeCandidate of
            Nothing -> False
            Just candidate -> isCompletableHelper graph (state candidate)

isComplete :: Map Id Node -> State -> Bool
isComplete graph (State inventory roomId collectedItems) =
    let artifactTempleItem = getVal (Map.lookup (I ArtifactTemple) graph) "Missing Item ArtifactTemple"
     in complete inventory collectedItems &&
        isAccessible graph roomId OArtifactTemple inventory collectedItems &&
        -- Either you collected Artifact Temple or you can collect it and return
        (Set.member ArtifactTemple collectedItems || isAccessible graph (warp artifactTempleItem) OArtifactTemple inventory collectedItems)

-- Try some warp chains and return the best state we could reach
getBestCandidate :: Map Id Node -> State -> Maybe CandidateState
getBestCandidate graph state = 
    let candidates = getAllCandidates graph (getAccessibleItems graph state) state 1 []
     in case sort candidates of
        [] -> Nothing
        (best:_) -> Just best

getAllCandidates :: Map Id Node -> [Node] -> State -> Int -> [ItemName] -> [CandidateState]
getAllCandidates _ [] _ _ _ = []
getAllCandidates _ (Room {}:_) _ _ _ = error "invalid argument - list includes room node"
getAllCandidates graph (item:rest) currState depth newItems =
    let Item itemId itemName warp = item
        State inventory _ collectedItems = currState
        newInventory = addItem itemName inventory
        newIds = Set.insert itemId collectedItems
        newState = State newInventory warp newIds
        accessibleItems = getAccessibleItems graph newState
        accessibleItemsInaccessibleFromStart = filter (`notElem` getAccessibleItems graph (State newInventory OLandingSite newIds)) accessibleItems
        numAccessibleItems = length accessibleItems
        belowDepthLimit
            | numAccessibleItems > 8 = depth <= 2
            | numAccessibleItems > 2 = depth <= 5
            | otherwise = True -- If there's only one or two items reachable, we can continue the chain until that is no longer the case
        recurseItemList = getAllCandidates graph rest currState depth newItems
        recurseDeeper = getAllCandidates graph accessibleItems newState (depth + 1) (itemName : newItems)
        recurseDeeperLimitSearch = getAllCandidates graph accessibleItemsInaccessibleFromStart newState (depth + 1) (itemName : newItems)
        candidate = CandidateState newState depth (itemName : newItems)
        warpCanAccessStart = isAccessible graph warp OLandingSite newInventory newIds
        startCanAccessWarp = isAccessible graph OLandingSite warp newInventory newIds
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
        listContains newItems EnergyTank && not (containsCount 6 EnergyTank previousInventory) || 
        listContains newItems Missile && not (containsCount 8 Missile previousInventory)

collectFreeItems :: Map Id Node -> State -> State
collectFreeItems graph state = collectFreeItemsHelper graph (getAccessibleItems graph state) state

collectFreeItemsHelper :: Map Id Node -> [Node] -> State -> State
collectFreeItemsHelper _ (Room {}:_) _ = error "invalid argument - list includes room node"
collectFreeItemsHelper _ [] currState = currState
collectFreeItemsHelper graph (item:rest) currState =
    let (State inventory roomId collectedItems) = currState
        Item itemId itemName warp = item
        newInventory = addItem itemName inventory
        newIds = Set.insert itemId collectedItems
        newState = State newInventory warp newIds
     in if isMutuallyAccessible graph warp roomId newInventory newIds -- Check to make sure the warp is not useful, so that collecting the item has no cost
            && itemId /= ElderChamber -- This warp is needed to exit if warped to Elder Chamber, so it is delayed until getBestCandidate is called
            then if itemName `elem` [Missile, EnergyTank, Artifact] && missile inventory collectedItems
                    then collectFreeItemsHelper graph rest newState -- We got a non-upgrade item, and it warped us somewhere mutually accessible, so we don't need to calculate accessible items again
                    else collectFreeItemsHelper graph (getAccessibleItems graph newState) newState -- Recalculate accessible items since we got an upgrade
            else collectFreeItemsHelper graph rest currState

isMutuallyAccessible :: Map Id Node -> RoomId -> RoomId -> Map ItemName Int -> Set ItemId -> Bool
isMutuallyAccessible graph room1 room2 inventory itemIds = isAccessible graph room1 room2 inventory itemIds && isAccessible graph room2 room1 inventory itemIds

isAccessible :: Map Id Node -> RoomId -> RoomId -> Map ItemName Int -> Set ItemId -> Bool
isAccessible graph fromRoom = isAccessibleHelper graph [fromRoom] []

isAccessibleHelper :: Map Id Node -> [RoomId] -> [RoomId] -> RoomId -> Map ItemName Int -> Set ItemId -> Bool
isAccessibleHelper _ [] _ _ _ _ = False
isAccessibleHelper graph (roomId:rest) checkedRooms destination inventory itemIds =
    roomId == destination ||
    case Map.lookup (R roomId) graph of
        Just (Room _ edges) ->
            let predicates = map predicate edges
                nodeIds = map nodeId edges
                bools = eval2 predicates inventory itemIds
                roomIds = getRoomIds (checkBools nodeIds bools)
                uncheckedRoomIds = filter (`notElem` checkedRooms) roomIds 
             in isAccessibleHelper graph (uncheckedRoomIds ++ rest) (roomId : checkedRooms) destination inventory itemIds
        _ -> error ("Missing or incorrect Room " ++ show roomId)

getAccessibleItems :: Map Id Node -> State -> [Node]
getAccessibleItems graph (State inventory roomId collectedItems) =
    let itemIds = getAccessibleItemsHelper graph [roomId] [] inventory collectedItems []
        uniqueItemIds = nub itemIds -- Remove duplicates
        maybeItems = mapM (((\x -> x graph) . Map.lookup) . I) uniqueItemIds
     in case maybeItems of
            Nothing -> error "Missing Item"
            Just items -> items

getAccessibleItemsHelper :: Map Id Node -> [RoomId] -> [RoomId] -> Map ItemName Int -> Set ItemId -> [ItemId] -> [ItemId]
getAccessibleItemsHelper _ [] _ _ _ result = result
getAccessibleItemsHelper graph (roomId:rest) checkedRooms inventory collectedItems result =
    case Map.lookup (R roomId) graph of
        Just (Room _ edges) ->
            let predicates = map predicate edges
                nodeIds = map nodeId edges
                bools = eval2 predicates inventory collectedItems
                reachableNodeIds = checkBools nodeIds bools
                roomIds = getRoomIds reachableNodeIds
                itemIds = getItemIds reachableNodeIds
                uncheckedRoomIds = filter (`notElem` checkedRooms) roomIds 
                uncollectedItemIds = removeSet itemIds collectedItems
             in getAccessibleItemsHelper graph (uncheckedRoomIds ++ rest) (roomId : checkedRooms) inventory collectedItems (uncollectedItemIds ++ result)
        _ -> error ("Missing or incorrect Room " ++ show roomId)
