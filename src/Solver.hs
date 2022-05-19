module Solver where

import Node
import Predicates
import State
import Util

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set, empty, fromList, toList)
import qualified Data.Set as Set

isCompletable :: Map Id Node -> Bool
isCompletable graph =
    case Map.lookup (R OLandingSite) graph of
        Just startingRoom -> isCompletableHelper graph (State Map.empty startingRoom Set.empty)
        Nothing -> error "Missing start room"

isCompletableHelper :: Map Id Node -> State -> Bool
isCompletableHelper graph currState =
    let newState = collectFreeItems graph currState
        maybeCandidate = getBestCandidate graph newState
     in isComplete graph newState ||
        case maybeCandidate of
            Nothing -> False
            Just candidate -> isCompletableHelper graph (state candidate)

isComplete :: Map Id Node -> State -> Bool
isComplete graph (State inventory (Room roomId _) collectedItems) =
    let artifactTempleItem = getVal (Map.lookup (I ArtifactTemple) graph) "Missing Item ArtifactTemple"
     in complete inventory collectedItems &&
        isAccessible graph roomId OArtifactTemple inventory collectedItems &&
        -- Either you collected Artifact Temple or you can collect it and return
        (Set.member ArtifactTemple collectedItems || isAccessible graph (warp artifactTempleItem) OArtifactTemple inventory collectedItems)
isComplete _ _ = error "invalid args for isComplete"

-- Try some warp chains and return the best state we could reach
getBestCandidate :: Map Id Node -> State -> Maybe CandidateState
getBestCandidate graph state = getBestCandidateHelper graph (getAccessibleItems graph state) state 1 []

getBestCandidateHelper :: Map Id Node -> [Node] -> State -> Int -> [ItemName] -> Maybe CandidateState
getBestCandidateHelper _ [] _ _ _ = Nothing
getBestCandidateHelper _ (Room {}:_) _ _ _ = error "invalid argument - list includes room node"
getBestCandidateHelper graph (item:rest) currState depth newItems =
    let Item itemId itemName warp = item
        State inventory _ collectedItems = currState
        newRoom = getVal (Map.lookup (R warp) graph) ("Missing Room " ++ show warp)
        newInventory = addItem itemName inventory
        newIds = Set.insert itemId collectedItems
        newState = State newInventory newRoom newIds
        accessibleItems = getAccessibleItems graph newState
        numAccessibleItems = length accessibleItems
        belowDepthLimit
            | numAccessibleItems > 8 = depth <= 2
            | numAccessibleItems > 2 = depth <= 5
            | otherwise = True -- If there's only one or two items reachable, we can continue the chain until that is no longer the case
        recurseItemList = getBestCandidateHelper graph rest currState depth newItems
        recurseDeeper = getBestCandidateHelper graph accessibleItems newState (depth + 1) (itemName : newItems)
        candidate = Just (CandidateState newState depth (itemName : newItems))
        warpCanAccessStart = isAccessible graph warp OLandingSite newInventory collectedItems
        startCanAccessWarp = isAccessible graph OLandingSite warp newInventory collectedItems
     in if warpCanAccessStart && startCanAccessWarp
            then if containsUpgrade (itemName : newItems) newInventory
                      then minMaybe candidate recurseItemList
                      else recurseItemList
            else if warpCanAccessStart && containsUpgrade (itemName : newItems) newInventory
                     then if belowDepthLimit
                               then minMaybe candidate (minMaybe recurseItemList recurseDeeper)
                               else minMaybe candidate recurseItemList
                     else if belowDepthLimit
                               then minMaybe recurseItemList recurseDeeper
                               else recurseItemList

containsUpgrade :: [ItemName] -> Map ItemName Int -> Bool
containsUpgrade newItems inventory =
    let previousInventory = removeAll inventory newItems
        ids = Data.Set.empty
    in listContainsAny newItems [MorphBall,SpaceJumpBoots,GrappleBeam,WaveBeam,IceBeam,PlasmaBeam
                            ,ChargeBeam,XRayVisor,PhazonSuit,GravitySuit,Artifact] || -- These items are always an improvement
        (not (spider previousInventory ids) && spider inventory ids) ||
        (not (bombs previousInventory ids) && bombs inventory ids) ||
        (not (boost previousInventory ids) && boost inventory ids) ||
        (not (supers previousInventory ids) && supers inventory ids) ||
        (not (pb previousInventory ids) && pb inventory ids) ||
        (not (heatResist previousInventory ids) && heatResist inventory ids) ||
        listContains newItems EnergyTank && not (containsCount 6 EnergyTank previousInventory) || 
        listContains newItems Missile && not (containsCount 8 Missile previousInventory)

collectFreeItems :: Map Id Node -> State -> State
collectFreeItems graph state = collectFreeItemsHelper graph (getAccessibleItems graph state) state

collectFreeItemsHelper :: Map Id Node -> [Node] -> State -> State
collectFreeItemsHelper _ (Room {}:_) _ = error "invalid argument - list includes room node"
collectFreeItemsHelper _ _ (State _ (Item {}) _) = error "invalid state"
collectFreeItemsHelper _ [] currState = currState
collectFreeItemsHelper graph (item:rest) currState =
    let (State inventory (Room roomId _) collectedItems) = currState
        Item itemId itemName warp = item
        newInventory = addItem itemName inventory
        newRoom = getVal (Map.lookup (R warp) graph) ("Missing Room " ++ show warp)
        newState = State newInventory newRoom (Set.insert itemId collectedItems)
     in if isMutuallyAccessible graph warp roomId newInventory collectedItems -- Check to make sure the warp is not useful, so that collecting the item has no cost
            && itemId /= ElderChamber -- This warp is needed to exit if warped to Elder Chamber, so it is delayed until getBestCandidate is called
            then collectFreeItemsHelper graph (getAccessibleItems graph newState) newState
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
                uncheckedRoomIds = roomIds \\ checkedRooms
             in isAccessibleHelper graph (uncheckedRoomIds ++ rest) (roomId : checkedRooms) destination inventory itemIds
        _ -> error ("Missing or incorrect Room " ++ show roomId)

getAccessibleItems :: Map Id Node -> State -> [Node]
getAccessibleItems _ (State _ Item {} _) = error "invalid state"
getAccessibleItems graph (State inventory (Room roomId _) collectedItems) =
    let itemIds = getAccessibleItemsHelper graph [roomId] [] inventory collectedItems
        uniqueItemIds = Data.Set.toList (Data.Set.fromList itemIds) -- Remove duplicates
        maybeItems = mapM (((\x -> x graph) . Map.lookup) . I) uniqueItemIds
     in case maybeItems of
            Nothing -> error "Missing Item"
            Just items -> items

getAccessibleItemsHelper :: Map Id Node -> [RoomId] -> [RoomId] -> Map ItemName Int -> Set ItemId -> [ItemId]
getAccessibleItemsHelper _ [] _ _ _ = []
getAccessibleItemsHelper graph (roomId:rest) checkedRooms inventory collectedItems =
    case Map.lookup (R roomId) graph of
        Just (Room _ edges) ->
            let predicates = map predicate edges
                nodeIds = map nodeId edges
                bools = eval2 predicates inventory collectedItems
                reachableNodeIds = checkBools nodeIds bools
                roomIds = getRoomIds reachableNodeIds
                itemIds = getItemIds reachableNodeIds
                uncheckedRoomIds = roomIds \\ checkedRooms
                uncollectedItemIds = removeSet itemIds collectedItems
             in uncollectedItemIds ++ getAccessibleItemsHelper graph (uncheckedRoomIds ++ rest) (roomId : checkedRooms) inventory collectedItems
        _ -> error ("Missing or incorrect Room " ++ show roomId)
