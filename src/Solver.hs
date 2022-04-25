module Solver where

import Node
import State
import Util
import Predicates

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set, toList, fromList)
import qualified Data.Set as Set

isCompletable :: Map Id Node -> Bool
isCompletable graph = let Just startingRoom = Map.lookup (R OLandingSite) graph
                        in isCompletableHelper graph (State Map.empty startingRoom Set.empty)

isCompletableHelper :: Map Id Node -> State -> Bool
isCompletableHelper graph currState = 
    let newState = collectFreeItems graph currState;
        maybeCandidate = getBestCandidate graph newState;
    in 
        isComplete graph newState || case maybeCandidate of 
                                        Nothing -> False
                                        Just candidate -> isCompletableHelper graph (state candidate)

isComplete :: Map Id Node -> State -> Bool
isComplete graph (State inventory (Room roomId edges) collectedItems) = 
    let artifactTempleItem = getVal (Map.lookup (I ArtifactTemple) graph) "Missing Item ArtifactTemple"
    in complete inventory && isAccessible graph roomId OArtifactTemple inventory 
    -- Either you collected Artifact Temple or you can collect it and return
    && (Set.member ArtifactTemple collectedItems || isAccessible graph (warp artifactTempleItem) OArtifactTemple inventory)
isComplete _ _ = error "invalid args for isComplete"

-- Try some warp chains and return the best state we could reach
getBestCandidate :: Map Id Node -> State -> Maybe CandidateState
getBestCandidate graph state = getBestCandidateHelper graph (getAccessibleItems graph state) state 1 []

getBestCandidateHelper :: Map Id Node -> [Node] -> State -> Int -> [ItemName] -> Maybe CandidateState
getBestCandidateHelper _ [] _ _ _ = Nothing
getBestCandidateHelper graph (item:rest) currState depth newItems = 
    let Item itemId itemName warp = item
        State inventory (Room roomId edges) collectedItems = currState
        newRoom = getVal (Map.lookup (R warp) graph) ("Missing Room " ++ show warp)
        newInventory = addItem itemName inventory
        newState = State newInventory newRoom (Set.insert itemId collectedItems) 
        accessibleItems = getAccessibleItems graph newState
        numAccessibleItems = length accessibleItems
        belowDepthLimit = (numAccessibleItems > 8 && depth <= 2)  || (numAccessibleItems <= 8 && depth <= 5)
        recurseItemList = getBestCandidateHelper graph rest currState depth newItems 
        recurseDeeper = getBestCandidateHelper graph accessibleItems newState (depth+1) (itemName:newItems) 
        candidate = Just (CandidateState newState depth (itemName:newItems))
        warpCanAccessStart = isAccessible graph warp OLandingSite newInventory
        startCanAccessWarp = isAccessible graph OLandingSite warp newInventory
    in
        if warpCanAccessStart && startCanAccessWarp
            then (if containsUpgrade (itemName:newItems) newInventory then minMaybe candidate recurseItemList else recurseItemList)
        else if warpCanAccessStart && containsUpgrade (itemName:newItems) newInventory
            then (if belowDepthLimit then minMaybe candidate (minMaybe recurseItemList recurseDeeper) else minMaybe candidate recurseItemList)
        else
            (if belowDepthLimit then minMaybe recurseItemList recurseDeeper else recurseItemList)

-- TODO may want to add Artifacts as progressing items
-- Also may want to add < 5 e tanks and < 8 missiles
-- Also may want to add triggers
containsUpgrade :: [ItemName] -> Map ItemName Int -> Bool
containsUpgrade newItems inventory = let previousInventory = removeAll inventory newItems 
                                    in listContainsAny newItems [MorphBall,SpaceJumpBoots,GrappleBeam,WaveBeam,IceBeam,PlasmaBeam
                                                            ,ChargeBeam,XRayVisor,PhazonSuit,GravitySuit] -- These are always an improvement
                                    || (listContains newItems SpiderBall && contains previousInventory MorphBall)
                                    || (listContains newItems MorphBallBomb && contains previousInventory MorphBall)
                                    || (listContains newItems BoostBall && contains previousInventory MorphBall)
                                    || (not (supers previousInventory) && supers inventory)
                                    || (listContains newItems Missile && not (contains previousInventory Missile))
                                    || (listContains newItems PowerBomb && (contains previousInventory MorphBall && not (contains previousInventory PowerBomb)))
                                    || (listContains newItems VariaSuit && not (containsAny previousInventory [PhazonSuit,GravitySuit]))

collectFreeItems :: Map Id Node -> State -> State
collectFreeItems graph state = collectFreeItemsHelper graph (getAccessibleItems graph state) state

collectFreeItemsHelper :: Map Id Node -> [Node] -> State -> State
collectFreeItemsHelper _ [] currState = currState
collectFreeItemsHelper graph (item:rest) currState = 
    let (State inventory (Room roomId edges) collectedItems) = currState
        Item itemId itemName warp = item
        newInventory = addItem itemName inventory
        newRoom = getVal (Map.lookup (R warp) graph) ("Missing Room " ++ show warp)
        newState = State newInventory (Room roomId edges) (Set.insert itemId collectedItems) 
    in 
        -- If we can reach our starting location, then the warp is not useful, so collecting the item has no cost
        if isMutuallyAccessible graph warp roomId newInventory
            then collectFreeItemsHelper graph (getAccessibleItems graph newState) newState
        else 
            collectFreeItemsHelper graph rest currState

isMutuallyAccessible :: Map Id Node -> RoomId -> RoomId -> Map ItemName Int -> Bool
isMutuallyAccessible graph room1 room2 inventory = isAccessible graph room1 room2 inventory && isAccessible graph room2 room1 inventory

isAccessible :: Map Id Node -> RoomId -> RoomId -> Map ItemName Int -> Bool
isAccessible graph fromRoom = isAccessibleHelper graph [fromRoom] [] 

isAccessibleHelper :: Map Id Node -> [RoomId] -> [RoomId] -> RoomId -> Map ItemName Int -> Bool
isAccessibleHelper _ [] _ _ _ = False
isAccessibleHelper graph (roomId:rest) checkedRooms destination inventory = 
    roomId == destination || case Map.lookup (R roomId) graph of
                            Nothing -> error ("Missing Room " ++ show roomId)
                            Just (Room _ edges) -> let predicates = map canUse edges;
                                                        nodeIds = map nodeId edges;
                                                        bools = eval predicates inventory;
                                                        roomIds = getRoomIds (checkBools nodeIds bools);
                                                        uncheckedRoomIds = roomIds \\ checkedRooms
                                                in isAccessibleHelper graph (uncheckedRoomIds ++ rest) (roomId:checkedRooms) destination inventory

getAccessibleItems :: Map Id Node -> State -> [Node]
getAccessibleItems graph (State inventory (Room roomId edges) collectedItems) = 
    let itemIds = getAccessibleItemsHelper graph [roomId] [] inventory collectedItems
        uniqueItemIds = Data.Set.toList (Data.Set.fromList itemIds) -- Remove duplicates
        maybeItems = mapM (((\ x -> x graph) . Map.lookup) . I) uniqueItemIds -- Result is Maybe [Node] with all of the item nodes
        in case maybeItems of 
                Nothing -> error "Missing Item"
                Just items -> items

getAccessibleItemsHelper :: Map Id Node -> [RoomId] -> [RoomId] -> Map ItemName Int -> Set ItemId -> [ItemId]
getAccessibleItemsHelper _ [] _ _ _ = []
getAccessibleItemsHelper graph (roomId:rest) checkedRooms inventory collectedItems = 
    case Map.lookup (R roomId) graph of
        Nothing -> error ("Missing Room " ++ show roomId)
        Just (Room _ edges) -> let predicates = map canUse edges;
                                    nodeIds = map nodeId edges;
                                    bools = eval predicates inventory;
                                    reachableNodeIds = checkBools nodeIds bools;
                                    roomIds = getRoomIds reachableNodeIds;
                                    itemIds = getItemIds reachableNodeIds;
                                    uncheckedRoomIds = roomIds \\ checkedRooms;
                                    uncollectedItemIds = removeSet itemIds collectedItems;
                                in uncollectedItemIds ++ getAccessibleItemsHelper graph (uncheckedRoomIds ++ rest) (roomId:checkedRooms) inventory collectedItems                            
