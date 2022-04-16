module State where
    import Node
    import Util
    import Data.List
    import Data.Map (Map)
    import qualified Data.Map as Map

    data State = State {inventory :: [ItemName], currentNode :: Node, collectedItems :: [ItemId]}
                deriving (Show)
    data CandidateState = CandidateState{state :: State, depth :: Int, newItems :: [ItemName]}
                deriving (Show)

    instance Ord CandidateState where
        compare p@(CandidateState a b c) q@(CandidateState d e f)
            | countOf c [WaveBeam, IceBeam, PlasmaBeam] > countOf f [WaveBeam, IceBeam, PlasmaBeam] = LT
            | countOf c [WaveBeam, IceBeam, PlasmaBeam] < countOf f [WaveBeam, IceBeam, PlasmaBeam]= GT
            | countOf c upgrades < countOf f upgrades = GT
            | countOf c upgrades > countOf f upgrades = LT
            | b < e = LT
            | b > e = GT
            | otherwise = compareElem c f
    
    instance Eq CandidateState where 
        p@(CandidateState a b c) == q@(CandidateState d e f) = compare p q == EQ 
                            
    upgrades :: [ItemName]
    upgrades = [MorphBall,SpaceJumpBoots,GrappleBeam,WaveBeam,IceBeam,PlasmaBeam,SpiderBall,ChargeBeam,XRayVisor,PhazonSuit,GravitySuit]

    isCompletable :: Map Id Node -> Bool
    isCompletable graph = let Just startingRoom = Map.lookup (R OLandingSite) graph
                            in isCompletableHelper graph (State [] startingRoom [])

    isCompletableHelper :: Map Id Node -> State -> Bool
    isCompletableHelper graph currState = let newState = collectFreeItems graph currState;
                                                candidates = getCandidateStates graph newState;
                                            in 
                                                isComplete graph newState || (nonEmpty candidates && isCompletableHelper graph (getBestCandidate candidates))

    isComplete :: Map Id Node -> State -> Bool
    isComplete graph (State inventory (Room roomId edges) collectedItems) = 
        let Just artifactTempleItem = Map.lookup (I ArtifactTemple) graph 
        in complete inventory && isAccessible graph roomId OArtifactTemple inventory 
        && (ArtifactTemple `elem` collectedItems || isAccessible graph (warp artifactTempleItem) OArtifactTemple inventory)

    getBestCandidate :: [CandidateState] -> State
    getBestCandidate list = let (candidate:rest) = sort list
                                CandidateState currState _ _ = candidate
                            in currState

    -- Try some warp chains and return some possible states that we could reach (that have a chance of being an improvement)
    getCandidateStates :: Map Id Node -> State -> [CandidateState]
    getCandidateStates graph state = getCandidateStatesHelper graph (getAccessibleItems graph state) state 1 []

    getCandidateStatesHelper :: Map Id Node -> [Node] -> State -> Int -> [ItemName] -> [CandidateState]
    getCandidateStatesHelper _ [] _ _ _ = []
    getCandidateStatesHelper graph (item:rest) currState depth newItems = 
        let Item itemId itemName warp = item
            State inventory (Room roomId edges) collectedItems = currState
            newRoom = getVal (Map.lookup (R warp) graph) ("Missing Room " ++ show warp)
            newState = State (itemName:inventory) newRoom (itemId:collectedItems) 
            recurseItemList = getCandidateStatesHelper graph rest currState depth newItems 
            recurseDeeper = getCandidateStatesHelper graph (getAccessibleItems graph newState) newState (depth+1) (itemName:newItems) 
            candidate = CandidateState newState depth (itemName:newItems) 
        in
            if isAccessible graph warp OLandingSite (itemName:inventory)
                then if containsUpgrade (itemName:newItems) (itemName:inventory) then candidate:recurseItemList else recurseItemList
            else
                recurseItemList ++ recurseDeeper
            
            {-- The below code is the original implementation
                It leads to massive amounts of checking when using "easy" difficulty item requirements
                This is because one-way paths to Landing Site are frequent
                In higher difficulties this might not be an issue 

                if isMutuallyAccessible graph warp OLandingSite (itemName:inventory)
                    then (if containsUpgrade (itemName:newItems) (itemName:inventory) then candidate:recurseItemList else recurseItemList)
                else if isAccessible graph warp OLandingSite (itemName:inventory) && containsUpgrade (itemName:newItems) (itemName:inventory)
                    then (if depth <= 5 then candidate : (recurseItemList ++ recurseDeeper) else candidate : recurseItemList)
                else
                    (if depth <= 5 then recurseItemList ++ recurseDeeper else recurseItemList) --}

    -- TODO may want to add Artifacts as progressing items
    containsUpgrade :: [ItemName] -> [ItemName] -> Bool
    containsUpgrade newItems inventory = let previousInventory = inventory \\ newItems 
                                        in containsAny newItems [MorphBall,SpaceJumpBoots,GrappleBeam,WaveBeam,IceBeam,PlasmaBeam
                                                                ,ChargeBeam,XRayVisor,PhazonSuit,GravitySuit] -- These are always an improvement
                                        || (contains newItems SpiderBall && contains previousInventory MorphBall)
                                        || (contains newItems MorphBallBomb && contains previousInventory MorphBall)
                                        || (contains newItems BoostBall && contains previousInventory MorphBall)
                                        || (not (supers previousInventory) && supers inventory)
                                        || (contains newItems Missile && not (contains previousInventory Missile))
                                        || (contains newItems PowerBomb && (contains previousInventory MorphBall && not (contains previousInventory PowerBomb)))
                                        || (contains newItems VariaSuit && not (containsAny previousInventory [PhazonSuit,GravitySuit]))

    isMutuallyAccessible :: Map Id Node -> RoomId -> RoomId -> [ItemName] -> Bool
    isMutuallyAccessible graph room1 room2 inventory = isAccessible graph room1 room2 inventory && isAccessible graph room2 room1 inventory
    
    collectFreeItems :: Map Id Node -> State -> State
    collectFreeItems graph state = collectFreeItemsHelper graph (getAccessibleItems graph state) state

    collectFreeItemsHelper :: Map Id Node -> [Node] -> State -> State
    collectFreeItemsHelper _ [] currState = currState
    collectFreeItemsHelper graph (item:rest) currState = 
        let (State inventory (Room roomId edges) collectedItems) = currState
            Item itemId itemName warp = item
            newRoom = getVal (Map.lookup (R warp) graph) ("Missing Room " ++ show warp)
            newState = State (itemName:inventory) (Room roomId edges) (itemId:collectedItems) 
        in 
            -- If we can reach landing site, then the warp is not useful, so collecting the item has no cost
            if isMutuallyAccessible graph warp roomId (itemName:inventory)
                then collectFreeItemsHelper graph (getAccessibleItems graph newState) newState
            else 
                collectFreeItemsHelper graph rest currState

    isAccessible :: Map Id Node -> RoomId -> RoomId -> [ItemName] -> Bool
    isAccessible graph fromRoom = isAccessibleHelper graph [fromRoom] [] 

    isAccessibleHelper :: Map Id Node -> [RoomId] -> [RoomId] -> RoomId -> [ItemName] -> Bool
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
    getAccessibleItems graph (State inventory (Room roomId edges) collectedItems) = getAccessibleItemsHelper graph [roomId] [] inventory collectedItems

    getAccessibleItemsHelper :: Map Id Node -> [RoomId] -> [RoomId] -> [ItemName] -> [ItemId] -> [Node]
    getAccessibleItemsHelper _ [] _ _ _ = []
    getAccessibleItemsHelper graph (roomId:rest) checkedRooms inventory collectedItems = 
        case Map.lookup (R roomId) graph of
            Nothing -> error ("Missing Room " ++ show roomId)
            Just (Room _ edges) -> let predicates = map canUse edges;
                                        nodeIds = map nodeId edges;
                                        bools = eval predicates inventory;
                                        reachableNodeIds = checkBools nodeIds bools;
                                        roomIds = getRoomIds reachableNodeIds;
                                        itemNodeIds = getItemNodeIds reachableNodeIds;
                                        uncheckedRoomIds = roomIds \\ checkedRooms;
                                        uncollectedItemIds = itemNodeIds \\ map I collectedItems;
                                        maybeUncollectedItems = mapM ((\x -> x graph) . Map.lookup) uncollectedItemIds -- Result is Maybe [Node] with all the item nodes
                                    in case maybeUncollectedItems of 
                                        Nothing -> error "Missing Item"
                                        Just uncollectedItems -> uncollectedItems ++ getAccessibleItemsHelper graph (uncheckedRoomIds ++ rest) (roomId:checkedRooms) inventory collectedItems

    checkBools :: [Id] -> [Bool] -> [Id]
    checkBools (id:rest1) (bool:rest2) = if bool then id : checkBools rest1 rest2  else checkBools rest1 rest2
    checkBools _ _ = []

    getRoomIds :: [Id] -> [RoomId]                   
    getRoomIds ((R roomId):rest) = roomId : getRoomIds rest
    getRoomIds (item:rest) = getRoomIds rest
    getRoomIds [] = []

    getItemNodeIds :: [Id] -> [Id]                   
    getItemNodeIds ((I itemId):rest) = I itemId : getItemNodeIds rest
    getItemNodeIds (room:rest) = getItemNodeIds rest
    getItemNodeIds [] = []
