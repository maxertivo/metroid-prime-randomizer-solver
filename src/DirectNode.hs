module DirectNode where

import Node
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import Util
import Data.Vector
import qualified Data.Vector as Vector
import Data.List (sortBy)

data DirectRoom = DirectRoom {roomId :: RoomId, roomEdges :: [DirectEdge], itemEdges :: [DirectIEdge]} 

data DirectItem =  DirectItem {itemId :: ItemId, itemName :: ItemName, warp :: DirectRoom}

instance Eq DirectRoom where
    DirectRoom id1 _ _ == DirectRoom id2 _ _ = id1 == id2

instance Eq DirectItem where
    DirectItem id1 _ _ == DirectItem id2 _ _ = id1 == id2

data DirectEdge = DirectEdge {predicate :: Map ItemName Int -> Set ItemId -> Bool, room :: DirectRoom}

data DirectIEdge = DirectIEdge {itemPredicate :: Map ItemName Int -> Set ItemId -> Bool, item :: DirectItem}

instance Show DirectRoom where
    show (DirectRoom roomId _ _ ) = show roomId

instance Show DirectItem where
    show (DirectItem itemId _ _ ) = show itemId

instance Show DirectEdge where 
    show (DirectEdge _ (DirectRoom roomId _ _)) = show roomId

instance Show DirectIEdge where 
    show (DirectIEdge _ (DirectItem itemId _ _)) = show itemId

sortByEnum :: (Int,a) -> (Int,a) -> Ordering
sortByEnum (a,b) (c,d) = compare (fromEnum a) (fromEnum c)

convertToDirectArrays :: [(Int, Room)] -> [(Int, Item)] -> (Vector DirectRoom, Vector DirectItem)
convertToDirectArrays roomList itemList = 
    (directRooms, directItems)
    where 
        sortedRooms = sortBy sortByEnum roomList

        sortedItems = sortBy sortByEnum itemList

        (directRooms, directItems) = (Vector.fromList
                        [ convertDirectRoom room | (roomId,room) <- sortedRooms ]
                    , Vector.fromList
                        [ convertDirectItem item | (itemId,item) <- sortedItems ])
     
        convertDirectRoom (Room roomId roomEdges itemEdges) = DirectRoom roomId (convertDirectEdges roomEdges) (convertDirectIEdges itemEdges)

        convertDirectItem (Item itemId itemName warp) = DirectItem itemId itemName (directRooms Vector.! fromEnum warp)

        convertDirectEdges [] = []
        convertDirectEdges (Edge predicate roomId : rest) = DirectEdge predicate (directRooms Vector.! fromEnum roomId) : convertDirectEdges rest

        convertDirectIEdges [] = []
        convertDirectIEdges (IEdge itemPredicate itemId : rest) = DirectIEdge itemPredicate (directItems Vector.! fromEnum itemId) : convertDirectIEdges rest

-- convertDirectRoom :: Room -> Map RoomId Room -> Map ItemId Item -> DirectRoom
-- convertDirectRoom (Room roomId roomEdges itemEdges) roomMap itemMap = DirectRoom roomId (convertDirectEdges roomEdges roomMap itemMap) (convertDirectIEdges itemEdges roomMap itemMap)

-- convertDirectItem :: Item -> Map RoomId Room -> Map ItemId Item -> DirectItem
-- convertDirectItem (Item itemId itemName warp) roomMap itemMap = DirectItem itemId itemName (convertDirectRoom (mapLookup warp roomMap) roomMap itemMap)

-- convertDirectEdges :: [Edge] -> Map RoomId Room -> Map ItemId Item -> [DirectEdge]
-- convertDirectEdges [] _ _ = []
-- convertDirectEdges (Edge predicate roomId : rest) roomMap itemMap = DirectEdge predicate (convertDirectRoom (mapLookup roomId roomMap) roomMap itemMap) : convertDirectEdges rest roomMap itemMap

-- convertDirectIEdges :: [IEdge] -> Map RoomId Room -> Map ItemId Item -> [DirectIEdge]
-- convertDirectIEdges [] _ _ = []
-- convertDirectIEdges (IEdge itemPredicate itemId : rest) roomMap itemMap = DirectIEdge itemPredicate (convertDirectItem (mapLookup itemId itemMap) roomMap itemMap) : convertDirectIEdges rest roomMap itemMap