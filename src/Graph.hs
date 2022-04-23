module Graph where

import Data.Map (Map)
import qualified Data.Map as Map

import Node

{-- 
A graph is represented as a collection of nodes. Specifically, it's a Map of the Node ID to the Node itself.

Note that Rooms and Items are both nodes in this graph, with edges representing the requirements (specified as a function) to move from node to node.
As such, Items are nodes that force you to traverse to their designated "warp" room.

We calculate if a seed is completable by constructing a graph representing it and calling isCompletable(Graph).
This function does the following:
    1. Collects all "free" items. That is, items where we can handle the warp and the warp is not useful for later
    2. Checks if you can complete the game with current items. If so, we are done and can complete the game.
    3. Tries to find a sequence of at most 5 warps that collects at least one progression item
    4. If no such sequences can be found, we are done and cannot complete the game. Otherwise, repeat steps 1-3.

Predicates:
The trickiest part of constructing this graph is determining the requirements to move between rooms. If you wish to modify the item/room requirements, see the Predicates.hs file

Graph Construction:
Much of the graph construction is hard coded. During construction, the difficulty is passed in to any edge predicate the requires it, 
thereby eliminating the need to consider difficulty when solving the graph.

Regarding Warps:
If the item does not warp, it is treated as if it warps to the room containing the item (Or in some cases, an adjacent room that is more appropriate).

    --}

buildMap :: [Node] -> Map Id Node
buildMap nodes = Map.fromList (map createMapKeys nodes)

createMapKeys :: Node -> (Id, Node)
createMapKeys node =
    case node of
        Item item _ _ -> (I item, node)
        Room room _ -> (R room, node)
