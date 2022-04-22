module Graph where

import Data.Map (Map)
import qualified Data.Map as Map

import Node

{-- 
A graph is represented as a collection of nodes. Specifically, it's a Map of the Node ID to the Node itself.

Note that Rooms and Items are both nodes in this graph, with edges representing the requirements (specified as a function) to move from node to node.
As such, Items are nodes that connect only to the room in which they reside.

We calculate if a seed is completable by constructing a graph representing it and calling isCompletable(Graph, CollectedItems, StartingNode).
This function will be called recursively with progressively more items in the inventory until one of two things happens:
    1. We call the function with all 12 artifacts in inventory and it is able to reach Artifact Temple (result is True)
    2. We traverse the entire graph (using all available warps: see below), but cannot collect all 12 artifacts and reach Artifact Temple (result is False)

Traversal:
The graph is traversed by attempting to follow all edges available to the given Node in order (if they go to an unvisited node). 
When we arrive at the new node, we add it to the collection of visited nodes, and continue attempting to follow all edges, 
until we find that all edges are either unavailable or lead to an already-visited node.

Predicates:
The trickiest part of constructing this graph is determining the requirements to move between rooms. If you wish to modify the item/room requirements, see the Predicates.hs file

Regarding Warps:
Upon traversing to an Item, we need to restart the graph traversal from the room that the item warps to (if it warps), 
but this time we have the new item in inventory and the item node can no longer be used. 

Note that in the case of extra missile expansions or power bomb expantions, we don't gain access to more nodes, 
so in this case we don't start the traversal fresh, but instead continue the current traversal from the new room.

If the item does not warp, it is treated as if it warps to the room containing the item.

    --}

buildMap :: [Node] -> Map Id Node
buildMap nodes = Map.fromList (map createMapKeys nodes)

createMapKeys :: Node -> (Id, Node)
createMapKeys node =
    case node of
        Item item _ _ -> (I item, node)
        Room room _ -> (R room, node)
