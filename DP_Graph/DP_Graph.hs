import qualified Data.Map as Map
import Data.Map (Map)

data List a = Nil | Cons a (List a)
-- Nil is a singleton. 
-- Cons :: a -> List a -> List a
-- Cons 1 Nil = [1]
-- Const 2 (Cons 1 Nil) = [2, 1]

append :: List a -> a -> List a
append Nil y = Cons y Nil
-- if we are given an empty list, it becomes Cons y Nil = [y]
append (Cons x xs) y = Cons x (append xs y)
-- otherwise we are given [x, ...] y, it becomes Cons x (append [...] y) = [x, ..., y]

data Node = Node {
    name :: String,
    children :: List Edge
}

data Edge = Edge {
    to :: Node,
    len :: Int
}


addEdge :: Node -> Edge -> Node 
addEdge nd ed = Node {
    name = (name nd), 
    children = (append (children nd) ed)
}

addNode :: Node -> Node -> Int -> Node
addNode x n l = addEdge x Edge{to = n, len = l}


type Writer a = (a, String)
    
type MemoMap = Map String Int
{-
newMap = Map.insert (name someNode) 42 oldMap

case Map.lookup (name someNode) distMap of
    Just dist -> -- already have it, use dist
    Nothing   -> -- need to compute it
-}

-- minDist will handle all of the orchestration
-- takes in start, end, empty map...returns the minDist and the memo
minDist :: Node -> Node -> MemoMap -> (Int, MemoMap)
minDist from toNode mmap = case (name from == name toNode) of
    True -> (0, mmap)  -- base case, we're at the end
    False ->           -- call on all children
        let (dists, newMap) = minDists (children from) toNode mmap
        in (minInt dists, newMap)   -- return minimum from kids!
    
-- recursively works through children, actually computing dists
-- takes in children (List Edge), end, memo...returns dists (List Int) and memo
minDists :: List Edge -> Node -> MemoMap -> (List Int, MemoMap)
minDists Nil toNode mmap = (Nil, mmap)  -- done, just pass map
minDists (Cons e rst) toNode mmap =  -- NOW WE PROCESS!
        
        let (dist, newMap) = getOrCompute (to e) toNode mmap
        -- add the node to the map, this is where we compute distance!

            (restDists, newnewMap) = minDists rst toNode newMap
            -- now recursively call minDists on rest of children
        
        in (Cons (dist + len e) restDists, newnewMap)
        -- append this node's minDist + its edge weight to the results of rec call
        


-- gets or adds to memo and returns it
-- takes in start, end, memo...returns distance it gets from calling minDist!
getOrCompute :: Node -> Node -> MemoMap -> (Int, MemoMap)
getOrCompute fromNode toNode mmap = case (Map.lookup (name fromNode) mmap) of 
    Just dist -> (dist, mmap) -- already have it, just pass through
    Nothing -> 
        let (dist, newMap) = minDist fromNode toNode mmap
        -- call minDist !
        
        in (dist, Map.insert (name fromNode) dist newMap)
        -- insert computed distance at node's name
    
-- simple rec min int
minInt :: List Int -> Int
minInt Nil = maxBound
minInt (Cons n rst) = min n (minInt rst)
    
nodeList :: List Node -> Writer ()
nodeList Nil = ((), "done")
nodeList (Cons node rst) = 
    let (_, stringFromRest) = nodeList rst
    in ((), name node ++ "->" ++ stringFromRest)


nodeA :: Node
nodeA = Node {name = "A", children = Nil}
nodeB :: Node
nodeB = Node {name = "B", children = Nil}
nodeC :: Node
nodeC = Node {name = "C", children = Nil}
nodeD :: Node
nodeD = Node {name = "D", children = Nil}
nodeE :: Node
nodeE = Node {name = "E", children = Nil}
nodeF :: Node
nodeF = Node {name = "F", children = Nil}



nodeD' :: Node
nodeD' = addNode nodeD nodeF 5

nodeE' :: Node
nodeE' = addNode nodeE nodeD' 2
nodeE'' :: Node
nodeE'' = addNode nodeE' nodeF 4

nodeC' :: Node
nodeC' = addNode nodeC nodeE'' 3
nodeC'' :: Node
nodeC'' = addNode nodeC' nodeD' 6

nodeB' :: Node
nodeB' = addNode nodeB nodeC'' 1
nodeB'' :: Node
nodeB'' = addNode nodeB' nodeE'' 5
_nodeB'' :: Node
_nodeB'' = addNode nodeB'' nodeD' 4

nodeA' :: Node
nodeA' = addNode nodeA nodeC'' 2
nodeA'' :: Node
nodeA'' = addNode nodeA' _nodeB'' 3


main :: IO ()
main = do
  let myEmptyMap = Map.empty :: Map String Int
      myInt = fst (minDist nodeA'' nodeF Map.empty)
  print myInt

