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
-----------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------
-- THE BRAINS
-----------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------
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
-----------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------
    
nodeList :: List Node -> Writer ()
nodeList Nil = ((), "done")
nodeList (Cons node rst) = 
    let (_, stringFromRest) = nodeList rst
    in ((), name node ++ "->" ++ stringFromRest)
    
strList :: List String -> Writer ()
strList Nil = ((), "done")
strList (Cons str rst) = 
    let (_, stringFromRest) = strList rst
    in ((), str ++ "->" ++ stringFromRest)


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


extractPath :: MemoMap -> Node -> Node -> List String
extractPath mmap start end = case (name start == name end) of
    True -> Cons (name end) Nil 
    -- if we're at end, we just give the name of the end node...
    -- this will make it into our list cuz recursive call calls Cons on this
    
    False -> -- otherwise, we need to chosoe the best child from here
        let bestEdge = minChildDist (children start) mmap
            -- best edge gets us the best edge
            bestChild = to bestEdge -- and we extract the node
        in Cons (name start) (extractPath mmap bestChild end)
        -- and we recursively call extractPath on the rest of the list, 
        --  now with start as the head of the list!


-- we take in a list of children and map and return best edge
minChildDist :: List Edge -> MemoMap -> Edge
minChildDist (Cons ed Nil) mmap = ed
-- assume lists isnt empty (cuz Final doesnt call this)
-- ... if we have a one edge list, then that's the best

minChildDist (Cons ed rst) mmap =  -- otherwise we gotta recurse
    let bestOfRest = minChildDist rst mmap -- get the best of others
        
        thisCost = case (Map.lookup (name (to ed)) mmap) of
        -- look up cost from currEdge. 
            Nothing -> maxBound
            Just d -> d + len ed
            -- if exists (it does), then add ITS PATH TO END to path from prev
            
        restCost = case (Map.lookup (name (to bestOfRest)) mmap) of
            Nothing -> maxBound
            Just d -> d + len bestOfRest
            -- same logic for here...recursively, we assume bestOfRest is found
    in if thisCost < restCost then ed else bestOfRest
    -- then if this edge is better than rest, choose it!


main :: IO ()
main = do
  let myEmptyMap = Map.empty :: Map String Int
      (dist, mmap) = minDist nodeA'' nodeF Map.empty
      pathList = extractPath mmap nodeA'' nodeF
      pathString = snd (strList pathList)
  print dist
  putStrLn pathString


{-
Should make the graph interface nicer for sure.
But overall very very happy with where this is. If you look at the DP_Graph.cpp file, you'll
see something that is much easier to comprehend. We have a very nice generalized Memoize() function
to turn any function into one which keeps a memo of its work. In this problem, that allows an efficient
recursive + memo solution. Translating this stateful solution to Haskell is non-trivial. Instead of
mutable state, we have to use pure functions which pass everything around. Other stateful aspects of the
C++ are the min of a list, the looping of a list itself! These things all had to be done in Haskell. We
did it with more and more functions! We cannot do a for-loop and call minDist on all of the nodes, but we
can make a recursive function (minDists) that goes through each element (Edge) and calls minDist on it!
Then for min, we similarly recursively process the list. It is just a totally different way to see the problem
of looping and traversal. 
-}