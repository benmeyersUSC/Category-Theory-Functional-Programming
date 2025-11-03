
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

addNode :: Node -> Node -> Node
addNode x n = addEdge x Edge{to = n, len =0}


type Writer a = (a, String)

{-
Morphisms are such: 
    a -> Writer a 
-}

{-Composition defined by >==> -}
(>=>) :: (a -> Writer b) -> (b -> Writer c) -> (a -> Writer c)
m1 >=> m2 = \x ->
    let (y, s1) = m1 x
        (z, s2) = m2 y
    in (z, s1 ++ s2)


childTrav :: Node -> Writer (List Edge)
childTrav n = (children n, "getting kids: ")

-- this is where we go through the list:
edgeList :: List Edge -> Writer ()
edgeList Nil = ((), "done")
edgeList (Cons edge rst) = 
    let (_, stringFromRest) = edgeList rst
    in ((), name (to edge) ++ ", " ++ stringFromRest)
    



nodeA :: Node
nodeA = Node {name = "A", children = Nil}
nodeB :: Node
nodeB = Node {name = "B", children = Nil}
nodeC :: Node
nodeC = Node {name = "C", children = Nil}
nodeD :: Node
nodeD = Node {name = "D", children = Nil}

-- node :: Node
-- node = Node {name = "Z", children = Cons nodeA (Cons nodeB (Cons nodeC (Cons nodeD Nil)))}

nodeF :: Node
nodeF = addNode nodeA nodeB

nodeG :: Node 
nodeG = addNode nodeF nodeC

nodeH :: Node 
nodeH = addNode nodeG nodeD

writerF :: Writer ()
writerF = edgeList (children nodeH)

main = putStrLn (snd writerF)


{-

I need node interface nicer

i need to be able to call addNode and it returns itself with a new edge node
in in there.

it should really just be = addEdge . Edge{

-}



