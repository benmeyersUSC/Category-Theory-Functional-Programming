
main = putStrLn "Hello World"
class Functor_ f where
    fmap_ :: (a -> b) -> f a -> f b
    
newtype Fix f = Fix (f (Fix f))
-- Fix f unwraps one layer of this recursive fixed point structure. 
-- If f is Prelist a, then this data constructor pulls one layer of 
-- wrapping off of our Prelist. If it is 'Cons x rst', then we get 
-- x, and then rst is a Fix-tagged Prelist a! It lazily recurses. 

-- Fix is actually the Y combinator. It performs recursion on types, 
-- compared to the Y combinator's functions. They are both recursion
-- by indirect self-reference. And indirect self-reference brings us
-- to Gödel. His formua G = 'g is not provable', where g = GödelNum(G). 
-- De-Gödelizing g gives you G. Gödel-Numbering and De-Gödelizing are
-- functors on each other's domains. With G/g, you have a fixed point. 
-- When you De-Gödelize g, you get G---the structure from which g was
-- extracted. It is self-similar, recursive. It can be represented with
-- a fixed point on the function that parameterizes g to be some number. 
data Formula = Str String | Composed Formula Int Formula
extract :: Formula -> Int
extract (Str _) = -1
extract (Composed _ n _) = n

g_num :: Int
g_num = 927
g_formula :: Formula
g_formula = Composed (Str "") g_num (Str " is not provable")

deGodel :: Int -> Formula
deGodel x = Str ""
-- this is not implemented...I wish!

-- deGodel(extract(G)) = G
--                     = deGodel(g)
--                     = ("", g, " is not provable")
--                     = G




data List a = Nil | Cons a (List a)
data Prelist a b = Niz | Conz a b
-- prelist is defined to be List with recursion factored out
-- such that [Prelist a]'s fixed point is [List a]

sumAlg :: Prelist Int Int -> Int
sumAlg Niz = 0
sumAlg (Conz x acc) = x + acc
-- prelist is for accumulation

foldr_ :: (a -> b -> b) -> b -> List a -> b
foldr_ f z Nil = z
foldr_ f z (Cons x xs) = f x (foldr_ f z xs)
-- this function uses the property that Fix [Prelist a] = Cons a (Fix [Prelist a]) = List a 
-- we lazily unpack this fixed point point by point (as the recursive List does anyway!)
-- and we call the algebra on these
--
-- You see how the base case uses the algebra: When we get to the base case of the recursive
-- fixed point, we finally return (to be called) the ALGEBRA's base case. Then this result (0)
-- becomes (already was, but now has an explicit value) the acc term in all of the non-base calls
-- which made their way down. 


-- Recursive tree can be decomposed too
data Tree a = Leaf a | Node (Tree a) (Tree a)
data Pretree a r = Leaf a | Node r r
type Tree a = Fix (Pretree a)


-- Natural numbers as well
data Nat = Zero | Succ Nat -- recursive def
data Prenat r = Zer | Suc r
-- type Nat = Fix Prenat

natSumAlg :: Prenat Int -> Int
natSumAlg Zer = 0
natSumAlg (Suc x) = 1 + x

collapseNat :: (Prenat r -> r) -> Nat -> r
collapseNat alg Zero = alg Zer  -- actually extract the base
collapseNat alg (Succ x) = alg (Suc (collapseNat alg x)) -- recursively process 'rest'
-- we're popping off the Suc layer (+1). When the bottom becomes 0, the next to 
-- bottom becomes 1 + 0, and then 1 + (1 + 0) and so on. Prenat is entirely non-recursive. 
-- But is has a develish fixed point!

-- Let's see this fixed point one more time:
-- Fix (Prenat r) = Prenat (Fix (Prenat r))
--                = Suc (Fix (Prenat r))
--                = Suc (Suc (Fix (Prenat r)))
--                = Suc (Suc (Suc (Fix (Prenat r))))
--                = Suc (Suc (Suc (Suc...(Suc (Fix (Zer)))...)))
--                = Suc (Suc (Suc (Suc...(Suc (Zer (Fix (Zer))))...)))
--                = Suc (Suc (Suc (Suc...(Suc (Zer))...)))
-- 
-- These are church numerals, these are nested sets, this is natural numbers!






-- We can then define, empowered, the general catamorphism that collapses a recursive fixed point!:
cata :: Functor_ f => (f a -> a) -> (Fix f) -> a
cata alg (Fix fx) = alg (fmap_ (cata alg) fx)
-- it uses the Fix f definition (unpacks it with pattern matching!) to lazily unpack and process 
-- the recursive structure (which is the fixed point). This function works until the base case
-- at which point the algebra will be able to collapse its first value; then it runs up the ladder. 
-- You are eliminating the recursive self reference by unfurling across time, setting up a domino
-- array of computations. 