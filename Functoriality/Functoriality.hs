
main = putStrLn "Functoriality"

-- A->C in Category X
----could be Int->Float
-- B->D in Category Y
----could be Float->Int

------we could have (Int,Float) -> (Float,Int)...

-- -> now: List (Int,Float) -> List (Float,Int)
class Bifunctor f where
    -- takes (Int->Float), (Float->Int) -> List (Int,Float) -> List (Float,Int)
    bimap :: (a -> c) -> (b -> d) -> f a b -> f c d
    bimap g h = first g . second h
    -- first g (second h)
    -- first g ((B->D) -> F A B -> F A D)
    -- ((A->C) -> F A B -> F C B)(((B->D) -> F A B -> F A D))
    
    -- ((B->D) -> F A B -> F A D) -> F [(B->D)] B -> F [F A B -> F A D] B
    --  [ A  ] -> [     C      ]  -> F [  A   ] B -> F [      C       ] B  

    first :: (a -> c) -> f a b -> f c b
    first g = bimap g id
    second :: (b -> d) -> f a b -> f a d
    second = bimap id
    
    
    -- this is very confusing if we start in the general case!
    
{-
    What we really have is a mapping from a product of objects from two 
    categories into a single object in third category. Suppose we have two
    functions: F :: A->C and G :: B->D...these are just mappings from object to 
    object. We can produce something then that takes an A AND a B, (A,B), and 
    maps it to the dually transformed: (F A, G B) = (C, D)! 
    
    When shown as the product, it makes a lot more sense. Let's implement it for
    products:
-}

instance Bifunctor (,) where
    bimap f g (x,y) = (f x, g y)

-- this compiles, of course, so there we go. 

floatToInt :: Float -> Int
floatToInt _ = 1

boolToString :: Bool -> String
boolToString _ = "Hello!"

fb_to_is :: (Float, Bool) -> (Int, String)
fb_to_is (x, y) = (floatToInt x, boolToString y)

-- boom!
    
    