
main = putStrLn "Products and Coproducts!"


{-

Cartesian product C of two sets A and B is (A,B)

standard projections from Cartesian product:
-}
fst :: (a,b) -> a
fst (x,_) = x

snd :: (a,b) -> b
snd (_,y) = y


{-Generalizing this to all Categories, we can define-}

p :: (a,b) -> a
p (x,_) = x

q :: (a,b) -> b
q (_,y) = y

{-Any C that permits this set of relationships will satisfy-}


{-Lets suppose A = Int and B = Bool...what could C be? 

Let's say C = Int
-}


pInt :: Int -> Int
-- pInt x = x

qInt :: Int -> Bool
-- qInt _ = True



{-It satisfies our definition...but it seems to be missing the point.
It seems to small (it masks info)-}


{-What about C = (Int, Int, Bool) ? -}

{-
These are commented so the bottom versions will work

pTrip :: (Int, Int, Bool) -> Int
pTrip (x, _, _) = x

qTrip :: (Int, Int, Bool) -> Bool
qTrip (_,_,b) = b

-}

{-Well this one seems too big...the middle term is ignored!-}


{- Key to the Universal Construction: Ranking 

we want to be able to sort or rank instances of our pattern
some are more universal, more reduced, more targeted and structured intrinsically


We want to compare the candidate product object C and its mappings p and q
    to another candidate C' with its p' and q'
    
    C is better than C' iff:
    - there is a morphism m from C' -> C
    - p' and q' can be reconstructed from p and q by:
        p' = p . m
        q' = q . m
        
    this means that whatever is superfluous in C', p', and q' can be FACTORIZED by m
    
    
    back to our candidates for C...Int and (Int,Int,Bool):
-}

{- m for C = Int -}
mInt :: Int -> (Int, Bool)
mInt x = (x, True)

{-then-}

pInt x = p (mInt x)
qInt x = q (mInt x) 

{- m for C = (Int,Int,Bool) -}
mTrip :: (Int,Int,Bool) -> (Int, Bool)
mTrip (x,_,y) = (x,y)

{-then-}

pTrip x = p (mTrip x)
qTrip x = q (mTrip x) 


{-
We've shown that C is better than the other C' options
    by factorizing C' -> C, p' -> p, and q' -> q
    
now we can show that the reverse is impossible too!


Can we define an m'/m'Int to reconstruct fst/p and snd/q from pInt and qInt?
    >> well qInt always returns True, so any pair with (_,False) could not be mapped
    

Can we define an m'/m'Trip to reconstruct fst/p and snd/q from pTrip and qTrip?
    >> mTrip is an infinite family, not a unique morphism that factorizes one and all
    
    
    

Formal Definition of Product:
    Product of A and B is the object C equipped with two projections
    such that for any other object C' equipped with two projections there
    is a uniqye morphism M from C' to C that factorizes those projections.

-}

factorizer :: (c -> a) -> (c -> b) -> (c -> (a, b))
factorizer p q = \x -> (p x, q x)
{-
this takes p' and q' and factorizes them into a single m
it returns a function to take C' -> C = (A, B)
-}






{-

Product relates intimately to the Singleton (terminal)

recall our ideal product C of Int and Bool sets is (Int,Bool)
and what about a poor choice, ()

-}
pSingleton :: () -> Int
pSingleton () = 1

qSingleton :: () -> Bool
qSingleton () = True

-- from universal product factorizer:
mSingleton :: () -> (Int, Bool)
mSingleton () = (1, True)

-- p becomes => p () = fst (m ())
-- q becomes => q () = snd (m ())
-- these will return 1 and True respectively 


{-
Coproduct instea flips the arrows:
    - C is coproduct of A and B, and has two *injections* i and j from a -> c and b -> c
-}

i :: a -> Either a b
i x = Left x
j :: b -> Either a b
j x = Right x


{-
the coproduct C is better than C' (who has i' and j') if there is a morphism m that goes C -> C'
-}
--     i' :: m . i
--     j' :: m . j

{-
Formal Definition of Coproduct:
    Coproduct of A and B is the object C equipped with two injections
    such that for any other object C' equipped with two injections there
    is a uniqye morphism M from C to C' that factorizes those injections.


can be understood in terms of the DISJOINT union of two sets A and B
    where elements of C are either in A or B (or both)
    elements of C are tagged with their source

can also be understood in terms of Union of Types:

struct Contact{
    enum {isPhone, isEmail} tag;
    union {int phoneNum; const char* emailAddr;};
};

-}



{-Contact coproduct example-}

data Contact = PhoneNum Int | EmailAddr String

{-here these serve both as constructors (injections) and tags for pattern matching...-Bartosz

for instance-}

helpdesk :: Contact
helpdesk = PhoneNum 9273

{-
in Haskell, whereas the canonical product is the pair (,), Coproduct is Either

data Either a b = Left a | Right b-}

{-or this-}
data MyEither a b = Int a | String b
myFactorizer :: (Int -> c) -> (String -> c) -> (MyEither Int String -> c)
myFactorizer i j (Int a) = i a
myFactorizer i j (String b) = j b
{-this looks like it is pattern matching both the functions and the input for the last one (currying?)-}

{-
the general factorizer from C -> C' or i -> i' and j -> j' is below
-}
factorizerCo :: (a -> c) -> (b -> c) -> (Either a b -> c)
factorizerCo i j (Left a) = i a
-- these are curry pattern matches
factorizerCo i j (Right b) = j b


