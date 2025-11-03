import Data.Void
main = putStrLn "Algebraic Types"


data BenList a = Niz | Yee a (BenList a)
{-
x(a) = 1 + a(x(a))
x(a) = 1 + a(1 + a(x(a)))
x(a) = 1 + a + aa(x(a))
x(a) = 1 + a + aa + aaa(x(a))
x(a) = 1 + a + aa + aaa + aaa(x(a))
...  = ...
x(a) = 1 + a + ... + na(x(a))
-}


data Shape = Circle Float | Rect Float Float | Square Float

area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rect d h) = d * h
area (Square f) = f * f

circ :: Shape -> Float 
circ (Circle r) = 2 * pi * r
circ (Rect d h) = 2 * (d + h)
circ (Square f) = 4 * f






{-
An arbitrary number of types can be stored in nested tuples. 
But it is the same thing as a single tuple/record with all of the
types at the same level. This is shown first by deriving an isomorphism
between any two packing schemes of the same data. 

For three types, a, b, c, in this order, there are two ways to pack them:

(a, (b, c)) and ((a, b), c)

-}
alpha :: (a, (b, c)) -> ((a, b), c)
alpha (x, (y, z)) = ((x, y), z)

alphaInv :: ((a, b), c) -> (a, (b, c))
alphaInv ((x, y), z) = (x, (y, z))

-- this compiles, hence we have an isomorphism

{-
We can see the creation of product types as a binary operation on two types
The above then resembles the associativity seen in Monoids

(a * b) * c = a * (b * c)

However, whereas in Monoids, these two are truly equivalent, with product
types, they are equal up to isomorphism. But to me, that is no deterrent. 
Isomorphism is *effective* equality because of its very definition: you can
always go from one to another with perfect fidelity. I don't need the fidelity
handed on a silver platter of absolute equivalence. 


If we are okay with the *isomorphic* nature of our Monoid-Product analogy, we
can push further. In Monoids, we have a unit type that acts as identity when 
operated with another object. In Product types, that is the Unit, the singleton
set: ()...
-}

rho :: ((), a) -> a
rho (_, x) = x

rhoInv :: a -> ((), a)
rhoInv x = ((), x)

{-
This also compiles, hence the isomorphism. 

In Haskell, there is a much more general way to make product types:

data Pair a b = P a b

'Pair' is the type parameterized by two types. 'P' is the named value
constructor. 
-}
data Pair a b = P a b

stmt :: Pair String Bool            -- this constructs the type
stmt = P "This sentence is" False   -- this creates a value

{-
RECORDS:
A very useful way to make product types. 

For example, if we wanted to make types representing Elements with their
symbol, name, and atomic number, we could use a (,,) and just remember the order
and such. But the following has the same functionality with built in bookkeeping.
-}

data Element = Element {
    name :: String,
    symbol :: String,
    number :: Int
}

-- this is isomorphic to (String, String, Int):

elemToTuple :: Element -> (String, String, Int)
elemToTuple e = (name e, symbol e, number e)
-- notice that these fields act as getters! projections!
-- name :: Element -> String, etc.

tupleToElem :: (String, String, Int) -> Element
tupleToElem (n, s, i) = Element {name = n, symbol = s, number = i}

-- this compiles!

{-
Now let's map out the equivalence of the Set of types and a monoid with
'+'as the operator. Here, the binary operator is 'Either' and unit is Void:
-}

psi :: Either a Void -> a
psi (Left x) = x
psi (Right x) = absurd x

psiInv :: a -> Either a Void
psiInv x = Left x

-- there we are!



{-Finally, we have:
    a x (b + c) = a x b + a x c
-}

prodToSum :: (a, Either b c) -> Either (a, b) (a, c)
prodToSum (x, e) = case e of
    Left y -> Left (x, y)
    Right z -> Right (x, z)
    
sumToProd :: Either (a, b) (a, c) -> (a, Either b c)
sumToProd e = case e of
    Left (x, y) -> (x, Left y)
    Right (x, z) -> (x, Right z)
    
-- <3



















-- CHALLENGES
{-
 Show the isomorphism between Maybe a <-> Either () a
-}

maybeToEither :: Maybe a -> Either () a
maybeToEither Nothing = Left () -- oh now i get what Pattern Match means
maybeToEither (Just x) = Right x-- we literally tell compiler what to do
                                -- when we see f (x)    !   !   !

eitherToMaybe :: Either () a -> Maybe a
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right x) = Just x

-- This is our proof (it compiles)!


{-
Show that a + a = 2 x a holds for types (up to isomorphism)

which means Either a a = (Bool, a)
-}

aPlusA :: Either a a -> (Bool, a)
aPlusA (Left x) = (True, x)
aPlusA (Right x) = (False, x)

twoTimesA :: (Bool, a) -> Either a a
twoTimesA (True, x) = Left x
twoTimesA (False, x) = Right x

-- This, again, is our proof (it compiles). 
-- We could, of course, do this 1 other way. 
-- a + (a + a) = 3 x would have 3 x 2 = 6
--             = 4 x            4 x 3 = 12, 20, 30, 42, 56, 72, 90, 110, 132...    

data Tri = A | B | C

aPlusAPlusA :: Either a (Either a a) -> (Tri, a)
aPlusAPlusA (Left x) = (A, x)
aPlusAPlusA (Right (Left x)) = (B, x)
aPlusAPlusA (Right (Right x)) = (C, x)

threeTimesA :: (Tri, a) -> Either a (Either a a)
threeTimesA (A, x) = Left x
threeTimesA (B, x) = Right (Left x)
threeTimesA (C, x) = Right (Right x)

-- ABC (3), BAC (3), 

-- ABCD (4), BACD (4), BCAD (4)

-- ABCDE (5), BACDE (5), BCADE (5), BCDAE (5)

{-
Yeah, so we have found the formula here. Our problem is just one of ordering. 
How many ways can you order N elements of type a? How many isomorphisms can you
define between these two types? You need uniform alignment between the two 
complements in the isomorphism for it to work. That is, the degree of freedom
in our unique (and equivalent) isomorphisms is just determined by the order in which
you make these mappings. Why do we have N-1 distinct orders and N distinct isomorphisms
in each? Well just look at our examples. A can be in place 0, 1, ..., N. But
when it is in position N, it is really just in positiion -1 (again, if we're
talking about distinct orders). So that is the amount of distinct orders we have. 
In each distinct order, there are N possible ways to order. For a given ordering,
there are N distinct lists given by the N different ways you could 'start' that order. 
We avoid loops because we're now just talking about a finite N-ple. The constraint 
in these N of an order-family is as follows: A -> B or A -> _. B -> C or B -> _...
AND it cannot be the case that more than one letter points to _ in a given list. 
The list corresponds to the finite set of patterns you match for one of these functions
in one of these isoomorphic pairs, and the order in which you match them. 

    Orders:
        A -> B -> C     or      B -> A -> C     (what about B -> C -> A? that is == A -> B -> C)

    within an Order, we have the distinct lists (schemes for invertible maps):
        ABC
        BCA
        CAB
-}


