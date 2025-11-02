
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


