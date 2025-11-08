{-# LANGUAGE DeriveFunctor #-}
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
    


-- and what about Coproduct?
instance Bifunctor Either where
    bimap f _ (Left x) = Left (f x)
    bimap _ g (Right y) = Right (g y)
    
fORb_to_iORs :: Either Float Bool -> Either Int String  
-- we could do:
-- fORb_to_iORs (Left x) = Left (floatToInt x)
-- fORb_to_iORs (Right y) = Right (boolToString y)
-- or:
fORb_to_iORs x = bimap floatToInt boolToString x





-- recall the Kleisli Writer category...

type Writer a = (a, String)

-- this is a Functor because it is a simple product type (and we can derive fmap)

-- recall the bind operator that allows for composition:
(>=>) :: (a -> Writer b) -> (b -> Writer c) -> (a -> Writer c)
m1 >=> m2 = \x ->
    let (y, s1) = m1 x
        (z, s2) = m2 y
    in (z, s1 ++ s2)
    
-- and the identity morphism is called return:
returnW :: a -> Writer a
returnW x = (x, "")

{-
and now let's recall what fmap is..
it lifts a function from a -> b to Fa -> Fb:

instance Functor Writer where
    fmap :: (a -> b) -> (Writer a -> Writer b)
    fmap f = id >=> (\x -> returnW (f x))

first id maps Writer a -> Writer a, then >=> grabs the a and passes to lambda
then the lambda uses f to turn a -> b, then wraps it in Writer with returnW!

This schema works for every Kleisli Category that implements embellished
composition and identity: >=> and return!

These are all Functors and their FMAPs are identical
-}


{-
now let's recall the Reader Functor, beased on the partially applied:
    (->) r
-}

type Reader r a = r -> a
{-
instance Functor (Reader r) where
    fmap :: (a -> b) -> (Reader r a -> Reader r b) :: (a -> b) -> (r -> a) -> (r -> b)
    fmap f g = f . g
    
again, we just lift f :: a -> b into Reader r's world...in this case, it makes
much more sense to see fmap as a function on two inputs, but again not necessary
In that veiw, though, all we do is use the provided g :: r -> a, then apply f!

But we have a bigger question: is this a bifunctor on r and a? Other functors
with two arguments like Either or (,) were indeed bifunctors. 

Let's first see if we are functorial in the first argument...start with a variation
on the Reader:
-}

type Op r a = a -> r
{- reader but with args flipped...and the fmap?
   fmap :: (a -> b) -> (a -> r) -> (a -> b)
   ...there is no way to compose these functions to get the third! If the first 
   two args (a and b) were flipped, sure, but not as it is now. 
   We would like to invert the function but we cannot. What we can do is go to 
   the Opposite category!
-}

{- 
    Every category C has a C_op with all arrows flipped. 
    
    Consider the functor: F :: C_op -> D

    this maps morphisms f_op :: a -> b to Ff_op Fa -> Fb in D...
    but f_op secretly corresponds to f :: b -> a in C...
    
    F is just a normal functor from C_op to D. There must be another functor
    from C -> D, which we can call G. It will map objects the same way that
    F does, because C and C_op have all of the same objects. However it will
    flip the morphisms.
    So G :: b -> a in f first maps to f_op :: b -> a in C_op, then uses F to get
    to D, where we will have Fa -> Fb. The entire mapping done by G is:
        (b -> a) -> (Ga -> Gb)
    A functor with a Twist! This is known as a Contravariant Functor!
-}

-- class Contravariant f where
--     contramap :: (b -> a) -> (f a -> f b)
--     contramap f g = g . f
    
-- f (which takes b -> a) is first applied to a, giving b. Then we apply
-- g(b) which gives us fb! (This gets confusing, but just trace through it!)

-- and then we can say:
-- instance Contravariant (Op r) where
--   contramap :: (b -> a) -> ((a -> r) -> (b -> r))
--   contramap f g = g . f
--OR contramap = flip (.)

-- because there is a flip function
-- flip :: (a -> b -> c) -> (b -> a -> c)
-- flip f y x = f x y


-- So, the Reader r or (->) r operator is contravariant in its first argument,
-- because the flipped version is covariant. The second argument, a, is treated
-- covariantly by Reader....what is this?

-- This is a Profunctor: contrafunctorial in its first argument and functorial in
-- its second argument. We can define it:


-- class Profunctor p where
-- -- notice that a -> b gets flipped to be b -> a, but c -> d stays:
--     dimap :: (a -> b) -> (c -> d) -> p b c -> p a d
--     dimap f g = lmap f . lmap g
--     lmap :: (a -> b) -> p b c -> p a c
--     lmap f = dimap f id
--     rmap :: (b -> c) -> p a b -> p a c
--     rmap = dimap id


-- and then we can show that (->) is a profunctor:
-- instance Profunctor (->) where
--     dimap ab cd bc = cd . bc . ab
--     lmap = flip (.)     -- contramap of (->)!
--     rmap = (.)          -- fmap of (->)!
    
    
    
    
{-CHALLENGES-}

{-1-}
-- Show that data Pair a b = Pair a b is a bifunctor.
--- Implement bimap, first, second
-- instance Bifunctor Pair where
    -- bimap :: (a -> c) -> (b -> d) -> (Pair a b -> Pair c d)
    -- bimap f g = first f . second g
    -- first :: (a -> c) -> Pair a b -> Pair c b
    -- first ac pab = (ac (fst pab), snd pab)
    -- second :: (b -> d) -> Pair a b -> Pair a d
    -- second bd pab = (fst pab, bd (snd pab))
    
bimap :: (a -> c) -> (b -> d) -> ((a, b) -> (c, d))
first :: (a -> c) -> (a, b) -> (c, b)
second :: (b -> d) -> (a, b) -> (a, d)
-- bimap ac bd = first ac . second bd
bimap ac bd = (\x -> (ac (fst x), snd x)) . (\x -> (fst x, bd (snd x)))
-- first ac pab = (ac (fst pab), snd pab)
first ac = \x -> (ac (fst x), snd x)
-- second bd pab = (fst pab, bd (snd pab))
second bd = \x -> (fst x, bd (snd x))
-- compiles!
-- bimap first uses second to take b->d & (a,b) -> (a,d)
--  then uses first to take a->c & (a,d) -> (c,d) (the b in first is d!)

-- said another way, second takes b->d and gives a function ready to take (a,b) -> (a,d)
-- and first takes a a->c and gives a function to take (a,b) -> (c,b)

{-
    original bimap g h = first g . second h
                       = (((bimap g id) . (bimap id)) g id) . (((bimap g id) . (bimap id)) id)
                       = ehh...
    
-}


{-2-}
-- Show the isomorphism between Maybe a and:
-- type Maybe' a = Either (Const () a) (Identity a)

--recall
class Functor_ f where
    fmap_ :: (a -> b) -> f a -> f b
    
data Const_ c a = Const_ c

instance Functor_ (Const_ c) where
    -- fmap :: (a -> b) -> Const_ c a -> Const_ c b
    fmap_ _ (Const_ v) = Const_ v

-- the whole point of const is that it ignores the second type entirely

--recall
data Identity_ a = Identity_ a
instance Functor_ Identity_ where
    fmap_ f (Identity_ x) = Identity_ (f x)
    
--now we show: Maybe' a = Either (Const () a) (Identity a) <=> Maybe a = Nothing | Just a
type Maybe_ a = Either (Const_ () a) (Identity_ a)

maybeToBoof :: Maybe a -> Maybe_ a
maybeToBoof Nothing = Left (Const_ ())
maybeToBoof (Just x) = Right (Identity_ x)

boofToMaybe :: Maybe_ a -> Maybe a
boofToMaybe (Left (Const_ ())) = Nothing
boofToMaybe (Right (Identity_ x)) = Just x

-- what is happening here is that Nothing and Const_ () _ are the same singleton. 
-- Const always ignores the second argument, so if its first is a (), then its a (). 
-- explicitly, maybeToBoof Nothing = Left (Const_ ()) fills in the left side of either with
-- a Const () that can take any a (the definition!)
-- Similarly, when we unpack that left side, we just assign it to nothing. Then with



{-3-}
-- data Prelist a b = Nil | Cons a b
----this replaces recursion with the type parameter b...if we recursively
----apply Prelist to itself, we can recover our og List
----show that this is a bifunctor

--recall
class Bifunctor_ f where
    bimap_ :: (a -> c) -> (b -> d) -> f a b -> f c d
    bimap_ g h = first_ g . second_ h
    first_ :: (a -> c) -> f a b -> f c b
    first_ g = bimap_ g id
    second_ :: (b -> d) -> f a b -> f a d
    second_ = bimap_ id

data Prelist a b = Nil | Cons a b

instance Bifunctor_ Prelist where
    -- bimap_ :: (a -> c) -> (b -> d) -> Prelist a b -> Prelist c d
    bimap_ g h = first_ g . second_ h
    -- first_ :: (a -> c) -> Prelist a b -> Prelist c b
    first_ ac plab = case plab of 
        Nil -> Nil
        Cons x othr -> Cons (ac x) othr
    second_ :: (b -> d) -> Prelist a b -> Prelist a d
    second_ bd plab = case plab of
        Nil -> Nil
        Cons x othr -> Cons x (bd othr)
        
-- this one still doesn't really work in my head, but at the same time it does
-- i am so used to recursive list, but Prelist is all about NO recursion. 
-- it seems too easy to just apply bd to othr, but other is of type b!
    


   
{-4-}
-- Show that the following are Bifunctors in a and b:
data K2 c a b = K2 c
instance Bifunctor_ (K2 m) where 
    -- bimap_ :: (a -> c) -> (b -> d) -> K2 m a b -> K2 m c d
    bimap_ g h = first_ g . second_ h
    -- first_ :: (a -> c) -> K2 m a b -> K2 m c b
    first_ g (K2 m) = K2 m
    -- second_ :: (b -> d) -> K2 m a b -> K2 m a d
    second_ g (K2 m) = K2 m

data Fst a b = Fst a
instance Bifunctor_ Fst where
    -- bimap_ :: (a -> c) -> (b -> d) -> Fst a b -> Fst c d
    bimap_ g h = first_ g . second_ h
    -- first_ :: (a -> c) -> Fst a b -> Fst c b
    first_ g (Fst x) = Fst (g x)
    -- second_ :: (b -> d) -> Fst a b -> Fst a d
    second_ g (Fst x) = Fst x

data Snd a b = Snd b
instance Bifunctor_ Snd where
    -- bimap_ :: (a -> c) -> (b -> d) -> Snd a b -> Snd c d
    bimap_ g h = first_ g . second_ h
    -- first_ :: (a -> c) -> Snd a b -> Snd c b
    first_ g (Snd x) = Snd x
    -- second_ :: (b -> d) -> Snd a b -> Snd a d
    second_ g (Snd x) =  Snd (g x)
    
    
    

{-5-}
-- Define Bifunctor in C++...implement bimap for std::pair<A,B>

{-6-}
-- should std::map be considered a Bifunctor in Key and Value? How would you redesign
--  the datatype so that it is? 

-- hmm...it is really just List (Key, Value). List is functorial and Pair is Bifunctorial....
-- but not REALLY: a fundamental property is unique keys. So turning it into a [(k,v)] would make it
-- bifunctorial, but we would lose the unique keys. How can we assure that any function to map K-K' would 
-- be 1-1?





    