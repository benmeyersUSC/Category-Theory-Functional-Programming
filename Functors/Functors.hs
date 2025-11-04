
main = putStrLn "Functors!"

{-
    Functors act as maps between categories
    - they map Objects in C to Objects in D
    - they map morphisms from c_a -> c_b to Fc_a -> Fc_b
    -
    - they must preserve identity and composition!
-}


-- Maybe is a functor:

-- data Maybe a = Nothing | Just a
-- takes a -> Maybe a

-- f :: a -> b

-- f' :: Maybe a -> Maybe b
-- f' Nothing = Nothing
-- f' Just x = Just (f x)

-- Concrete example

isEven :: Int -> Bool
isEven x = mod x 2 == 0

isEven' :: Maybe Int -> Maybe Bool
isEven' Nothing  = Nothing
isEven' (Just x) = Just (isEven x)

-- Generalized fmap for Maybe
fmap :: (a -> b) -> Maybe a -> Maybe b
fmap f Nothing = Nothing
fmap f (Just x) = Just (f x)

{-
    Now we will use Equational Reasoning to prove that id and composition
    are maintained through this Maybe functor
    
    1. Identity
        - show that: fmap id = id
    
        two cases: Nothing and Just
        Nothing: show that 'fmap id Nothing = id Nothing'
        Just: show that 'fmap id (Just x) = id (Just x)'
        
        fmap id Nothing = fmap id Nothing
        = Nothing                           {def of fmap}
        = id Nothing                        {def of id}
        
        fmap id (Just x) = fmap id (Just x)
        = Just (id x)                       {def of fmap}
        = Just x                            {def of id}
        = id (Just x)                       {def of id}

    2. Composition
        - show that: fmap (g . f) = fmap g . fmap f
        
        two cases: Nothing and Just
        Nothing: show that 'fmap (g . f) Nothing = fmap g (fmap f Nothing)'
        Just: show that 'fmap (g . f) (Just x) = fmap g (fmap f (Just x))'
        
        fmap (g . f) Nothing = fmap g (fmap f Nothing)
        = Nothing                           {def of fmap}
        = fmap g Nothing                    {def of fmap}
        = fmap g (fmap f Nothing)           {def of fmap}
        
        fmap (g . f) (Just x) = fmap g (fmap f (Just x))
        = Just ((g . f) x)                  {def of fmap}
        = Just (g (f x))                    {def of composition}
        = fmap g (Just (f x))               {def of fmap}
        = fmap g (fmap f (Just x))          {def of fmap}
        
    
    Hence Maybe is a functor! The categories it maps are the set of types
    to the set of type-parameterized Maybe types. 
    
    What have we done in these above proofs? Specifically the final one. It 
    seems like we are just doing trickery to move symbols around, but we are 
    just following the definitions of all of these three key components: id, 
    fmap, and composition. Selectively subsituting terms and not always 
    exhausting simplification until needed is a key technique in deriving things
    in math. 
-}




-- Abstracting the functor with typeclasses
-- in Haskell, typeclasses allow you to implement trees of interfaces

-- for example:

--(we have to use Eq_ and === instead of Eq and == because they're in Haskell)
class Eq_ a where
    (===) :: a -> a -> Bool


-- it is like an interface: any type that is an instance of this class must implement ==

-- for example
data Point = Pt Float Float
-- can declare itself an Eq instance:

instance Eq_ Point where
   (Pt x y) === (Pt x' y') = x == x' && y == y'


-- Now we can apply this to fmap and generalize a Functor class!
-- there is a key difference though: Eq defined a family of types, 
-- but Functor defines a family of type constructors. That is,
-- Functor is a mapping between types, so to map THAT, we need a meta
-- typeclass. 

--(likewise, Functor already exists, so we use Functor_)
class Functor_ f where
    fmap :: (a -> b) -> f a -> f b
    
instance Functor_ Maybe where
    fmap _ Nothing = Nothing
    fmap f (Just x) = Just (f x)
    
-- there we go!
    


-- if we think coarsely, any parameterized container is a functor
-- they map types to types and can map typed functions to other type funcs

-- let's see List as a functor
data List a = Nil | Cons a (List a)
    
instance Functor_ List where
    fmap_ _ Nil = Nil
    fmap_ f (Cons x t) = Cons (f x) (fmap_ f t)
-- this is powerful (the non-Nil case):
-- like in our Graph_DP work, we process this recursive list recursively.
-- We operate on one element at a time then pass the buck down.
    
    
  
    

{-
    Now departing a bit from the previous intuition of Functors as containers,
    we can see them a bit more radically. What about a mapping from a type to
    a function that returns that type? 
    
    In Haskell, you define the type of a function as so: a -> b is a function
    that goes from a to b. We could also have said: (->) a b. Hmm....so does 
    that mean we can partially apply this function (like any other w two params)?
    Yes. We can do: (->) a, which is a function from a. 
    
    Let's do something a bit different and define a Functor that takes type a 
    and maps to a function returning a, in other words, a function _ -> a. We 
    can call that parameterized argument type 'r'. 
    
    So we want a -> (r -> a)
    this is called the Reader Functor
    
    So our fmap will be:
    fmap :: (a -> b) -> (r -> a) -> (r -> b)
    
    So we have a puzzle: given a function f :: a -> b and a function 
    g :: r -> a, we need a function from r -> b....what if we apply g to a, then
    apply f...composition!
    
-}

instance Functor_ ((->) r) where 
    fmap_ f g = f . g
-- you see, we are still parameterized by r. 
-- but given the parameters to fmap (two functions, but really two types), 
-- we can return that third function (which is a type). This defintion uses
-- the uncurried view for sure (it treats fmap as a function with two params). 
-- We could just as easily partially apply fmap to one function f, then take a
-- '(->) a' to a '(->) b'. 

-- also, if we want to be slick, we could have defined: fmap_ f g = (.)



{-
    Just as morphisms between objects compose, functors between categories
    compose. 
-}

-- Consider:
maybeTail :: [a] -> Maybe [a]
maybeTail [] = Nothing
maybeTail (x:xs) = Just xs
-- this function uses slick Haskell list operators. They make sense. 
-- What it does is helps us recurse through the list. It is not recursive,
-- but it inches forward. Given a list, if it's empty, it gives nothing, otherwise
-- it gives us the tail of the list (discards the first item, the head).

-- But if we look at this through the lens of Functors, it is mapping a parameterized
-- type (list of a, a Functor) to another parameterized type (Maybe list of a, another functor)
-- The return type is a composition of Functors on a!

-- These two Functors have their own fmaps, but what if we want to apply a function f
-- to the contents of Maybe [a]? We need to break through the fmaps to get access to
-- the 'a's themselves. We can use fmap to break through the Maybe to get [a], but 
-- if f is a function on a (type a ->), then we need to break through [] as well. 
-- To do this, we send (fmap f) into the Maybe !

-- a function just on ints
square :: Int -> Int
square x = x * x

-- a Maybe list of ints!
nums :: Maybe [Int]
nums = Just [1, 2, 3]

numsSquared :: Maybe [Int]
numsSquared = fmap (fmap square) nums
-- hmm...
-- the compiler needs to figure out what is going on here.
-- this is it: the inner fmap takes square (Int->Int) to []-square ([Int]->[Int])
-- then the outer fmap fakes []-square ([Int]->[Int]) to Maybe-[]-square (Maybe [Int] -> Maybe [Int])



{-
    Challenges

    1. Can we turn the Maybe type constructor into a Functor with
        fmap _ _ :: Nothing

        This definition would be compatible with that already written for the Nothing case,
        but would steamroll the Just case as well. Let's show -- by using the functor laws --
        that, no, this cannot be used to define a functor of Maybe:

        Let's work through the identity case:

        show: fmap id Maybe a = id Maybe a

            fmap id Nothing = id Nothing
            = Nothing                           {def of fmap}
            = id Nothing                        {def of id}
                Okay, this case worked. What about id (Just x)

            fmap id (Just x) = id (Just x)
            = Nothing                           {def of fmap}
            
            id (Just x)
            = Just x                            {def of id}

            Since Nothing != Just x, we have
            fmap id (Just x) != id (Just x)
            Hence this definition of fmap VIOLATES the id law for functors. 


        show: fmap (g . f) Maybe a = fmap g . (fmap f Maybe a)

            fmap (g . f) Nothing = fmap g . fmap f Nothing
            = Nothing                           {def of fmap}
            = fmap f Nothing                    {def of fmap}
            = fmap (g . fmap f) Nothing         {def of fmap}
            = fmap g . fmap f Nothing           {def of composition}
                Okay, again, the Nothing case works

            fmap (g . f) (Just x) = fmap g . fmap f (Just x) 
            = Nothing

            fmap g . fmap f (Just x)
            = fmap (g . fmap) f (Just x)        {def of composition}
            = Nothing
                Okay this case actually holds too

            But we could not show that: fmap id (Just x) = id (Just x), 
            so this fmap does not satisfy functor-hood.



    2. Prove the functor laws for the Reader functor.
        Reader:
        instance Functor ((->) r) where 
            fmap f g = f . g 

        Prove:
        1: fmap id h = id h
        2: fmap (g . f) h = fmap g . fmap f h
            for some arbitrary reader function h :: r -> a


        fmap id h = id h
        = id . h                            {def of fmap}      
        = id h                              {def of composition}


        fmap (g . f) h = fmap g . fmap f h
        = (g . f) . h                       {def of fmap}
        = (fmap g f) . h                    {def of fmap}
        = (fmap g) . f h                    {def of composition}
        = (fmap g) . fmap r h               {def of fmap}
    
    3. Implement the reader functor in C++
        - see Functors.cpp

    4. Prove the functor laws for the List functor.
        List:
        instance Functor List where
            fmap _ [] = []
            fmap f [x:t] = [(f x) : (fmap f t)]
        
        Prove:
        1: fmap id [a] = id [a]
        2: fmap (g . f) [a] = fmap g . fmap f [a]


        fmap id [a] = id [a] (two cases, [] and [_:_])
            fmap id [] = id []
            = []                                {def of fmap}
            = id []                             {def of id}

            fmap id [x:t] = id [x:t]
            = [id x : fmap id t]                {def of fmap}
            = [x : fmap id t]                   {def of id}
            = [x : t]                           {INDUCTION: assume fmap id t = t}
            = id [x : t]                        {def of id}

        fmap (g . f) [a] = fmap g . fmap f [a] (two cases, [] and [_:_])
            fmap (g . f) [] = fmap g . fmap f []
            = []                                {def of fmap}
            = (fmap g . fmap f) []              {def of fmap}
        
            fmap (g . f) [x:t] = fmap g . fmap f [x:t]
            = fmap g (fmap f [x:t])             {def of composition}
            = fmap g ([f x : fmap f t])         {def of fmap}
            = [g(f x) : fmap g (fmap f t)]      {def of fmap}
                (now, from the left)
            = [(g . f) x : fmap (g . f) t]      {def of fmap}
            = [g(f x) : fmap (g . f) t]         {def of composition}

            now, show:       
            [g(f x) : fmap (g . f) t] = [g(f x) : fmap g (fmap f t)]
            = [g(f x) : fmap g . fmap f t]      {INDUCTION: assume fmap (g . f) t = fmap g . fmap f t}
            = [g(f x) : fmap g  (fmap f t)]     {def of compositon}

        
        This is worth dwelling on. Only in the final proof did we have to work from both left
        and right. We started by reducing the right side, then we paused and started reducing the left side.
        This brought us to:
            [g(f x) : fmap (g . f) t] and [g(f x) : fmap g (fmap f t)]
        and we just had to show that the right sides (the tails) of both of these lists were equivalent. 
        To do this, we used INDUCTION: we assumed that what we were proving held for the recursive case.
        This allowed us to replace 'fmap (g . f) t' with 'fmap g . fmap f t'. This was taken directly from
        the very top of the proof (where we declared what we were proving about [x:t]). We just subbed t in
        for [x:t] because they both represent lists. From there, we just had to rewrite it using composition
        laws and we had our answer!

        Hence List is a functor. 
        
-}


