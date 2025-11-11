
main = putStrLn "Function Types!"

{-
    We introduce a new universal construction: Function Types.
    Typically, we think of a function in Haskell as f :: a -> b.
    This is the hom-set of a and b and in the category Set, this
    hom-set is an object. In other categories, it can be external
    to the category. In others still, it can also be internal to
    the category, like with Set. But we want to talk about this latter
    case more generally than for the category Set. 
    
    We can say that the function object is called z in some category. 
    Now we are dealing with three objects: a, b, and z. a and b are 
    fixed as argument and return, but we're trying to figure out what 
    z is. What happens when we pass a to a function and get b? Well for
    one thing, we know we need both a and z. We need the product of a and
    z, (z,a) or z x a, to get b. So there is a morphism g :: (z,a) -> b. 
    So this g is part of our universal construction. But the vital key
    that goes with this general pattern is the ranking. Here is how we
    rank candidate z-gs:z and g :: (z,a) -> b is better than z' and 
    g' :: (z',a) if there is a unique map, h, that factorizes z' -> z and 
    g' to g. But given that for g we need to map (z',a) -> (z,a), we need
    to do something a bit more creative. 
    
    We know that products are functorial (endobifunctorial). So we need to
    use it to lift/map a product of MORPHISMS, not a product of objects. 
    What product of morphisms do we want to apply to (z',a) to get (z,a)?
    We want (h,id)...this gives: g' = g . (h,id). Finally, for the universal
    construction, we want a special name for the best z and best g. We will
    call these a=>b and eval. 
        eval :: (a=>b,a) -> b
    So a function object from a -> b is a=>b if its eval is defined as above
    such that for any other object z with a morphism g :: (z,a) -> b, there
    is a unique morphism h :: z -> a=>b that factors through g = eval . (h,id).
    
    Taking a step back, thinking in Haskell, we have the function g :: (z,a) -> b.
    This can essentially be understood as a function on two arguments. And 
    for every g we have the function h :: z -> a=>b, that takes a z object and 
    maps it to the universal function object. So h is a function of one 
    parameter that returns a function from a -> b. So in one case we have 
    a function that takes z and a and gives b. Then we have one that takes z
    and gives a function ready to take in a and give b...they seem very similar.
    And they are! They are the curried and uncurried versions of each other. 
    g, taking in two arguments, is the uncurried version of h, of one argument. 
    h is the curried version of g. 
    
    These are in one-to-one correspondence because for any g there is an h and
    given an h, we can reconstruct g with:
        g = eval . (h,id)
    Recall that eval takes in (a=>b,a) and returns b. So when we first make our
    product of h :: z -> a=>b and id (which leaves a unchanged), then pass through
    eval, we end up with a g that is ready to take in the product of the function
    a=>b and a and give us b. If this sounds confusing it is because the correspondence
    is so tight that we are talking about the same thing in two different ways!
    
    Can we map these to each other? 
    
    Yes:
-}
--       [     g    ]    pA   pB
curry_ :: ((a,b) -> c) -> (a -> b -> c)
curry_ g pA pB = g (pA, pB)
-- we're taking in g :: (z,a) -> b, a, and b
-- then returning the params a and b passed in as co-args to g

--         [     h     ]    (pA,pB)
uncurry_ :: (a -> b -> c) -> ((a,b) -> c)
uncurry_ h (pA, pB) = h pA pB
-- here we take in h :: z -> a=>b, and a pair (a,b)
-- and returns the unpacked pair passed (twice) to h
-- h(a) :: b -> c, h(a)(b) :: c

-- It is easier to see this in application.

-- this is like g (uncurried)
catstr :: (String, String) -> String
catstr (s1, s2) = s1 ++ s2

-- this is like h (curried)
catstr' :: String -> String -> String
-- catstr' s = \s' -> s ++ s'
catstr' = curry_ catstr
-- this compiles!
-- when we curry the uncurried catstr :: (a,b) -> c, we make
-- a function(a) :: b -> c


-- these are really the same thing!
-- they can both be partially applied to give something like:

greet :: String -> String
-- greet = catstr "Hello "
greet = catstr' "Hello "
-- these both compile!


-- Curry is the factorizer function in the universal construction for
-- function object. It takes a weaker function on a product and gives a
-- curried function that returns a function!
factorizer :: ((a,b) -> c) -> (a -> b -> c)
factorizer z = \pA -> (\pB -> z(pA,pB))
-- this is really what curry does:
-- curry_ g pA pB = g (pA, pB)
-- it just uses slicker syntax borrowing from Haskell's already flexible
-- curry flexibility 




{-
    EXPONENTIALS
    
    Buckle up, this is beautiful. We can use alternative notation to 
    represent a function from a -> b. We can rewrite it as b^a. How? 
    Why? Let's start with two concrete types: Bool and Char with 2 and 256
    values respectively. A function from Char -> Bool (such as isupper) has
    two possible outputs (True and False) for all of its 256 possible inputs. 
    We can imagine a function like this as an actual finite table with 2^256
    values. Why? Because for each possible char, there are two possible values!
    Said another way, the answer to this function is literally a 256-length tuple
    of Bools. This function will have just, again, 256 values, but if we think about
    all possible functions from Char -> Bool (all possible 256-length tuples of Bools),
    then obviously there are 2^256 such possibilities! And remember, the function
    object Char -> Bool is the hom-set between Char and Bool, i.e. all possible
    functions. Going the other way, suppose we wanted to think about all the functions
    from Bool -> Char. The trivial one is {True : 'T', False : 'F'}. But we could
    imagine any number of schemes...any number? What number? Well exactly 256^2. 
    This is because we have two values in our tuple, each which can be one of 
    256 possible things. 
    
    Back to Char -> Bool, in general this function type has a memo/table that is:
    Bool x Bool x Bool x ... x Bool, 256 times. In other words, it is an iterated 
    product, a tuple with 256 values. In best words, it is 2^256!
    
    This is immense. This notion of functions as DATA STRUCTURES (either finite or
    at least lazily evaluated) is the essence of the equivalence between
    functions, which are morphisms, and function types, which are objects. A function
    is one morphism from the hom-set, which is the function type!

    Let's try to see this in the lens of the Curry correspondence between:
    (a,b) -> c and a -> b -> c...

            c^(a x b) = c^b^a
            
            since p^q^r = p^(q x r), then...we have it!

            for example, let's imagine a type: data Tri = A | B | C
            and the function type (Tri x Tri) -> Bool. What are the exponentials?

            (Tri, Tri) -> Bool = 2^(3 x 3) = 2^9 = 512
            Tri -> Tri -> Bool = 2^3^3 = 8^3     = 512

    If a function type is a table or a map between sets, then we define it (the function TYPE) 
    by its size. This is how we define all other types (which are sets)...Bool is 2 is {1,0}, 
    Char is 256 is {a, b, ..., A, B, ...}. Tri is 3 is {A, B, C}. Function types are 
    just sets of mappings between sets and their type tells us exactly how many such mappings
    are possible. 
    
    
    CARTESIAN CLOSED CATEGORIES
    
    We have been using the category of Set, with much success. A generalization
    that encapsulates this category is the Cartesian Closed Category. Its requirements
    are: a terminal object, a product of any pair of objects, an exponential for
    any pair of objects. If an exponential is an iterated product (remember Bool x
    Bool x ... x Bool), then a CCC is one that supports products of any arity (size,
    number of iterations). Further, the terminal object can be thought of as a product
    of 0 objects, i.e. Object^0 = 1. This is because the terminal object is a singleton
    to which every object points. 
    
    Terminal object and products have their counterparts: initial object and coproduct.
    A CCC that supports these, and in which product can distribute like so:
        a x (b + c) = a x b + a x c
        (b + c) x a = b x a + c x a
    is known as a bicartesian closed category. 


    some Curry Howard...

    EVAL :: ((a => b), a) -> b IS LITERALLY MODUS PONENS


    We can also show that false statements get the proper treatment in Haskell:

    take this false statement:        a OR b => a

    How do we know it's false? Because it is impossible to implement in Haskell...

    it would be f :: Either a b -> a
    okay...
    f (Left x) = x
    f (Right y) = ...

    With pure functions, it just cannot be defined. EVER.
-}