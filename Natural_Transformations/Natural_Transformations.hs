
main = putStrLn "Natural Transformations"

{-
    Natural transformations are mappings between functors that move across
    the same categories. If we have a functor F and a functor G from C to D,
    and we have a morphism, f :: a -> b, in C, then we should be able to get
    from Fa -> Gb in two ways...first we see the morphism:
                            alpha_a :: F a -> G a
                            alpha_b :: F b -> G b
    If we want to go from Fa -> Gb, we should be able to either do: 
                            alpha_b . Ff
    or
                            Gf . alpha_a
    In other words:
                       Gf . alpha_a = alpha_b . Ff
    or in Haskell, with type inference:
                     fmap f . alpha = alpha . fmap f
    If we think about functors as type-parameterized containers, then going from
    Fa -> Gb means both changing the internal types (template type) from a to b
    and changing from one container, F, to another, G. We should be able to do
    either first; those operations should be orthogonal. 
-}

-- let's bring in our functor...
class Functor_ f where
    fmap_ :: (a -> b) -> f a -> f b
-- and make sure list is one
instance Functor_ [] where
    fmap_ _ [] = []
    fmap_ f (x:xs) = (f x : fmap_ f xs)
-- and maybe
instance Functor_ Maybe where
    fmap_ _ Nothing = Nothing
    fmap_ f (Just x) = Just (f x)

-- natural transformation in Haskell: List a -> Maybe a
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:rst) = Just x
-- this is parameterized by a, and compiles, so it works for all a.
-- this is parametric polymorphism; in Haskell this is required and it
-- tells us that we have a natural transformation...why?
-- because this is an alpha! It works for a, so it works for b. And since 
-- [] and Maybe are functors, they already have their own fmaps! We have the square.

-- we can verify our natruality condition: fmap f . alpha = alpha . fmap f
-- we have two cases
{-
    We can verify the two cases of our naturality condition: 
                    fmap_ f . safeHead = safeHead . fmap_ f
    (1)
    fmap_ f (safeHead []) = fmap f Nothing = Nothing
    =
    safeHead . (fmap_ f []) = safeHead [] = Nothing 
    
    (2)
    fmap_ f (safeHead (x:xs)) = fmap_ f (Just x) = Just (f x)
    =
    safeHead . (fmap_ f (x:xs)) = safeHead (f x : fmap_ f xs) = Just (f x)
    
    
    there we go
-}


-- CONST natural transformations
data Const_ c a = Const_ c

instance Functor_ (Const_ c) where
    -- fmap :: (a -> b) -> Const_ c a -> Const_ c b
    fmap_ _ (Const_ v) = Const_ v
    
length_ :: [a] -> Const_ Int a
length_ [] = Const_ 0
length_ (x:xs) = Const_ (1 + unConst (length_ xs))

unConst :: Const_ c a -> c
unConst (Const_ x) = x



-- READER FUNCTOR
newtype Reader e a = Reader (e -> a)
-- takes two types and gives the exponential of them 

instance Functor_ (Reader e) where
    fmap_ f (Reader g) = Reader (\x -> f (g x))
-- g :: e -> a, f :: a -> b, fmap f (Reader e) :: e -> b
-- we apply g to x (type e) to get an a, then map that to b using f!

-- For any type e, we can define a family of natural transformations from
-- (Reader e) to any Functor F. The members of this family will always be
-- in 1-1 correspondence with the members of F e. 

-- Using the example of (Reader ())...this just takes a type a and returns a
-- function () -> a, which just picks a value from a. Thus there are 
-- as many of these functions as there are in a. 

-- Consider transformations from Reader () a to Maybe a.
-- alpha :: Reader () a -> Maybe a
-- this has two possibilities:
-- dumb (Reader _) = Nothing
-- obivous (Reader g) = Just (g ())

-- There are, as predicted, two of these functions, corresponding to the two
-- members of F e = Maybe () : Nothing and Just ()...This is the Yoneda Lemma



-- BEYOND NATURALITY
-- all parametrically polymorphic functions between two functors are natural 
-- transformations. To compile, they must hold for all functors and types. Since 
-- all algebraic data types are functors, all PP functions between them are NTs. 

-- But what about contravariance? The (Reader e) is covariantly functorial in its
-- return type. Function types (like Reader) are not covariant in their argument
-- type. 

--recall
newtype Op r a = Op (a -> r)
-- this is contravariant in a

class Contravariant_ f where
    contramap_ :: (b -> a) -> (f a -> f b)
    -- contramap_ f g = g . f
-- f (which takes b -> a) is first applied to a, giving b. Then we apply
-- g(b) which gives us fb! (This gets confusing, but just trace through it!)

-- and then we can say:
instance Contravariant_ (Op r) where
    contramap_ f (Op g) = Op (g . f)
-- again g :: a -> r, f :: b -> a, so the composition gives b -> r


-- we can write a polymorphic function from Op Bool -> Op String
predToStr :: Op Bool a -> Op String a
predToStr (Op f) = Op (\x -> if f x then "T" else "F")
-- any function -> Bool can be turned into one -> String!

-- since the two functors are not covariant, it is not a NT, but it fulfills
-- the 'opposite' naturality conditions:

{-
contramap_ f . predToStr = predToStr . contramap_ f

contramap_ :: (b -> a) -> (Op Bool a -> Op Bool b)
and 
contramap_ :: (b -> a) -> (Op String a -> Op String b)

so we either make a b->String then contramap it to another arg type with cmap,
or we first change it to the different arg -> Bool, then change it to String

totally the same vibe of a natural transformation. We have a square here!
-}



{-  So...since Functors are objects and we now have morphisms between them, 
    do they form a category? YES! Between two categories C and D, there is a 
    set of functors (objects) and their natural transformations (morphisms).
    We need to define composition and identity. Composition means going from:
                            alpha_a :: Fa -> Ga
    and
                            beta_a :: Ga -> Ha
    to
                        beta_a . alpha_a :: Fa -> Ha
    This is the a component of the transformation. We already know how to do the
    b part and composing them is just as easy. All together, we have the relationship:
                       (beta . alpha)_a = beta_a . alpha_a 
    And from here, we have the full natural transformation relationship:
                  Hf . (beta . alpha)_a = (beta . alpha)_b . Ff
    Think carefully why the b version has to be applied to the Ff case...it is 
    because Ff already takes us from Fa -> Fb, so we want to now transform the 
    b-parameterized functor F to that of H. 
    
    
    
    So there is a functor category between C and D. It can be written Fun(C,D) or
    [C,D] or...D^C....HMMM! This suggests that the functor category is a function 
    type between C and D...Is this the case? Well we know there will have to be 
    C different mappings (one for each object in C). This is how many items
    are in each 'row' of the function object table. Then each value in each row 
    can be one of the elements of D! So that gives us D^C! Just as from Char -> Bool,
    we have rows of 256 Boolean values, giving 2^256 possible rows!
    
    
    Let's take a step back and look at the abstractions we've crafted. We have categories,
    which contain objects (sets) and morphisms (functions). These categories are 
    objects in Cat. Morphisms in Cat are functors. The Hom-set in Cat is a set of 
    Functors. For instance, Cat(C,D) is a set of functors between C and D. 
    
    A Functor category [C,D] is also a set of functors between two categories (plus
    natural transformations as morphisms). Its objects are the same as those in the
    Hom-set Cat(C,D). Moreover, [C,D] must also be an object in Cat. So we have an object
    and a Hom-set in the same category being the same thing. This is exactly like the
    exponential object! In Hask, we had function objects a=>b which were also the Hom-set
    Hask(a,b).
    
    To define the exponential, we needed a product. Categories (small) are sets of
    objects and we know how to define products of sets. An object in C x D is just 
    a pair of objects (c, d); one in C and one in D. A morphism between two such pairs
    (c,d) to (c',d') is a pair of morphisms (f,g) where f :: c -> c' and g :: d -> d'.
    These pairs of morphisms compose component wise, so Cat is really a closed cartesian 
    category! There is an exponential object D^C for any pair of categories. These 
    objects are Functor categories between C and D. 
-}
    
{-
    2-CATEGORIES & HORIZONTAL NATURAL TRANSFORMATIONS

    In Cat, any Hom-set is a set of functors. But it is more than a set, it is a category
    of functors, where natural transformations are morphisms. Since functors, though, are
    morphisms in Cat, then natural transformations are morphisms between morphisms. (Levels!)

    This represents a 2-category: objects (categories), 1-morphisms (functors), and
    2-morphisms (natural transformations).

    So instead of the Hom-set between two categories C and D, we have a Hom-category D^C. 
    Regular functor composition: a functor F from D^C composes with a functor G from E^D to
    give G . F from E^C. This is horizontal. We also have vertical composition inside each
    Hom-category: compositions of natural transformations between functors. 

    With two kinds of compositon (G . F = E^C and Fa -> Ga -> Ha), how do they interact?

    Let's pick two functors (1-morphisms) in Cat:
                                    F :: C -> D
                                    G :: D -> E
    and their composition:
                                G . F :: C -> E
    Then we have two natural transformations, alpha and beta, that act on F and G:
                                    alpha :: F -> F'
                                    beta :: G -> G'
    Again, F and F' both take us from C -> D and G and G' both take us from D -> E. We cannot
    directly compose alpha and beta, because alpha's target (F') is different from beta's source (G).
    They are members of two different categories, namely D^C and E^D. But we can compose F and G and 
    F' and G'. What is the relationship between G' . F' and G . F?

    Using alpha and beta, we can define a natural transformation from G' . F' to G . F!
    We start with an object a in C. It gets mapped to both Fa and F'a in D. There is a component
    of alpha:
                                alpha_a :: Fa -> F'a
    When going to D from here, we get another bifurcation to make four objects: G(Fa), G(F'a), G'(Fa), G'(F'a).
    We have four morphisms forming a square. Two from alpha, two from beta.The two morphisms that take us 
    from 2 -> 4 are the components of beta:
                                beta_Fa :: G(Fa) -> G'(Fa)
                               beta_F'a :: G(F'a) -> G'(F'a)
    And then the two others are images of alpha_a, under the two functors:
                              G_alpha_a :: G(Fa) -> G(F'a)
                             G'_alpha_a :: G'(Fa) -> G'(F'a)    
    Our goal is to go from G(Fa) to G'(F'a), a candidate for the natural transformation connecting the
    two functors G' . F' and G . F. There are actually two paths:
                              G'_alpha_a . beta_Fa
                                beta_F'a . G_alpha_a
    These are equivalent routes. This is one component (a) of the full natural transformation from
    G . F to G' . F'...this is called the horizontal composition of alpha and beta:
                           beta . alpha :: G . F -> G' . F'
    To stay on top of this: F and F' both go from C -> D, G and G' from D -> E. We are connecting two ways
    to get from C -> E and showing that there are two routes to do so (through these four functors). 
    We already dealt with natural transformations between F and F'. They are morphisms between functors. 
    That morphism is a set of arrows. Then we have the same from D -> E, with G and G'. They have their own 
    set of morphisms/arrows that are natural transformations. The composition of these two sets of arrows
    is the horizontal composition:
             (beta' . alpha') . (beta . alpha) = (beta' . beta) . (alpha' . alpha)
-}
    
{-
    CHALLENGES
-}

{-1-}
-- define a natural transformation between Maybe and List. Prove naturality. 

maybeList :: Maybe a -> [a]
maybeList Nothing = []
maybeList (Just x) = [x]

{-Now prove naturality: fmap_ f . maybeList = maybeList . fmap_ f

fmap f . (maybeList Nothing) = fmap f [] = []
maybeList . (fmap f Nothing) = maybeList Nothing = []

fmap f . (maybeList (Just x)) = fmap f [x] = [f x : fmap f []] = [f x]
maybeList . (fmap f (Just x)) = maybeList Just (f x) = [f x]
-}


{-2-}
-- Define at least 2 natural transformations between Reader () and List.
-- How many lists of () are there? 

unitToNil :: Reader () a -> [a]
unitToNil _ = []

unitToListUnit :: Reader () a -> [a]
unitToListUnit (Reader g) = [g ()]
-- g :: () -> a, so when we apply to (), we get an a!

--but we could also do:
unitToListUnit2 :: Reader () a -> [a]
unitToListUnit2 (Reader g) = [g (), g ()]

-- we could define this for 3, 4, 5, ..., n. Why? because there is an infinity
-- of different [()] structures! Yoneda!

{-3-}
-- Now do it for Reader Bool a -> Maybe a

toBoolToJustT :: Reader Bool a -> Maybe a
toBoolToJustT (Reader g) = Just (g True)

toBoolToJustF :: Reader Bool a -> Maybe a
toBoolToJustF (Reader g) = Just (g False)

toBoolToNothing :: Reader Bool a -> Maybe a
toBoolToNothing (Reader _) = Nothing

-- there are three possible Maybe Bools: Nothing, Just True, Just False


{-4-}
-- create some test cases for the opposite naturality condition for transformations
-- between two Op functors. 

opBI :: Op Bool Int
opBI = Op (\x -> x > 0)

fSI :: String -> Int 
fSI x = read x

intPredToString :: Op Bool Int -> Op Bool String 
intPredToString (Op g) = Op (g . fSI)
-- g :: Int -> Bool, we want to make String->Bool
-- so f :: String -> Int, then g :: Int -> Bool

-- lets do another

opMI :: Op Int (Maybe a)
opMI = Op (\x -> case x of
    Nothing -> 0
    (Just y) -> 1)

fLM :: [a] -> Maybe a
fLM [] = Nothing
fLM (x:xs) = Just x

listToInt :: Op Int (Maybe a) -> Op Int [a]
listToInt (Op g) = Op (g . fLM)

-- boom!

{-5-}
-- Show that the horizontal composition of natural transformations satisfies
-- the naturality condition. (Use components).

-- Show:               G'(α_a) ∘ β_Fa = β_F'a ∘ G(α_a)

--                                      F :: C -> D
--                                      G :: D -> E

--                                  alpha :: F -> F'
--                                   beta :: G -> G'

--                                alpha_a :: Fa -> F'a

--                                beta_Fa :: G(Fa) -> G'(Fa)
--                               beta_F'a :: G(F'a) -> G'(F'a)

--                              G_alpha_a :: G(Fa) -> G(F'a)
--                             G'_alpha_a :: G'(Fa) -> G'(F'a)   

--                   G'_alpha_a . beta_Fa = beta_F'a . G_alpha_a
--                                EXPAND LEFT TYPES
--[G'(Fa) -> G'(F'a)] . [G(Fa) -> G'(Fa)] =
--  G(Fa) -> G'(Fa) -> G'(Fa) -> G'(F'a)  =
--  G(Fa) --[DROP COMPOSITION]-> G'(F'a)  =
--  G(Fa) ---------------------> G'(F'a)  =
--                                EXPAND RIGHT TYPES
--                                        = [G(F'a) -> G'(F'a)] . [G(Fa) -> G(F'a)]
--                                        = G(Fa) -> G(F'a) -> G(F'a) -> G'(F'a)
--                                        = G(Fa) --[DROP COMPOSITION]-> G'(F'a)
--                                        = G(Fa) ---------------------> G'(F'a)
--                                      EQUATE
--                       G(Fa) -> G'(F'a) = G(Fa) -> G'(F'a)


{- 5 continued...

    Concrete example:
        - F :: Reader Bool a
        - F' :: Reader String a
        - G :: Maybe a
        - G' :: List a
        
    again to show: G'_alpha_a . beta_Fa = beta_F'a . G_alpha_a
-}
strToBool :: String -> Bool
strToBool "" = False
strToBool s = True

alpha :: (Bool -> a) -> (String -> a)
alpha g = \x -> g (strToBool x)

beta :: Maybe a -> [a]
beta = maybeList

betaF :: Maybe (Bool -> a) -> [Bool -> a]
betaF = beta
betaFPrime :: Maybe (String -> a) -> [String -> a]
betaFPrime = beta
gPrimeAlpha :: [Bool -> a] -> [String -> a]
gPrimeAlpha = fmap_ alpha 
gAlpha :: Maybe (Bool -> a) -> Maybe (String -> a)
gAlpha = fmap_ alpha

--                                 G(Fa)      ->     G'(F'a)
horizontalTransformation :: Maybe (Bool -> a) -> [(String -> a)]
-- G'_alpha_a . beta_Fa
horizontalTransformation = gPrimeAlpha . betaF
--OR
hT2 :: Maybe (Bool -> a) -> [(String -> a)]
-- beta_F'a . G_alpha_a
hT2 = betaFPrime . gAlpha

-- THIS ALL COMPILES :)

