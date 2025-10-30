import Data.Char {-for toUpper-}

{-parameterized by variable type a-}
type Writer a = (a, String)
{-Pair is much easier in Haskell-}

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
{- 
    \x means Lambda.x 
    'let' lets you declare aux variables used in 'in' :)
    common to pattern-match pairs with (fst, snd)
-}

{- Identity will be 'return' (for some reason!) -}
return :: a -> Writer a
return x = (x, "")


{- Now our toUpper, toWords, and process functions -}

upCase :: String -> Writer String
upCase s = (map toUpper s, "upCase ") {-map is like Python!-}

toWords :: String -> Writer [String]
toWords s = (words s, "toWords ")

process :: String -> Writer [String]
process = upCase >=> toWords
{-
    the fact that we can define functions like this is incredible
    takes us from String to [String] and all it does is compose two pieces
    
    we define process not by what it does to inputs but WHAT IT IS
-}



{-

template<class A> class optional{
    bool _isValid;
    A _value;
public:
    optional(): _isValid(false){}
    optional(A v): _isValid(true), _value(v){}
    bool isValid()const{return _isValid;}
    A value()const{return _value;}
};


-}

type Partial a = Maybe a

(>==>) :: (a -> Maybe b) -> (b -> Maybe c) -> (a -> Maybe c)
m1 >==> m2 = \x ->
    case m1 x of 
        Nothing -> Nothing
        Just y -> m2 y
        

returnPartial :: a -> Partial a
returnPartial x = Just x

safeRoot :: Double -> Partial Double
safeRoot x
    | x >= 0 = Just (sqrt x)
    | otherwise = Nothing
    
safeReciprocal :: Double -> Partial Double
safeReciprocal x
    | x /= 0 = Just (1.0 / x)
    | otherwise = Nothing
    
    
safeRootReciprocal :: Double -> Partial Double
safeRootReciprocal = safeRoot >==> safeReciprocal



main = do
    let _27_ = Just 0.00137 >>= safeRootReciprocal 
    case _27_ of
        Nothing -> putStrLn "Invalid calculation"
        Just result -> putStrLn $ "27 ~ " ++ show result

