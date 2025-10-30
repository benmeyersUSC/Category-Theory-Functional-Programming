import Data.Char {-for toUpper-}
main = putStrLn "Hello World"

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



