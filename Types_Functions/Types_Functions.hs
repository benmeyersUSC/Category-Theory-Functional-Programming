
{- mathematical ahh function-}
fact :: Integer -> Integer
fact n = product [1..n]

{-singleton
    
    in this example, f27 actually represents 27 itself
    
    we can have a 1-1 correspondence between elements of a Set (here Integer) 
    and functions from unit () -> element
-}
f27 :: () -> Integer
f27 () = 27

{-unit function: parametrically polymorphic-}
unit :: a -> ()
unit _ = ()


{-memoize is very hard in Haskel...-}



{-bool->bool functions-}
id :: Bool -> Bool
id b = b

nId :: Bool -> Bool
nId b = not b