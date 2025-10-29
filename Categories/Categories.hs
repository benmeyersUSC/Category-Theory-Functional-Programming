
class MyMonoid m where
    mempty :: m
    
    {- this can be seen as 
        m -> (m -> m)        = type m giving a function m->m
        or (m -> m) -> m     = function of two args returning m 
    -}
    mappend :: m -> m -> m
    
    
    
instance MyMonoid String where
    mempty = ""
    mappend = (++)
    {-
    importantly, we're assigning a function by equality, 
    not the equality of values produced by the function
        
        
    the latter is called a point-wise (or extensional) definition
    - shows how two points reach the same point through two morhpisms
    
    the former is a point-free definition, just equating arrows
    
    -}
    

{-
In Haskell, any infix, like ++, can be expressed as prefix with parens


String a :: "Hello" ++ "World"

equals

String b :: (++) "Hello" "World"

-}

main :: IO ()
main = putStrLn "Hello World"