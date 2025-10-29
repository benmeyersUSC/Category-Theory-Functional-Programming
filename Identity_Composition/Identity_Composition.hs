{-|

just looking for compilation

-}

id :: a -> a
id x = x


f :: Bool -> Int
f x = fromEnum x + 1

g :: Int -> Float
g x = fromIntegral x * 2.0

comp :: (b -> c) -> (a -> b) -> (a -> c)
comp m n = m . n


btf :: Bool -> Float
btf x = (comp g f) x
