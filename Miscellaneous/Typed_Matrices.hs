
{-# LANGUAGE DataKinds #-} 
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
main = putStrLn "Strongly Typed Matrices!"


{-Defining Natural nums recursively
Nats are either Zero or the successor of another Nat
-}
data Nat = Zero | Succ Nat

{-new types!!!-}
type N0 = Zero
type N1 = Succ N0
type N2 = Succ N1
type N3 = Succ N2
type N4 = Succ N3


{-Defining 2d Integer Matrix-}

{-datatype matrix has params:
 - rows :: Nat
 - cols :: Nat
 - a (valuetype) :: a
-}
data Matrix (rows :: Nat) (cols :: Nat) a where
{-Constructor takes a double list of a-s and returns a matrix-}
  Matrix :: [[a]] -> Matrix rows cols a
  
  
m13 :: Matrix N1 N3 Integer
m13 = Matrix [[1, 2, 3]]

m34 :: Matrix N3 N4 Integer
m34 = Matrix [[1, 2, 3, 4],
              [5, 6, 7, 8],
              [9, 10, 11, 12]]

{-we define matmul by enforcing dimension agreement!-}
matmul :: Matrix r c a -> Matrix c k a -> Matrix r k a
matmul (Matrix m1) (Matrix m2) = Matrix [[]]


m14 :: Matrix N1 N4 Integer
m14 = matmul m13 m34


matmul11t