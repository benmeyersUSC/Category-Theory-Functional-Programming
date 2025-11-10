
main = putStrLn "Lawvere"

-- Self reference
delta :: a -> (a, a)
delta x = (x, x)

-- Second argument passed to first
-- e.g. Gödel Substitution Function
f :: a -> a -> b
--or
f :: (a, a) -> b
f (x, y) = x(y)

-- Function evaluated on diagonal of f
-- e.g. NotProvable(x)
alpha :: a -> a

g :: ...
g = alpha . f . delta

{-
    This is dummy Haskell but it attempts to show what is happening in Lawvere's Theorem: the categorical
    generalization of diagonalization arguments. The theorem really tells you whether or not a certain morphism,
    alpha, has a fixed point. If there is an object G such that g(x) = f(G, x), i.e. g is represented by f, then
    alpha has a fixed point. This is because: 

                                                g(G) = alpha(f(G, G)) 
                                                g(G) = f(G, G)! 
                                                i.e. 
                                                alpha(f(G,G)) = f(G,G)

                                                Fix alpha = G(G)

    Alpha's fixed point is a self-referential object. Alpha is NotProvable(x). If Alpha does not have 
    a fixed point (such as NOT(x) or Cantor's +1 Mod 10), then g(x) is not represented by any G. This means that 
    There is no partially applied version of f (i.e. no G(_) waiting for an argument) represented by f. In the case
    of Cantor's original theorem, since alpha had no fixed point, it was shown that no f(_, x) would ever represent
    what g would create. Same with Barber/Russell: alpha is NOT(x) (applied to the function asking if people cut 
    their own hair), which has no fixed point, so the Barber's row (the one-hot truth table for who cuts his hair) 
    is undetermined. With Gödel, our f takes A and B and feeds B's Gödel Number to A to produce a sentence. Alpha takes
    A's Gödel Number and evaluates whether it represents a provable sentence. Thus f takes in two single-free variable
    predicates, creates a sentence, Alpha takes in a sentence and tells you if it is provable. Thus 
                                                alpha :: Sentence -> Sentence 
    and                                         
                                                f :: Predicate x Predicate -> Sentence 
    Thus alpha(f(delta(A))) <=> g is well-typed and g is in the image of f, i.e. is representable by f. This means that 
    alpha (NotProvable) has a fixed point. The fix point is exactly the G that Gödel creates: G has the same meaning
    (is provably equivalent in arithmetic) as alpha(G). 

    For Turing, it is the other way. g = not(Halt(delta(A))). We take a program, double it through delta, pass that pair
    to Halt (run A as input to A and see if it halts), then we negate that result (by halting if Halt says 1 and running forever
    if Halt says 0). If we had a Halt(P, x) function for all input prorams P and all program inputs x, it would be total.
    Every program P would be representable by Halt, because Halt would have to run it and see if it Halts. Thus even the
    function g = not(Halt(delta(A))), which is just a composed function on a single input, would be representable by Halt.
    This would mean (almost like Gödel) that some 'row' of f = g, which means that alpha = not(x) would need a fixed point...
    But not(x) -> 2 has no fixed points. By definition, what you get out is opposite what you put in. So this is a contradiction
    and we thus have to repeal our assumption that Halt can be completely defined. 

    Lawvere's theorem sits above and predicts the divides in the two outcomes of this common self-referential theme. It pinpoints
    (1) the fundamental weirdness of all of these paradoxes in crisp categorical terms and (2) it tells us the key tension
    separating the cases where ALPHA.F.DELTA CANNOT BE REPRESENTED and those where ALPHA.F.DELTA MUST BE REPRESENTED. 
    
-}
