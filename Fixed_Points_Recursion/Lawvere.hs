
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



{-

    I came across a fantastic version of Lawvere's Theorem. It uses 'function' notation and thinking, but, of course, everything
    applies at the categorical level and therefore reaches more than just the domain of functions. It cuts through the 
    diagonalization examples (which are of course helpful motivators) and plainly shows that fixed points and self-reference
    go hand in hand. Here is the setup:

    n :: Y -> Y
    f :: X -> Y^X :: X x X -> Y :: X -> X -> Y
    -- f is (weakly) surjective, which means that its codomain (Y^X) is fully matched with input members of X

    Let

    f_D(x) = n(f(x)(x))

    Theorem: if there exists a d::X such that f(d) = f_D then n(v_d) = v_d, where v_d = f_D(d)
    -- i.e. if f_D is one of the functions that f returns, then f_D(d) is a fixed point of n

    Why? Let's show with substitution:

    >> sub in d for x
    f_D(d) = n(f(d)(d))

    >> sub in f_D for f(d)
    f_D(d) = n(f_D(d))

    VOILA

    f encodes functions from X -> Y with X values. So we pass d::X and get f_D(x) = f(d)(x). Now we can compose
    this with n to get f_D(x) = n(f(d)(x)). Since f is surjective (any X->Y is encoded with some X), then there must
    be the specific function f_0 such that f_0(x_0) = n(f_0(x_0)(x_0)). This is because n . f_0 :: X -> Y and must be 
    seen by f. But f_0(x_0) is still a function on X, so let's pass in x_0 to get: f(x_0)(x_0) = n(f_0(x_0)(x_0)). 
    That's the fixed point! The composition n . f_0 could only possibly be a function seen by f iff n has a fixed point!
    
    It is so clear and obvious that it often slips my understanding. In Gödel, Cantor, Turing, Tarski, and Russell, this 
    concept is made complex by the specific functions f_D and n. But they are all benefitting from this same principle
    so clearly shown by Lawvere. When f can encode a shifted diagonal, then that shift necessarily must have a fixed point. 
    Just by substituting, we see that if n didn't have a fixed point, then we could never construct a f_D(x) such that it
    equals n(f(x)(x)). If n had a decisive impact, then it could not be ignored. But if there is just one Y that it leaves
    unchanged, then we can build this maniacal f_D. 
-}


{-

f :: X -> (X -> Y) :: X -> Y^X
g :: Y -> Y

X = Natural Numbers (Gödel Numbers)
Y = {0,1} (Provable, not Provable)

f(p:X) = p(n:X)
f(p:X)(n:X) = p(n) 
Where p(n) means passing the number, n, into the 1-free-variable predicate encoded into Gödel Number p. 
When we close a predicate by passing a number, we get a Sentence.

Now let

g(s:Y) = if s then 0 else 1

d(p:X) = f(p)(p)

Prov(p:X) =  Ea: Proof(a, p)
-- there is a proof string, a, for p

U(p:X) = g(Prov(d(p)))
-- U is a predicate on one number. That number, again, is a Gödel Number of a sentence
-- Since U is a predicate on one variable, we can Gödel Number it as well. 

P = GödelNum(U)

G = d(P) 
-- this means passing U's Gödel Number into U

....so

U(P) = -Prov(d(P))

and d(P) = G

U(P) = -Prov(G)......but G is d(P) and P is U's GödelNumber...so G is equivalent to U(P)

G = -Prov(G)
fixed point!
-}