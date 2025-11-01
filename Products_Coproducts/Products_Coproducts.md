**Products and Coproducts**

- *Universal Construction*
- - a common construction in Category Theory for defining objects by their relationships
- - common way: pick a shape, as described by objects and morphisms, and look for it wherever you can
- - - constraint is needed for precision (as opposed to high recall for very general shapes)


- **Initial Object**
- - in partially ordered sets (*posets*) there could be a notion of the 'smallest' object
- - - one that is, say *less than*, all others
- - these are directed categories, with at most one arrow connecting any two objects
- - thus the **Initial Object** (if there is *one*) is one that has *one and only one* morphism going to *any* other object in the category
- - - this doesn't ensure **Uniqueness** but it does ensure *uniqueness up to isomorphism*
- - Example
- - - In the category of sets and functions, the initial object is the **Empty Set**
- - - - in Haskell, this is **Void** (no C++ equivalent)
- - - - the unique polymorphic function is **absurd**
- - - - - absurd :: Void -> a
- - - - - - this family of morphisms makes **Void** the initial object

- **Terminal Object**
- - in posets, this is the greatest object
- - the **Terminal Object** is the one that has *one and only one* morphism coming from *any* other object in the category
- - Example
- - - in the category of sets and functions, the terminal objecy is the **Singleton Set**...this at first felt unintuitive...how is the singleton the greatest set? it certainly does not have the most elements...but perhaps it has the most mappings-to, because literally anything--nay, *everything*--maps to it.....
- - - - in Haskell, this is **()** and in C++ it is **void**
- - - - unique polymorphic function is **unit**
- - - - - unit :: a -> ()
- - - - - unit _ = ()
- - - - it has to be **()/unit** because this is the sole morphism. something like **Bool** has at least one other function to it:
- - - - - yes :: a -> Bool
- - - - - yes _ = True
- - - - or
- - - - - no :: a -> Bool
- - - - - no _ = False


- **Duality**
- - the definitions of **Initial** and **Terminal** were mirror images of each other. the only difference is the *direction* of the morphisms
- - For every category, **C**, we can define its opposite, **C^op**
- - - automatically satisfies definition of a category, if arrows are swapped simultaneously
- - compositions also go in reverse
- - - original
- - - - **f :: a -> b**
- - - - **g :: b -> c**
- - - - **h :: a -> c**
- - - - -  **h = g . f**
- - - opposite
- - - - **f^op :: b -> a**
- - - - **g^op :: c -> b**
- - - - **h^op :: c -> a**
- - - - - **h^op = g^op . f^op**


- **Isomorphisms**
- - isomorphic objects look identical under our instruments
- - every part of one corresponds to those of the other
- - mathematically, it means there is a mapping from *a* to *b* and from *b* to *a* that are inverses of each other
- - category theoretically, it means an *invertible morphism*, or pair of morphisms, one being the other's inverse
- - - **Inverse**: morphism *g* is the inverse of morphism *f* if their composition = *Identity*
- - - - linalg
- - - **f . g = id**
- - - **g . f = id**
- - Proof of *uniqueness up to isomorphism* of **Initial/Terminal** objects (all reasoning for **Inverse** maps to that of **Terminal**)
- - - - wait, **C** and **C^op** are isomorphic...the **Op** morphism across these categories is invertible!
- - - suppose two initial objects *i* and *j*
- - - since *i* is initial, there's a single morphism *f* :: *i* -> *j*
- - - by the same token, there is a single *g* :: *j* -> *i*
- - - ***g* . *f*** must be *i* -> *i*
- - - since *i* is initial, there is only one morphism from *i* -> *i*
- - - since we are in a category, we know there is *id* :: *i* -> *i*
- - - since we know there is only room for one such morphism, it must be *id*
- - - same reasoning for ***f* . *g***
- - - thus *f* and *g* are inverse and any two initial objects, *i* and *j*, are isomorphic
- - - - the uniqueness of the morphism from *i* to *i* (definition of **Initial**) was vital to the proof of *uniqueness up to isomorphism*
- - - - why *uniqueness*? 
- - - - - because *i* then becomes *unique up to **unique** isomorphism*
- - - - - - in principle, objects can have multiple isomorphisms, but not the **Initial**
- - - - - - *uniqueness up to **unique** isomorphism* is key to all *universal construction*s
- - Proof for **Terminal** (why not)
- - - suppose two terminal objects *i* and *j*
- - - since *i* is terminal, there must be a single morphism *f* :: a -> *i*
- - - by the same token there is a *g* :: a -> *j*
- - - ***g* . *f*** must be *i* -> *i*
- - - since *i* is terminal, there is only one morphism *i* -> *i*
- - - since this is a category, there must be *id* :: *i* -> *i*
- - - there is only one such morphism (terminal), so it is *id*
- - - same reasoning for ***f* . *g***
- - - - once again, the uniqueness of morphisms a -> **Terminal** was key to the proof


- **Product**
- - pattern/shape/universal construction where *product* has two morphisms to its constituents
- - - p :: c -> a
- - - p = (a, b) = a
- - - q :: c -> b
- - - q = (a, b) = b
- - there can be many products of two constituent elements, but they have different levels of *universality* or *reducedness*
- - - *c* is better than *c'* if there exists a morphism *m* where
- - - - m :: *c'* -> *c*
- - - - *p* = *p'* . *m*
- - - - *q* = *q'* . *m*

- **Coproduct**
- - reverse the arrows of *product* and you have a *c* with two injections from *a* and *b*
- - - i :: a -> c
- - - j :: b -> c
- - ranking is reverse:
- - - *c* is better than *c'* (who has *i'* and *j'*) if there is an m from *c* to *c'* that factorizes *i* and *j* into *i'* and *j'*:
- - - - *i'* = m . *i*
- - - - *j'* = m . *j*
- - - the **best** Coproduct has a unique such *m* that is unique up to unique isomorphism


- **Asymmetry**
- - Just as we defined the **terminal object** with a flipping of arrows from the **initial object**, we defined the **coproduct** by way of flipping some of the arrows for **product**. But in the category of sets, the **initial** and **final** objects are very different; and the **product** and **coproduct** are also different. 
- - Eventually, **product** will act like *multiplication* and **coproduct** will act like *summation*
- - - with finite sets, the size of the **product** is the *product* of the sizes of the two sets and the size of the **coproduct** is the *sum* of the sizes
- - thus the category of sets is not symmetric with the inversion of arrows
- - Also whereas the Empty Set has arrows to everything (and none to it), the Singleton Set has all arrows pointing to it, but also points to every set
- - - see exactly what these morphisms are in Products_Coproducts.hs!
- - - - spoiler: **they are the *p* and *q* projections from the Product candidate ()**

- **Surjective / Onto**
- - Surjective functions map to entire codomain
- - - TO Terminal: 
- - - - unit :: a -> () is surjective because every output possible is hit
- - - FROM Terminal:
- - - - () -> a is NOT surjective because this can only be defined for one element of a

- **Injective / One-to-one**
- - Injective functions map every input to a different output
- - - FROM Initial:
- - - - Void -> a is injective (but vacuously so, because there are no inputs)
- - - FROM Terminal:
- - - - () -> a is injective because there is only one input
- - - TO Terminal:
- - - - a -> () is maximally NOT injective, because every input collapses to the same

- **Bijection**
- - symmetric between domain and codomain. invertible!
- - in the category of sets, isomorphism is the same as bijection

- small domain --> big codomain (must embed)
- - injective 
- - NOT surjective
- big domain --> small codomain (must collapse)
- - NOT injective
- - surjective



**CHALLENGES**
1. What is the Product in a poset? 
    * In a poset, the objects are quantifiable things (say integers)
    * Morphisms are <=
    * a -> b means a <= b
    * Product C such that:
        * C -> A and C -> B
        * means C <= A and C <= B
            * **C: some (integer) lower than both A and B**
        * Universal Product:
            * P' = P . M
            * Q' = Q . M
                * any **C** lower than the greatest-less-than can be mapped directly to **C_universal** with M
2. What is the Coproduct in a poset? 
    * By a similar token, let's define the coproduct universally:
    * Coproduct C such that:
        * A -> C and B -> C
        * means A <= C and B <= C
            * **C: some (integer) greater than both A and B**
        * Universal coproduct is the lowest such **C**
            * I' = M . I
            * J' = M . J
                * The best injection's result can always just be mapped to the worse one uniformly
3. Prove that **int** with the below I and J is a worse coproduct in C++ than **Either**
    * int i(int n){return n;} 
    * int j(bool b){return b ? 0 : 1;}                
        * see Products_Coproducts.cpp
4. Following the above, why *could **int** not possibly be better than **Either***?
    * hmmm...give an example where **int** loses information...well, in the case where n = 0 or n = 1, you're losing informaiton...but this seems more product-relevant
    * **step back**. I need to just try to map it back 
        * see Products_Coproducts.cpp!
    * the essence is this: we could define **2** m functions from **int** -> **Coproduct/Either**...we never get a **bool** from **int**'s **J** function. thus we could say m either gives a **True** or, in a separate definition, **False**
5. What if **int** had the following *i*?
    * int i(int n){if (n < 0){return n;} return n + 2;}
        * see Products_Coproducts.cpp 

6. Come up with another inferior candidate for coproduct of **int** and **bool** that fails because it has too many morphisms from it to **Either/Coproduct**
    * what about **bool**. we could map **int** -> **0** and **bool** -> **1**
    * we could definitely map **Either/Coproduct** to this (so **Either/Coproduct** is clearly better)
    * but we know it couldn't possibly be right because to map **bool** to even just **int**, we have infinite choices because the information is all lost!
- - - 
**Reflections**


- I really struggled through this chapter because Bartosz was beginning to make large intuitive claims about these tools (because, yes, they are getting advanced and powerful) that I was still trying to understand. The initial and terminal objects make sense to me. The discussion of Injective, Surjective, and Bijective really were not helped by him. I think sticking with concrete functions in math or programming would have helped him. I hope he returns to it. I had to understand those with external help....But anyway, Products made a lot of sense but Coproduct was really challenging me.
- - I worked and then stepped away, went to the gym, and was talking with a good CS friend about what I'd been reading. Product, again, was easy to explain and motivate...coproduct was harder...but as I was explaining all of this to him (particularly the notion of ranking candidates for a universal construction), I was being forced (successfully) to deeply think through and understand what we're doing here. *What is a universal construction? Who cares? why such tight rules if we are just talking about abstract structural patterns? why the ranking?*
- - - It was really in explaining the product (first as a std::pair and then as a <= in a poset) that made it click. Specifically the discussion of the ranking of candidates. 
- - - It makes so much sense: the point of category theory is a framework for making structural or relational analogies across systems/fields/branches. Yes, the notation of basic category theory (objects + morphisms) is vague and abstract, but that is necessary for the universality we seek. Once you define these terms, though, things get very rigorous and stringent and specific very quickly. This is because we need to *know exactly what we're talking about!* An analogy is only as good as the faithfulness of its mapping. And if I know something about std::pair products (the universal prodcut construction in programming), then when I go look for them in another field, I want to make sure I have really found the product. Whatever property we're trying to exploit from the std::pair is intrinsic to that minimal setup of the product. so it is important to be able to know *the best* object that satisfies the constraints of a universal construction, not just any that does. The whole point in identifying these universal constructions is to define insights about their intrinsic structure, which is described by their relationships. 
- Anyway, I feel like there is more to say, but three cheers for trying, stepping away, re-engaging, discussing, writing. 
- - Another beautiful moment was when I was answering challenges **1** and **2**. I was entirely overthinking what the Product in a poset is. And I ignored Bartosz's hint (*use the universal construction*)! I was thinking too much in terms of a std::pair...i have the two objects, i can get first and second...so for posets I need to obtain the quantity and return the thing that satisfies what the user is looking for...I was getting lost and off track.
- - - then I stepped back. What is the product? it is: P:: C -> A and Q:: C -> B. What is -> in poset? it is <=. So we need C <= A && C <= B. That's the answer!
- - - this is the point of the universal construction. and this is the point of the challenges. it is so easy to swallow pages in a book, write localized chunks of code related to a category, etc. It is easy to get along when you're following along! When pushed to your own devices, though, you have to think. and in category theory, to think, you must think structurally, *in terms of the universal construction*...this is why we're here: ***STRUCTURE***
