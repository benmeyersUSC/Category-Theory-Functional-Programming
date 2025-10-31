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

- **Coproducts**
- - reverse the arrows of *product* and you have a *c* with two injections from *a* and *b*
- - - p :: a -> b -> c
- - - p m n = (m, n)
