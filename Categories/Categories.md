**Categories Great and Small**

- Directed Graph -> Category
- - Starting with a directed graph, add two kinds of arrows:
- - - **identity** for each node
- - - **compositions** for any pair of nodes that are composable
- - - - often gives infinite arrows!
- - All possible chains of composable arrows become the morphisms of the category
- - this is a *free category* given by the graph. free construction, completing a structure

- Orders...category?
- - suppose the morphisms are <= and >=
- - - **identity**?
- - - - x <= -> yes!
- - - **composable** (associative)? 
- - - - a <= b and b <= c -> a <= c
- - - this is a *preorder*
- - - - *partial order* would be a <= b and b <= c -> a == b
- - - if all objects relate to each other some way or another, this is a *linear* or *total* order
- - - **Preorder**
- - - - elements are all connected by single morphisms. this is a *thin* category
- - **Hom-set**: C(a,b) or Hom_C(a,b)
- - - set of all morphisms between a and b
- - - in a preorder, Hom-sets contain either singletons or empty
- - - - we can do C(a, a)

- **Monoids**
- - categories with:
- - - **Associative binary operator**
- - - single **unit/identity value**
- - Examples:
- - - addition (+, 0)
- - - multiplication (x, 1)
- - - strings (concat, "")
- - - lists (append, Null)
- - Code shows Monoids as sets with elements and functions
- - - Monoids as categories
- - - - application of binary operator can be seen as *moving or shifting things around in the set*
- - - - - ex. adding 5...0->5, 1->6, 2->7, ..., n->n+5
- - - - - - this is a *function* defined on the *set* of natural numbers. 
- - - - - - for any number *n* there is a function of *adding n*....the *adder* of *n*
- - - - - these adders compose
- - - - - - adder5 + adder7 = adder12
- - - - - - we can replace addition in terms of *adder composition*
- - - - - adder0 is the identity in the set of naturals. it doesn't move anything around, it can compose
- - - - - instead of giving rules of addition, we could just give rules of adder composition and we'd lose no information. the adders compose because functions compose, and we have the identity function adder0
- - - - mapping from *integers* to *adders* corresponds directly to the interpretation of *mappend*: m -> (m -> m)
- - - - - from element of a monoid set to a set of functions acting on that set
- - - - **now forget the integers themselves and just imagine a single object with a set of *adder* functions**
- - - - - Monoid is a single element category. all can be described as a single object category with a set of morphisms that follow the appropriate rules of composition
- - - - we can always extract a set from a single object category: the set of morphisms
- - - - - in other words, we have a single element and a **Hom-set** containing all of the morphisms
- - - - - **Define binary operator:** the monoidal product of two set elements (morphisms in the hom-set) is the composition of those two morphisms
- - - - - - the product of two elements *f* and *g* from Hom_M(m, m) = *f* . *g*
- - - - - - can always recover set-monoid from a category monoid



**Challenges**
- What kind of order is this?
- - a set of sets with the inclusion operation. *A* is included in *B* if every element of *A* is in *B*
- - - **Preorder** because *A* inc *A* is true and if *A* inc *B* and *B* inc *C* then *A* inc *C*. not *partial* because *A* and *B* need not be the same; it can be hierarchical. 
- - C++ types with the following relationship: *T1* is a subtype of *T2* if a pointer to *T1* can be passed to a function expecting a *T2* argument and compiles
- - - seems like **preorder** as well, because *T1* needs to be a child of *T2* in order for this to work. it will be evaluated as a *T2* and only can if its a child. it is not *partial* because if *Tx* and *Ty* are both subtypes of *Tz* they *emphatically cannot* be the same
- Given that **bool** is a set with two types **True** and **False**, show that they form two monoidal sets with respect to **&&** and **||**
- - **&&**
- - - mempty = **True**
- - - - **True** && **True** == **True**
- - - - **False** && **True** == **False**
- - - mappend = **&&**
- - **||**
- - - mempty = **False**
- - - - **True** || **False** == **True**
- - - - **False** || **False** == **False**
- - - mappend = **||**
- - *shown as a proposition in code in Composition.cpp*