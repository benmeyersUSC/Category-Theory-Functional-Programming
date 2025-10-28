**Types and Functions**

- Types are sets of values. 
- - **Bool** is a two-member set (finite).
- - **String** is an infinite set composed of **Char**s, which are a finite set. 
- - **Integer** is an infinite set of ints in Haskell. **Int** is a finite set that corresponds to the **int** in C++.

- **Set** is a category with elements that are objects and arrows, elements and morphisms
- **Bottom** ( _ | _ ) extends every type in Haskell. You can explicitly return it by returning **undefined**. 
- - Functions that return **Bottom** are *partial* whereas those with a definite value for every input are *total*

- **Denotational Semantics**. Every programming construct is given a mathematical interpretation. With that, to prove things about the program, you prove a theorem. 
- - fact n = product [1..n]
- - - this is easy to see mathematically
- - - but what about less obviously mathematical functions? how can these be proven correct like theorems?
- - much easier with *pure functions*

- **Types as Sets**
- - Empty set: **Void**
- - - a function (**absurd**) can take in a **Void** type and return any other type, because it can never be called (you cannot pass anything in!)
- - - absurd **Void** -> a
- - - - *from falsity follows anything*
- - Singleton set: C++ **void**
- - - takes in 'nothing', but really takes in the single dummy value **void**
- - Two element set: **Bool**
- - - any function to **Bool** is a predicate!