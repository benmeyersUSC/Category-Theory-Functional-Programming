#include <functional>
#include <string>
#include <utility>
using std::function;

template<class T> 
T id(T v){return v;}

template<class A, class B, class C, class D, class X>
class Bifunctor {
    // bimap :: (a -> c) -> (b -> d) -> f a b -> f c d
    function<function<X(C,D)>(function<X(A,B)>)> bimap(function<C(A)> ac, function<D(B)> bd){
        return first(ac)(second(bd));
    }
    // first :: (a -> c) -> f a b -> f c b
    // first g = bimap g id
    function<function<X(C,B)>(function<X(A,B)>)> first(function<C(A)> ac){
        return bimap(ac, id);
    }
    // second :: (b -> d) -> f a b -> f a d
    // second = bimap id
    function<function<X(A,D)>(function<X(A,B)>)> first(function<D(B)> ac){
        return bimap(id);
    }
};


// floatToInt :: Float -> Int
// floatToInt _ = 1

// boolToString :: Bool -> String
// boolToString _ = "Hello!"

// fb_to_is :: (Float, Bool) -> (Int, String)
// fb_to_is (x, y) = (floatToInt x, boolToString y)


// template<class A, class B, class C, class D, class X>
// // bimap :: (a -> c) -> (b -> d) -> f a b -> f c d
// function<function<X(C,D)>(function<X(A,B)>)> bimap(function<C(A)> ac, function<D(B)> bd){
//     return first(ac)(second(bd));
// }

int floatToInt(float f){return 1;}
std::string boolToString(bool b){return "Hello!";}

template<class A, class B, class C, class D>
auto bimap(function<C(A)> f, function<D(B)> g) {
    return [f, g](std::pair<A, B> p) -> std::pair<C, D> {
        return std::make_pair(f(p.first), g(p.second));
    };
}


// in Haskell, an algebraic data type like tree is so easy as a functor:
// data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Functor

// Let's replicate...instead of sum types, we can use a class hierarchy
// unfortunately we cannot make a virtual fmap because it needs to be templated
// by the type it returns and you cannot have virtual template functions.
// we can just implement a generic Tree fmap and use dynamic_cast instead of 
// pattern matching! (also for dynamic cast to work, we need at least one virtual function)

template<class T>
struct Tree {
    virtual ~Tree() {};
};

template<class T>
struct Leaf : public Tree<T>{
    // leaf is just a wrapper around the Identity Functor
    T _label;
    Leaf(T t): _label(t){}
};

template<class T>
struct Node : public Tree<T>{
    Tree<T>* _left;
    Tree<T>* _right;
    Node(Tree<T>* l, Tree<T>* r): _left(l), _right(r) {}
};

template<class T, class R>
Tree<R>* fmap(function<R(T)> f, Tree<T>* t){
    // pattern match Leaf<T>
    Leaf<T>* leaf = dynamic_cast<Leaf<T>*>(t);
    if (leaf){
        // just apply f :: T -> R and rewrap
        return new Leaf(f(leaf->_label));
    }
    // pattern match Node<T> l r
    Node<T>* node = dynamic_cast<Node<T>*>(t);
    if (node){
        // apply fmap recursively to each subtree and rewrap!
        return new Node<R>(fmap<T, R>(f, node->_left), fmap<T, R>(f, node->_right));
    }
    return nullptr;
}

/*
if instead of: 
    data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Functor

we implemented the fmap, it would be:

instance Functor Tree where
    fmap :: (a -> b) -> Tree a -> Tree b
    fmap f (Leaf x) = Leaf (f x)
    fmap f (Node l r) = Node (fmap f l) (fmap f r)

because this was so easy, it can be derived by the compiler! All we have is a simple algebraic
data type:
    Tree<x> = x | (Tree<x>, Tree<x>)   
    f(x) = x + f(x) * f(x)
*/



// {-5-}
// -- Define Bifunctor in C++...implement bimap for std::pair<A,B>
template<class A, class B>
struct BPair {
    A _a;
    B _b;
    BPair(A a, B b): _a(a), _b(b){}
    A fst()const{return _a;}
    B snd()const{return _b;}
};

template<class A, class B, class C, class D>
function<BPair<C,D>(BPair<A,B>)> BPair_bimap(function<C(A)> ab, function<D(B)> bd){
    return [ab,bd](BPair<A,B> pab){
        first(ab)(second(bd));
    };
}
template<class A, class B, class C>
function<BPair<C,B>(BPair<A,B>)> first(function<C(A)> ac){
    return [ac](BPair<A,B> pab){
        return BPair(ac(fst(pab)), snd(pab));
    };
}
template<class A, class B, class D>
function<BPair<A,D>(BPair<A,B>)> second(function<D(B)> bd){
    return [bd](BPair<A,B> pab){
        return BPair(fst(pab), bd(snd(pab)));
    };
}




int main(){
    function<int(float)> fti = floatToInt;
    function<std::string(bool)> bts = boolToString;
    
    function<std::pair<int, std::string>(std::pair<float, bool>)> fb_to_is = bimap<float, bool, int, std::string>(fti, bts);
    
    std::pair<float, bool> input = {3.14f, true};
    auto result = fb_to_is(input);
    
    return 0;
}