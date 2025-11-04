#include <iostream>
#include <functional>
#include <vector>
using std::function;
using std::vector;


// here is the Maybe functor in C++
template<class T>
class optional{
    bool _isValid;
    T _v;
public:
    optional(): _isValid(false){}
    optional(T v): _isValid(true), _v(v){}
    bool isValid()const{return _isValid;}
    T val()const{return _v;}
};

// now we define fmap
template<class A, class B>
function<optional<B>(optional<A>)> fmap(function<B(A)> f){
    return [f](optional<A> opt){
        if (!opt.isValid()){
            return optional<B>{};
        }
        else{
            return optional<B>{f(opt.val())};
        }
    };
}

// or the uncurried version:
template<class A, class B>
optional<B> fmap(function<B(A)> f, optional<A> opt){
    if (!opt.isValid()){
        return optional<B>{};
    }
    else{
        return optional<B>{f(opt.val())};
    }
}



// now we will generalize functors here in C++...with a complication
// in Haskell, we easily parameterized not just our types, but our type constructors
// in C++ to templatize our type constructors, we use a template template!

// template<template<class> F, class A, class B>
// F<B> fmap(function<B(A)>, F<A>);

/*
now it would be nice to be able to write:

template<class A, class B>
optional<B> fmap(function<B(A)> f, optional<A> opt)

but C++ prohibits partial specialization of templates.


so we go back to:

template<class A, class B>
optional<B> fmap(function<B(A)> f, optional<A> opt){
    if (!opt.isValid()){
        return optional<B>{};
    }
    else{
        return optional<B>{f(opt.val())};
    }
}

(which we already defined) It does not cast out template-template version
down to optional, but instead it just works by defining it specifically for
optional<>
*/




// now let's piggy back on the Functor List from Haskell and see it for the vector
template<class A, class B>
vector<B> fmap(function<B(A)> f, vector<A> v){
    vector<B> w;
    std::transform(
        std::begin(v),
        std::end(v),
        std::back_inserter(w),
        f
    );
    return w;
}


// CHALLENGE 3: Implement Reader in C++
/*
fmap :: (a -> b) -> (r -> a) -> (r -> b)

instance Functor ((->) r) where 
    fmap f g = f . g
*/

// an old friend: composition function 
//      (this is flipped from my def in Identity_Composition.cpp to put the onus on the caller to pass in composition order notation)
template<typename T1, typename T2, typename T3>
std::function<T3(T1)> composition(function<T3(T2)> ttt, function<T2(T1)> ott)
{
    return [ott, ttt](T1 x){return ttt(ott(x));};
}

template<class R, class A, class B>
function<B(R)> fmap(function<B(A)> aToB, function<A(R)> rToA){
    return composition(aToB, rToA);
}

// and this compiles !!!! :)



int main(){
    // using vector fmap
    vector<int> v{1, 2, 3, 4, 5};
    for (int i : v){
        std::cout << i << " ";
    }
    std::cout << std::endl;
    auto lambda = [](int x) { return x * 2; };
    std::function<int(int)> func = lambda;
    auto w = fmap(func, v);
    for (int i : w){
        std::cout << i << " ";
    }

    return 0;
}