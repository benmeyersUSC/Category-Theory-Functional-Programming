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


int main(){
    function<int(float)> fti = floatToInt;
    function<std::string(bool)> bts = boolToString;
    
    auto fb_to_is = bimap<float, bool, int, std::string>(fti, bts);
    
    std::pair<float, bool> input = {3.14f, true};
    auto result = fb_to_is(input);
    
    return 0;
}