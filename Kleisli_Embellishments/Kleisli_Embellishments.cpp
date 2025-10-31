#include <functional>
#include <string>
#include <vector>
#include <algorithm>
#include <cctype>
#include <iostream>
using std::string;
using std::vector;
using std::make_pair;
using std::pair;
using std::function;
/*
How to handle functions that need to have side effects (like logging) in a funcitonal, pure way? 

Using pairs is a start (pass in value and logger, update both purely)
this would allow memoization...but we'd need logger to be identical to get from memo!

instead of pairs and memoizing, why not aggregate the logger with each call
*/

// Suppose we want these functions: toUpper and toWords
// these are normal 
// //---------------------------------------------------------------------------------------------------------
// string toUpper(string s){
//     string result;
//     int (*toupperp)(int) = &toupper;
//     transform(begin(s), end(s), back_inserter(result), toupperp);
//     return result;
// }

vector<string> words(string s){
    vector<string> result{""};
    for (auto i = begin(s); i != end(s); ++i){
        if (isspace(*i)){
            result.push_back("");
        }
        else{
            result.back() += *i;
        }
    }
    return result;
}

// vector<string> toWords(string s){
//     return words(s);
// }
// //---------------------------------------------------------------------------------------------------------

// Now we embellish

// pair whose first is arbitrary, second is string
template<class Z>
using Writer = pair<Z, string>;

// Embellished functions:

Writer<string> toUpper(string s){
    string result;
    transform(begin(s), end(s), back_inserter(result), [](int x){return x - ((x >= 'a' && x <= 'z') * 32);});
    return make_pair(result, "toUpper ");
}

Writer<vector<string>> toWords(string s){
    return make_pair(words(s), "toWords ");
}

// now to compose these into a function to uppercase then split!
// //---------------------------------------------------------------------------------------------------------
// Writer<vector<string>> process(string s){
//     auto p1 = toUpper(s);
//     auto p2 = toWords(p1.first);                            // standard composition
//     return make_pair(p2.first, p1.second + p2.second);      // then we concat the strings!
// }
// //---------------------------------------------------------------------------------------------------------
// now each function seamlessly does its part in continuing the log

/*
The Writer Category
- objects: the types of C++
- morphisms: embellished functions
*/

// for example, to embellish isEven (int -> bool)
Writer<bool> isEven(int n){
    return make_pair(n % 2 == 0, "isEven ");
}

// if Writer is a category, we should be able to compose...let's say with negate
Writer<bool> negate(bool b){
    return make_pair(!b, "Not So! ");
}

Writer<bool> isOdd(int n){
    auto p1 = isEven(n);
    auto p2 = negate(p1.first);
    return make_pair(p2.first, p1.second + p2.second);
}

/*
Thus we see a recipe for Writer morphism composition:
1. p1 <- execute first embellished function
2. p2 <- execute second embellished function on p1.first
3. s <- concat p1.second and p2.second
4. return pair(p2.first, s)
*/

template<class A, class B, class C>
function<Writer<C>(A)> compose(function<Writer<B>(A)> m1, function<Writer<C>(B)> m2){
    // make a lambda A -> Writer<C>
    return [m1, m2](A x){
        auto p1 = m1(x);
        auto p2 = m2(p1.first);
        return make_pair(p2.first, p1.second + p2.second);
    };
}
// since the above works with function pointers, we need to wrap them in functions to make them callable
template<class A, class B, class C>
function<Writer<C>(A)> compose(Writer<B>(*m1)(A), Writer<C>(*m2)(B)){
    return compose<A, B, C>(function<Writer<B>(A)>(m1), function<Writer<C>(B)>(m2));
}

// now process() becomes
Writer<vector<string>> process(string s){
    return compose(toUpper, toWords)(s);
}

// NOT quite done with the category...Identity!
template<class A>Writer<A> identityW(A x){
    return make_pair(x, "");        // empty string so 0 effect whatsoever...Identity!
}

/*
We have types as objects, associatively composable morphisms, and identity!

We could take this further than just strings to any Monoid

delta(compose) would be:
mappend instead of + 

delta(identity) would be:
mempty instead of ""


check Kleisli_Embellishments.hs for the Haskell version!
*/




/* CHALLENGE
    partial functions are not defined for all values of its arg

    can create them with functions that return embellished type optional()
*/
template<class A> class optional{
    bool _isValid;
    A _value;
public:
    optional(): _isValid(false){}
    optional(A v): _isValid(true), _value(v){}
    bool isValid()const{return _isValid;}
    A value()const{return _value;}
};

// optional<double> safe_root(double x){
//     if (x >= 0){ 
//         return optional<double>{sqrt(x)};
//     }
//     return optional<double>{};
// }

/*
Construct the Kleisli category for partial functions (define composition and identity)
*/

// Kleisli
template<class A>
using Partial = optional<A>;

Partial<double> safe_root(double x){
    if (x >= 0){
        return optional<double>{sqrt(x)};
    }
    return optional<double>{};
}

Partial<double> safe_reciprocal(double x){
    if (x != 0){
        return optional<double>{1.0 / x};
    }
    return optional<double>{};
}


template<class A, class B, class C>
function<Partial<C>(A)> compose(function<Partial<B>(A)> m1, function<Partial<C>(B)> m2){
    return [m1, m2](A x){
        auto p1 = m1(x);
        if (!p1.isValid()) return optional<C>{};
        auto p2 = m2(p1.value());                   // we let _isValid and _value play beautifully here
        if (!p2.isValid()) return optional<C>{};
        return p2;
    };
}

template<class A, class B, class C>
function<Partial<C>(A)> compose(Partial<B>(*m1)(A), Partial<C>(*m2)(B)){
    return compose<A, B, C>(function<Partial<B>(A)>(m1), function<Partial<C>(B)>(m2));
}

template<class A> 
Partial<A> identity(A x){
    return optional(x);
}

Partial<double> safe_root_reciprocal(double x){
    return compose(safe_reciprocal, safe_root)(x);
}


Writer<vector<string>> Iprocess(string s){
    return compose(identityW<string>, process)(s);
}

Writer<vector<string>> IprocessI(string s){
    return compose(Iprocess, identityW<vector<string>>)(s);
}

int main(){

    
    vector<string> G = IprocessI("this sentence is false").first;
    for (auto s : G){
        std::cout << s << std::endl;
    }

    
    double _27_ = safe_root_reciprocal(0.00137174211248).value();
    std::cout << std::endl << "27 ~ " << _27_ << std::endl;

    return 0;
}
