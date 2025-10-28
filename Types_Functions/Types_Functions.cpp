
#include <iostream>
#include <functional>
#include <unordered_map>

// memoize challenge: 
// - function that takes in pure function f 
// - returns function that behaves like f but only calls f once per arg
// - it internally stores returns for each input, then uses that as memo
template<typename T1, typename T2>
std::function<T2(T1)> memoize(std::function<T2(T1)> f){
    // memoized version makes this and saves it
    // the lambda it returns captures
    std::unordered_map<T1, T2> memo;
    return [f, memo](T1 x) mutable { // mutable means captures can be changed
        if (memo.find(x) == memo.end()){
            // to see if it rlly works
            std::cout << "adding " << x << " to memo!\n"; 
            memo[x] = f(x);
        }
        return memo[x];
    };
};

// int --> float
std::function<float(int)> TimesTwo = [](int x){return x*2.0f;};

int main()
{

    std::function<float(int)> memTimesTwo = memoize<int,float>(TimesTwo);
    std::cout<< memTimesTwo(27) << std::endl;
    std::cout<< memTimesTwo(27) << std::endl;
    std::cout<< memTimesTwo(27) << std::endl;
    return 0;
    
    /*
adding 27 to memo!
54
54
54

    */
}


// how many functions are there from bool -> bool? 
// 2!
// identity and inverse identity

bool id (bool b){return b;}
bool nId (bool b){return !b;}