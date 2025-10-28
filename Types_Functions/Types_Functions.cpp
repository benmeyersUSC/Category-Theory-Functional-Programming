#include <iostream>
#include <functional>   // lambdas
#include <unordered_map>

// pass a function and input and print application
template<typename T1, typename T2>
void PrintFunction(std::function<T2(T1)> f, T1 x){
    std::cout << "Function(" << x << ") = " << f(x) << std::endl;
}

// memoize challenge: 
// - function that takes in pure function f 
// - returns function that behaves like f but only calls f once per arg
// - it internally stores returns for each input, then uses that as memo
template<typename T1, typename T2>
std::function<T2(T1)> memoize(std::function<T2(T1)> f){
    // memoized version makes this and saves it
    // the lambda it returns captures
    
    std::unordered_map<T1, T2>* memo = new std::unordered_map<T1, T2>();
    return [f, memo](T1 x) mutable { // mutable means captures can be changed
        if (memo->find(x) == memo->end()){
            // to see if it's working!
            std::cout << "[adding " << x << " to the memo]";
            memo->operator[](x) = f(x);
        }
        return memo->operator[](x);
    };
};

// int -> float
std::function<float(int)> TimesTwo = [](int x){return x*2.0f;};


// how many functions are there from bool -> bool? 
// 2!
// identity and inverse identity

bool id (bool b){return b;}
bool nId (bool b){return !b;}

int main()
{

    std::function<float(int)> memTimesTwo = memoize<int,float>(TimesTwo);
    PrintFunction(memTimesTwo, 27);
    PrintFunction(memTimesTwo, 27);

    // int -> int
    std::function<int(int)> Factorial;
    Factorial = memoize<int, int>(
        [&](int x){
            if (x <= 1){
                return 1;
            }
            return Factorial(x-1) * x;
        }
    );    


    PrintFunction(Factorial, 27);
    PrintFunction(Factorial, 27);
    PrintFunction(Factorial, 30);
    PrintFunction(Factorial, 9);
    PrintFunction(Factorial, 27);
    PrintFunction(Factorial, 30);
    PrintFunction(Factorial, 9);
    
    return 0;

}


