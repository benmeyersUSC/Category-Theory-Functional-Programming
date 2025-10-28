
#include <iostream>
#include <functional>

// identity function
template<typename T> 
std::function<T(T)> id = [](T x){return x;};

// composition function
template<typename T1, typename T2, typename T3>
std::function<T3(T1)> composition(std::function<T2(T1)> ott, std::function<T3(T2)> ttt)
{
    return [ttt, ott](T1 x){return ttt(ott(x));};
}

// bool --> int
std::function<int(bool)> AddOne = [](bool x){return x+1;};
// int --> float
std::function<float(int)> TimesTwo = [](int x){return x*2.0f;};

int main()
{
    // bool --> float
    std::function<float(bool)> BoolToFloat = composition(AddOne, TimesTwo);

    // testing id
    int z = 927;
    std::cout << id<int>(z) << std::endl;
    std::cout << BoolToFloat(true) << std::endl;
    // this all runs. now let's just see if the below compiles...
    
    std::function<float(bool)> BoolToFloatId = composition(BoolToFloat, id<float>);
    std::function<float(bool)> IdBoolToFloat = composition(id<bool>, BoolToFloat);
    // great
    return 0;
}