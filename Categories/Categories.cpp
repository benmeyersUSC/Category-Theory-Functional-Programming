#include <string>

template<class T>
T mempty;

template<class T>
T mappend(T, T) = delete;
// deletes mean no default value in this concept


//------------------------------------------------------------
// template<class M>
// concept bool Monoid = requires (M m){
//     {mempty<M>} -> M;
//     {mappend(m, m);} -> M;
// };
// /*
// The concept Monoid is a predicate that tells whethere there are
// appropriate definitions for memtpy and mappend in a type M
// */ 
//------------------------------------------------------------


// this Monoid is true
template<>
std::string mempty<std::string> = {""};
std::string mappend(std::string s1, std::string s2){
    return s1 + s2;
}


// need wrappers to do both monoids

struct AndMonoid { bool value; };
struct OrMonoid { bool value; };

// && monoid
template<>
bool mempty<AndMonoid> = true;
bool mappend(AndMonoid a, AndMonoid b){
    return a.value && b.value;
}

// || monoid
template<>
bool mempty<OrMonoid> = false;
bool mappend(OrMonoid a, OrMonoid b){
    return a.value || b.value;
}

int main()
{
    int x = 2;
    return 0;
}