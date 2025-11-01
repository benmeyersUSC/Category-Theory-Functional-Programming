#include <functional>
using std::function;

// Product : pair
template<class A, class B>
using Product = std::pair<A, B>;

template<class A, class B>
struct Product{
    A fst;
    B snd;

    // projections
    A p(Product<A,B> c){
        return c.fst;
    }
    B q(Product<A,B> c){
        return c.snd;
    }

    // factorizer
    template<class C, class A, class B>
    function<Product<A,B>(C)> factorizer(function<A(Product<A,B>)> p, function<B(Product<A,B>)> q){
        return [p,q](C x){
            return Product<A,B>(p x, q x);
        }
    }
};



// Coproduct ~ tagged union of types
template<class A, class B>
struct Coproduct {
    A a;
    B b;
    bool isA;

    // injections
    Coproduct(A x){
        a = x;
        isA = true;
    }
    Coproduct(B x){
        b = x;
        isA = false;
    }

    // and this is the factorizer!
    template<class A, class B>
    function<Coproduct<A,B>(Coproduct<A,B>)> factorizerCo(function<Coproduct<A,B>(A)> i, function<Coproduct<A,B>(B)> j){
        return [i, j](Coproduct<A,B> e){
            if (e.isA){return Coproduct(e.a);}
            return Coproduct(e.b);
        };
    }
};



// another tagged union of types
struct Contact{
    enum {isPhone, isEmail} tag;
    union {int phoneNum; const char* emailAddr;};

    // injections...
    Contact PhoneNum(int n){
        Contact c;
        c.tag = isPhone;
        phoneNum = n;
        return c;
    }

    Contact EmailAddr(const char* s){
        Contact c;
        c.tag = isEmail;
        emailAddr = s;
        return c;
    }
    // these serve as constructors!
    // Haskell version is much nicer!

    // here is the Contact factorizer
    function<Contact(Coproduct<int,const char*>)> factorizer(function<Contact(int)> i, function<Contact(const char*)> j){
        // we need the [this] so we can use Contact stuff
        return [this, i,j](Coproduct<int,const char*> x){
            if (x.isA){return PhoneNum(x.a);}
            return EmailAddr(x.b);
        };
    }
};


// CHALLENGE 3

// show that Coproduct (either) is a better Coproduct than Int (with two injections)
int i(int n){return n;}                         // int -> int
int j(bool b){return b ? 0 : 1;}                // bool -> int   

/*
We need:

    I' = M . I
        I' n = n
    J' = M . J
        J' b = if b: 0 else: 1
        
        J just takes in the bool, so to replicate (to enboofify), we have to apply the superfluous transformation that J' does..
*/
int m(Coproduct<int,bool>& const e){
    if (e.isA){
        return e.a;
    }
    return e.b ? 0 : 1;
}       
/*
this will work for every possible bool input, hence Coproduct can be mapped uniformly to Int, hence Coproduct is better


but also...why could int not possibly be better than Coproduct/Either? lets see


We'd need:
    I' = M . I
    J' = M . J

as always. but now I' is Coproduct(int x) and J' is Coproduct(bool x)

so once again I' and I are doing the same thing. so M (int -> Cop) is fine

but J gives us an int too ! AHA !! AHA !!

our definition for M (bool -> Cop) would never possibly be called or used!!
J will give us an int (0 or 1)

AH...this is it: it doesn't even matter if we could recover information from a 0 or 1 to make a bool (we can't anyway)
    what matters is that we could define M bool as either = True or = False
    NOT A UNIQUE M TO GET FROM INT -> COPRODUCT/EITHER!
*/

Coproduct<int,bool> m1(int x) {
    if (x == 0 || x == 1) 
        return Coproduct<int,bool>(true);  // arbitrary choice and losing info if n = 0/1
    else 
        return Coproduct<int,bool>(x);
}

Coproduct<int,bool> m2(int x) {
    if (x == 0 || x == 1) 
        return Coproduct<int,bool>(false); // another arbitrary choice!
    else 
        return Coproduct<int,bool>(x);
}



/*CHALLENGE 5: what if we had:*/

int i2(int n){
    if (n < 0){return n;}
    return n + 2;
}
int j2(bool b){return b ? 0 : 1;}

// boom
int m2(Coproduct<int,bool>& const e){
    if (e.isA){
        if (e.a > 0){return e.a - 2;}
        else{return e.a;}
    }
    return e.b ? 0 : 1;
} 

// Coproduct is better
// same reasoning in the inverse. we could define the boolean case two ways

