/*
Sum type in Haskell:

data Shape = Circle Float | Rect Float Float



a function like area() acts on a Shape with pattern matching

area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rect d h) = d * h



Now let's implement this in C++

Shape as an interface, Circle and Rect as concretes, area() as a virtual function
*/
float PI = 3.1415926535;
class Shape{
    public:
    virtual float area() const = 0; 
    virtual float circ() const = 0; 
    virtual ~Shape() = default;    
};

class Circle : public Shape {
    float _radius;
public:
    explicit Circle(float r) : _radius(r) {}
    float radius() const { return _radius; }

    float area() const override { return PI * _radius * _radius; }
    float circ() const override { return 2 * PI * _radius; }
};

class Rect : public Shape {
    float _d, _h;
public:
    Rect(float d, float h) : _d(d), _h(h) {}
    float d() const { return _d; }
    float h() const { return _h; }

    float area() const override { return _d * _h; }
    float circ() const override { return 2 * (_d + _h); }
};

class Square : public Rect {
public:
    explicit Square(float s) : Rect(s, s) {}
};

/*
The differences in implementation between Haskell and C++ are once again very pronounced. 
At least as far as I know, it is easier in C++ to build these functions for each overload. 
That is, letting the discrimination be determined at the class/type level of the calling object
instead of a Shape being passed to a function that then has to check for the type of what was
passed in. I guess I could implement this in a more Haskell-like way in C++, but this is the way
we do polymorphism in OOP. Is polymorphic behavior like this just implemented in functional languages
as overloaded/pattern-matched mappings, mappings *of the type* that works on a whole tree of types? 
Do you have product types and functions on that type? Because that works. That really works. We have
done it here. And the paradigms want these alternatives their way; it is about fit. You have the exact
same data structure and interface in both, but the representations (the way you literally must think about
either alternative) are so different. You write the code in a different order. 


UHHH Classes are sums of products! Classes are sigma pi! Classes (like Shape, like in the real world) are
almost like CONCEPTS. And a neural network or binary circuit who has a unit which is a sum of products or
Disjunction of Conjunctions respectively is something which can define, with activations, a concept. 
*/


int main(){
    return 0;
}