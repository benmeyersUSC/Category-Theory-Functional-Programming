#include <utility>

template<int R, int C>
class Matrix {
private:
    float data[R][C];
    
public:
    constexpr std::pair<int, int> shape() const { 
        return {R, C}; 
    }
    
    constexpr int rows() const { return R; }
    constexpr int cols() const { return C; }

    template<int R1, int RC, int C2>
    Matrix<R1,C2> operator*(Matrix<RC,C2> other){
        
    }
};



int main() {
    Matrix<16, 32> mat;
    return 0;
}