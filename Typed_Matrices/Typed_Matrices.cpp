#include <utility>
#include <iostream>
template<int R, int C>
class Matrix {
private:
    float data[R][C];
    
public:
    constexpr std::pair<int, int> shape() const { 
        return {R, C}; 
    }
    constexpr float Data(int r, int c)const{return data[r][c];}
    constexpr float& Data(int r, int c){return data[r][c];}
    constexpr int Rows() const { return R; }
    constexpr int Cols() const { return C; }


    template<int M>
    Matrix<R,M> operator*(Matrix<C,M> other){
        Matrix<R,M> result{};
        // for each row in this
        for (int i = 0; i < R; i++){
            // for each column in other
            for (int j = 0; j < M; j++){
                // for this.col/other.row
                for (int k = 0; k < C; k++){
                    // this           = this[row][col] * other[otherRow][otherCol]
                    result.Data(i,j) += data[i][k] * other.Data(k,j);
                }
            }
        }
        return result;
    }

    Matrix<R,C> operator*(float scalar){
        Matrix<R,C> result{};
        for (int i = 0; i < R; i++){
            for (int j = 0; j < C; j++){
                result.Data(i, j) = data[i][j] * scalar;
            }
        }
        return result;
    }

    Matrix<R,C> operator/(float scalar){
        return this * (1.0f/scalar);
    }
    
    Matrix<R,C> operator+(Matrix<R,C> other){
        Matrix<R,C> result{};
        for (int i = 0; i < R; i++){
            for (int j = 0; j < C; j++){
                result.Data(i, j) = data[i][j] + other.Data(i, j);
            }
        }
        return result;
    }

    Matrix<R,C> operator-(Matrix<R,C> other){
        Matrix<R,C> result{};
        for (int i = 0; i < R; i++){
            for (int j = 0; j < C; j++){
                result.Data(i, j) = data[i][j] - other.Data(i, j);
            }
        }
        return result;
    }

    Matrix<R,C> operator+(float bias){
        Matrix<R,C> result{};
        for (int i = 0; i < R; i++){
            for (int j = 0; j < C; j++){
                result.Data(i, j) = data[i][j] + bias;
            }
        }
        return result;
    }
    
    Matrix<R,C> operator-(float bias){
        return this + (-1.0f * bias);
    }
};



template<int A>
struct Num {static constexpr int value = A;};
template<int C, int D>
   Num<C*D> operator*(Num<C> a, Num<D> b){
    return Num<C*D>{};
}

int main() {
    Matrix<16, 32> A;
    Matrix<32, 9> B;

    Matrix<16,9> C = A * B;
    Matrix<16,9> D = C + C;
    

    auto x = Num<9>{} * Num<3>{};
    std::cout << decltype(x)::value << '\n';  


    return 0;
}