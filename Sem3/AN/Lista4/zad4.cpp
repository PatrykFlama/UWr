#include <bits/stdc++.h>
using namespace std;
#define float double
float a0, b0;
const float max_error = 1e-8;
#define min_iter ceil(log2((b0 - a0)/max_error))


float f(float x){
    return pow(x, 4) - log(x + 4);
}


float find_zero(){
    float a = a0, b = b0, m = (a0 + b0)/2;

    for(int i = 0; i < min_iter; i++){
        if(f(m) == 0) return m;
        if(f(a)*f(m) < 0) b = m;
        else a = m;
        m = (a + b)/2;
    }

    return m;
}


int main(){
    // wolfram:
    // -1.02206624|04863400034
    //  1.13082966|56007111722
    //  0.123456 8  0123456789 
    cout << setprecision(10) << fixed;

    a0 = -2, b0 = 0;
    cout << find_zero() << '\n';

    a0 = 0, b0 = 2;
    cout << find_zero() << '\n';
}