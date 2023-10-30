#include <bits/stdc++.h>
using namespace std;
#define float double
const float e = 1e-6;
float a;

float f(float x){
    return x*x - a;
}

float F(float x){
    return x/2.+a/(2*x);
}

float calc_res(float x){
    int N = 1000;
    float last =  std::numeric_limits<float>::infinity();;

    while(N-- && abs((last - x) / x) > e && abs(f(x)) > e){
        last = x;
        x = F(x);
    }

    return abs(x);
}


int main(){
    cout << setprecision(10) << fixed;

    float x0 = 1000;
    float num = 10;
    

    a = num;
    int exponent = 0;
    while(a >= 1){
        a /= 2;
        exponent++;
    }
    while(a < 0.5){
        a *= 2;
        exponent--;
    }
    // cout << a << " * 2^" << exponent << '\n';

    if (exponent%2 == 0)
        cout << calc_res(x0) * pow(2, exponent/2) << '\n';
    else
        cout << calc_res(x0*2) * pow(2, (exponent-1)/2) << '\n';


    a = num;
    cout << calc_res(x0) << '\n';
}
