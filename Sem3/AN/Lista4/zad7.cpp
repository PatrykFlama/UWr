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

    while(N-- && abs(last - x) > e && abs(f(x)) > e){
        last = x;
        x = F(x);
    }

    return x;
}


int main(){
    a = 2;
    cout << setprecision(10) << fixed;
    cout << calc_res(100) << '\n';
}
