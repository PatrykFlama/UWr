#include <bits/stdc++.h>
using namespace std;


float f(float x){
    return pow(x-4, 3);
}

float fp(float x){
    return 3*x*x-24*x+48;
}

float g(float x){
    return x-4;
}

float F(float x){
    return x - f(x)/fp(x);
}

float G(float x){
    return x - 3*f(x)/fp(x);
}


int main(){
    float x0;
    float xf = x0, xg = x0;
    int max = 10;

    for(int i = 0; i < max; i++){
        xf = F(xf);
        xg = G(xg);
    }
    cout << "F after " << max << " iterations: " << xf << '\n';
    cout << "G after " << max << " iterations: " << xg << '\n';
}
