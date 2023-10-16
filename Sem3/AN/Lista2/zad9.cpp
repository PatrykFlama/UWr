#include <bits/stdc++.h>
using namespace std;
#define float double


float f1(int n){
    float res = 2;      // 1st
    for(int k = 1; k < n; k++){
        float exp = pow(2, k);
        res = exp*sqrt(2*(1-sqrt(1-pow(res/exp, 2))));
    }
    return res;
}

float f2(int n){
    float res = 2;      // 1st
    for(int k = 1; k < n; k++){
        res = sqrt(2*res*res/(1+sqrt(1-pow(res/pow(2, k), 2))));
    }
    return res;
}


int main(){
    cout << setprecision(20);
    for(int n = 20; n < 30; n++){
        cout << "n = " << n << '\n';
        cout << "f1(n) = " << f1(n) << '\n';
        cout << "f2(n) = " << f2(n) << "\n\n";
    }
}