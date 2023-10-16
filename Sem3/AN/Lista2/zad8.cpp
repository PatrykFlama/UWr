#include <bits/stdc++.h>
using namespace std;


float f(float x, int n){
    float res = 0;
    float fact = 1;
    for(int i = 0; i < n; i++){
        if(i%2) res += pow(x, 2*i)/fact;
        else res -= pow(x, 2*i)/fact;
        fact *= (i+1);
    }
    return res;
}


int main(){
    double x = 0.001;
    for(int i = 0; i < 10; i++){
        cout << "f(pow(10, -11)) = " << f(pow(10, -11), i) << endl;
    }
}