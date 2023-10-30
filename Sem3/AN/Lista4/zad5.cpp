#include <bits/stdc++.h>
using namespace std;
#define float double
float R = 4;
int max_iter = 0;

float F(float x){
    return x * (2. - x*R);
}


float calc_res(float x){
    const float e = 1e-4;
    int N = 1e6;
    int i = 0;

    while(N-- && abs(1/R - x) > e){
        x = F(x);
        i++;
    }

    if(abs(1/R - x) <= e) max_iter = (max_iter == 0) ? i : (max_iter + i) / 2;

    return x;
}



int main(){
    float x0 = 0.5;
    float x = calc_res(x0);
    cout << "x = " << x << '\n';
    cout << "max_iter = " << max_iter << '\n';
}
