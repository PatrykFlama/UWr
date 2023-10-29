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


bool test_for_conv(float x){
    int N = 1000;
    float last = std::numeric_limits<float>::infinity();

    // while(N-- && abs(x - last) > e && abs(f(x)) > e){
    while(N--){
        last = x;
        x = F(x);
    }

    return abs(abs(x) - sqrt(a)) < e;
}

float bins(bool opt = true){
    float l = -1e6, r = 1e6, mid = (l + r) / 2;
    while(r - l > e){
        if((opt ? test_for_conv(mid) : !test_for_conv(mid))) r = mid;
        else l = mid;
        mid = (l + r) / 2;
    }
    return mid;
}


int main(){
    a = 0.6;
    float limitR = bins(), limitL = bins(false);
    cout << limitR << ' ' << limitL << '\n';
    // cout << test_for_conv(1/2.+1/8.) << '\n';
    // cout << test_for_conv(-2) << '\n';
}
