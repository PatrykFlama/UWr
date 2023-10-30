#include <bits/stdc++.h>
using namespace std;
#define float double
float a;
float e = 1e-7;
float avg = 0;


float func(float x){
    return 1./(x*x) - a;
}

float F(float x){
    return 0.5*(3*x - x*x*a);
}

float calc_res(float x){
    int N = 10000;
    float last =  std::numeric_limits<float>::infinity();
    int iterations = 0;

    while(N-- && abs(last - x) > e && abs(func(x)) > e){
        last = x;
        x = F(x);
        iterations++;
    }

    if(abs(x-1./a) <= e) avg = (avg == 0 ? iterations : (avg + iterations)/2);

    return x;
}

float bins(float l, float r, bool side){
    while(abs(r-l) > e){
        float mid = (l+r)/2;
        if(side){
            if(abs(func(calc_res(mid))) > e) r = mid;
            else l = mid;
        }
        else{
            if(abs(func(calc_res(mid))) > e) l = mid;
            else r = mid;
        }
    }
    return (l+r)/2;
}

int main(){
    a = 4;
    cout << "for sure converges with x in " << 1/(2*a) << ' ' << 5/(2*a) << '\n';
    cout << calc_res(0.4) << '\n';
    cout << calc_res(10) << '\n';

    cout << "conv limits\n";
    cout << bins(-10, 0, false) << '\n';
    cout << bins(0, 10, true)  << '\n';

    for(float i = 1/(2*a); i < 5/(2*a); i += 0.01){
        calc_res(i);
    }
    cout << "avg iteartions: " << avg << '\n'; 
}
