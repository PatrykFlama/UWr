#include <bits/stdc++.h>
using namespace std;
const double e = 2e-8;
double a;

double f(double x){
    return x*x - a;
}

double F(double x){
    return x/2.+a/(2*x);
}

double calc_res(double x){
    int N = 1000;
    double last =  std::numeric_limits<double>::infinity();;

    while(N-- && abs((last - x) / x) > e && abs(f(x)) > e){
        last = x;
        x = F(x);
    }

    return abs(x);
}

double supercalc_res(double x, double num){
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

    double res;
    if (exponent%2 == 0)
        res = calc_res(x) * pow(2, exponent/2);
    else
        res = calc_res(x*2) * pow(2, (exponent-1)/2);
    a = num;
    return res;
}


int main(){
    cout << setprecision(10) << fixed;

    double x0 = 1000;
    double num = 10;
    

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


    // check only for m, which is in (1/2, 2)
    for(double x = 1./2. + 1e24; x < 2; x += 2e-24){
        if(abs(supercalc_res(x0, a) - sqrt(num)) < e) cout << x << '\n';
    }


    // x0 = 0 + 1e-127;
    // while(true){
    //     if(abs(supercalc_res(x0, a) - sqrt(num)) < 64*e) break;
    //     x0 *= 2;
    // }
    // cout << x0 << '\n';

    // x0 = numeric_limits<double>::max();
    // while(true){
    //     if(abs(supercalc_res(x0, a) - sqrt(num)) < 64*e) break;
    //     x0 /= 2;
    // }
    // cout << numeric_limits<double>::max() - x0 << '\n';
}
