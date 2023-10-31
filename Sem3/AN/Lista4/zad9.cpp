#include <bits/stdc++.h>
using namespace std;
double a = 4;


double f(double x){
    return pow(x-2, 2) * sin(x);
}

double F(double x){
    return x - (x-2) * sin(x)/(2*sin(x) + (x-2)*cos(x));
}

double G(double x){
    return x - 2 * (x-2) * sin(x)/(2*sin(x) + (x-2)*cos(x));
}

double f2(double x){
    return pow(x-2, 2) * log(x);
}

double F2(double x){
    return (2-x)/5. + x;
}

double G2(double x){
    return x - 5*(pow(x-2, 5))/(pow(5*(x-2), 4));
}


int main(){
    cout << setprecision(15);
    double x0 = 1.5;
    double xf = x0, xg = x0;
    int max = 24;

    for(int i = 0; i < max; i++){
        xf = F(xf);
        xg = G(xg);
    }
    cout << "F after " << max << " iterations: " << xf << '\n';
    cout << "G after " << max << " iterations: " << xg << '\n';


    x0 = 1.5;
    xf = x0, xg = x0;
    max = 100;

    for(int i = 0; i < max; i++){
        xf = F2(xf);
        xg = G2(xg);
    }
    cout << "F after " << max << " iterations: " << xf << '\n';
    cout << "G after " << max << " iterations: " << xg << '\n';
}
