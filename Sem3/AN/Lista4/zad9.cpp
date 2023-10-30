#include <bits/stdc++.h>
using namespace std;


double f(double x){
    return pow(x-2, 2) * sin(x);
}

double F(double x){
    return x - (x-2) * sin(x)/(2*sin(x) + (x-2)*cos(x));
}

double G(double x){
    return x - 2 * (x-2) * sin(x)/(2*sin(x) + (x-2)*cos(x));
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
}
