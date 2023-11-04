#include <bits/stdc++.h>
using namespace std;
int func = 0;


double X0[] = {0, 1, 1.809, 2.237, 0};
double XP[] = {1, 1.5, 1.5, -1.5, 1};
double f(double x){
    if(func == 0) return x*x;
    if(func == 1) return x*x*x - 2*x*x + 4;
    if(func == 2) return x*x*x*x - 4*x*x*x + 5*x*x - 2.5*x + 1.131;
    if(func == 3) return x*x*x*x*x - 5*x*x*x*x + 9*x*x*x - 7*x*x + 2*x - 1;
    if(func == 4) return sin(x)+x;
    return 0;
}
double fp(double x){
    if(func == 0) return 2*x;
    if(func == 1) return 3*x*x - 4*x;
    if(func == 2) return 4*x*x*x - 12*x*x + 10*x - 2;
    if(func == 3) return 5*x*x*x*x - 20*x*x*x + 27*x*x - 14*x + 2;
    if(func == 4) return cos(x)+1;
    return 0;
}
double fb(double x){
    if(func == 0) return 2;
    if(func == 1) return 6*x - 4;
    if(func == 2) return 12*x*x - 24*x + 10;
    if(func == 3) return 20*x*x*x - 60*x*x + 54*x - 14;
    if(func == 4) return -sin(x);
    return 0;
}

double F(double x){
    return x - 1./2. * fb(x)/fp(x) * pow(f(x)/fp(x), 2);
}


double rate_of_conv(double x0, double xp, double x, double xn){
    return log(abs((xn-x0)/(x-x0)))/log(abs((x-x0)/(xp-x0)));
}


int main(){
    cout << setprecision(10) << fixed;

    cout << "dla x0\t\tdla xn\t\twynik xn\n";
    for(func = 0; func < 5; func++){
        const double x0 = X0[func];
        double xp = XP[func];
        double x = F(xp);
        double xn = F(x);
        cout << rate_of_conv(x0, xp, x, xn) << '\t';

        for(int i = 0; i < 100 && !isnan(rate_of_conv(x0, F(xp), F(x), F(xn))); i++){
            xp = x;
            x = xn;
            xn = F(x);
        }
        cout << rate_of_conv(x0, xp, x, xn) << '\t' << xn << '\n';
    }
}

