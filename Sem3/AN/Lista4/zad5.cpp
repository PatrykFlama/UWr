#include <bits/stdc++.h>
using namespace std;
const double e = 1e-24;
double R = 4;
double avg_iter = 0;
int max_iter = 0;

double f(double x){
    return 1./x-R;
}

double F(double x){
    return x * (2. - x*R);
}

double calc_res(double x){
    double last =  std::numeric_limits<double>::infinity();
    int N = 1e6;
    int i = 0;

    while(N-- && abs(f(x)) > e && abs((last - x) / x) > e){
        last = x;
        x = F(x);
        i++;
    }

    if(N != 0) avg_iter = (avg_iter == 0) ? i : (avg_iter + i) / 2., max_iter = max(max_iter, i);
    return x;
}

int main(){
    cout << setprecision(15);
    double x0 = 1/(2*R);
    double x = calc_res(x0);
    cout << "starting from " << x0 << '\n';
    cout << "x = " << x << '\n';

    double a0 = 0+e, b0 = 1/R-e;
    for(double i = a0; i <= b0; i += 1e-3){
        calc_res(i);
    }

    cout << "avg_iter = " << avg_iter << '\n';
    cout << "max_iter = " << max_iter << '\n';
}
