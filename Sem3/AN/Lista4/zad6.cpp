#include <bits/stdc++.h>
using namespace std;
double a;
double e = 1e-7;
double avg = 0;


double func(double x){
    return 1./(x*x) - a;
}

double F(double x){
    return 0.5*(3*x - x*x*x*a);
}

double calc_res(double x){
    int N = 100000;
    double last =  std::numeric_limits<double>::infinity();
    int iterations = 0;

    while(N-- && abs(last - x) > e && abs(func(x)) > e){
        last = x;
        x = F(x);
        iterations++;
    }

    if(N != 0) avg = (avg == 0 ? iterations : (avg + iterations)/2);

    return x;
}

double bins(double l, double r, bool side, double error_multiplier = 1){        // one side has to be in convergent range
    while(r-l > e){
        double mid = (l+r)/2;
        double mid_res = calc_res(mid);

        if(side){           // we are looking for the rightmost root
            if(isnan(mid_res) || abs(mid_res - (1/sqrt(a))) > error_multiplier*e) r = mid;
            else l = mid;
        }
        else{
            if(isnan(mid_res) || abs(mid_res - (1/sqrt(a))) > error_multiplier*e) l = mid;
            else r = mid;
        }
    }

    return (l+r)/2;
}

int main(){
    cout << setprecision(10);

    a = 4;
    cout << "1/sqrt(a) = " << 1/sqrt(a) << '\n';

    cout << "for sure converges with x in (" << 1/(2*a) << ", " << 5/(2*a) << ")\n";
    cout << calc_res(0.7) << '\n';
    cout << calc_res(0.6) << '\n';
    cout << calc_res(0.5) << '\n';
    cout << calc_res(0.4) << '\n';
    cout << calc_res(0.3) << '\n';
    cout << calc_res(0.2) << '\n';
    cout << calc_res(0.1) << '\n';
    cout << calc_res(5) << '\n';

    cout << "\nbinsearched conv limits\n";
    double l = bins(-10, 0.5, false), r = bins(0.5, 10, true);
    cout << l << ' ' << calc_res(l+4*e) << ' ' << calc_res(l-4*e) << '\n';
    cout << r << ' ' << calc_res(r-4*e) << ' ' << calc_res(r+4*e) << '\n';

    for(double i = 1/(2*a); i < 5/(2*a); i += 0.01)
        calc_res(i);
    cout << "avg iteartions: " << avg << '\n'; 
}
