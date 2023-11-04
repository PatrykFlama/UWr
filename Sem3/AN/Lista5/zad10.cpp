#include <bits/stdc++.h>
using namespace std;

const double eR[4] = {0.763907023, 0.543852762, 0.196247370, 0.009220859};
const double eA[4] = {0.605426053, 0.055322784, 0.004819076, 0.000399783};

double rate_of_conv(double ep, double e, double en){
    return log(abs(en/e))/log(abs(e/ep));
}

double number_of_steps(double e0, double e1, double emax, double conv){
    int n = 1;
    double lep = log2(abs(e0));
    double le = log2(abs(e1));
    while(le > log2(abs(emax))){
        n++;
        double len = conv * (le - lep) + le;
        lep = le;
        le = len;
    }
    return n;
}

int main(){
    double rate_russia = rate_of_conv(eR[0], eR[1], eR[2]);
    double rate_america = rate_of_conv(eA[0], eA[1], eA[2]);

    cout << "conv:\n";
    cout << "russia: " << rate_russia << '\n';
    cout << "america: " << rate_america << '\n';

    const double emax = 1e-100;
    int steps_russia = number_of_steps(eR[0], eR[1], emax, rate_russia);
    int steps_america = number_of_steps(eA[0], eA[1], emax, rate_america);
    cout << "\nnumber of steps (one step = one week)\n";
    cout << steps_russia << '\n';
    cout << steps_america << '\n';
}

