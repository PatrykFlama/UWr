#include <bits/stdc++.h>
using namespace std;
#define PI 3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679
// #define float double


float actg(float x){
    return atan(1./x);
}

float acc(float x, float y){
    return -log10(abs(1-x/y));
}


float a(float x){
    return pow(pow(x, 3) + sqrt(pow(x, 6) + 2023.*2023.), -1);
}

float a_better(float x){
    return -(pow(x, 3) + sqrt(pow(x, 6) + 2023.*2023.)) / 2023.*2023.;
}


float b(float x){
    return log2(x) - 2.;
}

float b_better(float x){
    return log2(x/4.);
}


float c(float x){
    return pow(x, -3) * (PI/2. - x - actg(x));
}

float c_better(float x){
    // return pow(x, 6)/9. - pow(x, 4)/7. + pow(x, 2)/5. -1./3.;
    // if(x >= 0) return -1./(x*x) + atan(x)/pow(x, 3);
    // else return -1./(x*x) + atan(x)/pow(x, 3);
    return atan(x)/pow(x, 3) - 1/(x*x);
}



int main(){
    float A = -10000.;
    cout << "a) " << a(A) << "\n";
    cout << "a_better) " << a_better(A) << "\n";
    cout << "difference: " << acc(a(A), a_better(A)) << "\n\n";

    float B = 4.000001;
    cout << "b) " << b(B) << "\n";
    cout << "b_better) " << b_better(B) << "\n";
    cout << "difference: " << acc(b(B), b_better(B)) << "\n\n";

    float C = 0.000000000000001;
    // float C = M_PI/2.;
    cout << "c) " << c(C) << "\n";
    cout << "c_better) " << c_better(C) << "\n";
    cout << "difference: " << acc(c(C), c_better(C)) << "\n\n";
}