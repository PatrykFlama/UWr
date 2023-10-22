#include <bits/stdc++.h>
using namespace std;
# define M_PI 3.14159265358979323846
#define float double


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
    return pow(x, -3) * (M_PI/2. - x - atan(x));
}

float c_better(float x){
    return pow(x, -3) * (M_PI/2. - 2.*x - pow(x, 3)/3. + pow(x, 5)/5.);
}



int main(){
    cout << "a) " << a(-10000.) << "\n";
    cout << "a_better) " << a_better(-10000.) << "\n\n";

    cout << "b) " << b(4.001) << "\n";
    cout << "b_better) " << b_better(4.001) << "\n\n";

    cout << "c) " << c(0.86033358901938) << "\n";
    cout << "c_better) " << c_better(0.86033358901938) << "\n\n";
}