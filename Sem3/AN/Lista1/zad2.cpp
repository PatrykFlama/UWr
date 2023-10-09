#include <bits/stdc++.h>
using namespace std;

double f(double x){
    return 4046*(sqrt(pow(x, 14)+1)-1)/pow(x, 14);
}

int main(){
    double x = 0.001;
    cout << "f(0.001) = " << f(x) << endl;
}
