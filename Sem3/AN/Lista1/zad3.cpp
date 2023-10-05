#include <bits/stdc++.h>
using namespace std;


float f_single(float x, bool print_out = false){
    if(print_out){
        cout << "\n14*(1-cos("<<17*x<<"))" << '\n';
        cout << "14*(1-"<<cos(17*x)<<")" << '\n';
    }
    return 14*(1-(float)cos(17*x))/(x*x);
}

double f_double(double x, bool print_out = false){
    if(print_out){
        cout << "\n14*(1-cos("<<17*x<<"))" << '\n';
        cout << "14*(1-"<<cos(17*x)<<")" << '\n';
    }
    return 14*(1-cos(17*x))/(x*x);
}

int main(){
    for(int i = 11; i <= 20; i++){
        double r = pow(10, -i);
        cout << r << "\n";
        cout << "single: " << f_single(r) << '\n';
        cout << "double: " << f_double(r, true) << '\n';
    }

    cout << "dla malego x, nawet przemnozonego przez 17, wynik cos bedzie zbyt bliski 1, nawet dla podwojnej precyzji\n";
}
