#include <bits/stdc++.h>
using namespace std;
#define precision double

precision rec(int n, bool print_all = false){
    precision a = 1, b = -1./9.;
    while(n--){
        cout << a << '\n';
        precision temp = b;
        b = a + 80/9*b;
        a = temp;
    }
    cout << a << endl;

    return a;
}

int main() {
    // rec(50, true);
    int exp = 0;
    int limit = 100;
    while(1.+pow(10, exp) != 1. && limit--){
        exp--;
    }
    cout << exp+1 << '\n' << 
         pow(10, exp+1) << ' ' << (double)(1.+pow(10, exp+1)) << '\n'  <<
         pow(10, exp)   << ' ' << 1.+pow(10, exp) << '\n';
}
