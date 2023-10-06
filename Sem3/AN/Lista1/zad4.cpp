#include <bits/stdc++.h>
using namespace std;
#define precision double

precision rec(int n, bool print_all = false){
    precision a = 1., b = -1./9.;
    while(n--){
        if(print_all) cout << a << '\n';
        precision temp = b;
        b = a + 80./9.*b;
        a = temp;
    }
    if(print_all) cout << a << endl;

    return a;
}

int main() {
    rec(50, true);
}
