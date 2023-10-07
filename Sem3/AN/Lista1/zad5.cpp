#include <bits/stdc++.h>
using namespace std;
#define precision double


precision In(int to, bool print = false, int step = 1, int from = 1){
    precision res = log(2024./2023.);
    int print_step = -from+1;

    for(int i = 1; i <= to; i++){
        res = 1./(precision)i - 2023*res;
        if(print && !(print_step++%step)) cout << i << ": " << res << '\n';
    }

    return res;
}


int main(){
    In(20, true);
    cout << '\n';
    In(20, true, 2);
    cout << '\n';
    In(20, true, 2, 2);
}
