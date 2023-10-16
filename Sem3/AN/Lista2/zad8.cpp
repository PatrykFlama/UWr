#include <bits/stdc++.h>
using namespace std;
#define float double


float f(float x){
    return 14*((17*17)/2.-(pow(17, 4)*pow(x, 2)/24.));
}


int main(){
    for(int i = 0; i < 10; i++){
        cout << "f(pow(10, " << -11-i << ")) = " << f(pow(10, -11-i)) << endl;
    }
}