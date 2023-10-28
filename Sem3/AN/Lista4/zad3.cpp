#include <bits/stdc++.h>
using namespace std;

float f(float x){
    return x - 0.49;
}

int main(){
    float a0 = 0, b0 = 1;
    float a = a0, b = b0;

    for(int n = 0; n <= 5; n++){
        float m = (a + b) / 2.;
        if(f(m) > 0)
            b = m;
        else
            a = m;

        cout << abs(0.49 - m) << '\t' << (b0-a0)/pow(2, n+1) << '\t' <<
                (b0-a0)/pow(2, n+1) - abs(0.49 - m) << '\n';
    }
}
