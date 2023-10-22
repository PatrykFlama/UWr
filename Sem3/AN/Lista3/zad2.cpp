#include <bits/stdc++.h>
using namespace std;
#define float double


void find_zeros(float a, float b, float c){
    float delta = b*b - 4*a*c;
    if (delta < 0){
        cout << "imaginary zeros\n";
    }
    else if (delta == 0){
        cout << -b / (2*a) << '\n';
    }
    else{
        cout << (-b - sqrt(delta)) / (2*a) << ' '
             << (-b + sqrt(delta)) / (2*a) << '\n';
    }
}

void find_zeros_better(float a, float b, float c){
    float scale = 1000;
    if(b*b < scale*4*a*c){
        find_zeros(a, b, c);
        return;
    }

    float delta = b*b - 4*a*c;
    if (delta < 0){
        cout << "imaginary zeros\n";
    }
    else if (delta == 0){
        cout << -b / (2*a) << '\n';
    }
    else{
        if(b > 0){
            cout << (-b - sqrt(delta)) / (2*a) << ' '
                 << (2*c)/(-b-sqrt(b*b-4*a*c)) << '\n';
        }
        else{
            cout << (2*c)/(-b+sqrt(b*b-4*a*c)) / (2*a) << ' '
                 << (-b + sqrt(delta)) / (2*a) << '\n';
        }
    }
}


int main(){
    find_zeros(0.01, 100000000, 2);
    find_zeros_better(0.01, 100000000, 2);

    cout << '\n';
    find_zeros(0.01, -200000000, 2);
    find_zeros_better(0.01, -200000000, 2);
}
