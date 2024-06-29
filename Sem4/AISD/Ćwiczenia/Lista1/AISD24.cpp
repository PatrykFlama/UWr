/*
Patryk Flama
337382
KPO
*/
#include <bits/stdc++.h>
using namespace std;


int main(){
    int a, b;
    cin >> a >> b;

    while(a%2024) a++;

    while(a <= b){
        cout << a << ' ';
        a += 2024;
    }
}
