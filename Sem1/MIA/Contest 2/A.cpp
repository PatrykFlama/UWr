#include <bits/stdc++.h>
using namespace std;


int main(){
    int n, v; cin >> n >> v;
    int fuel = 0, price = 0;
    for(int i = 1; i <= n; i++){
        int add = max(0, min(v-fuel, n-i-fuel));
        price += i*add;
        // cout << v-fuel << ' ' << n-i-fuel << ' ' << add << '\n';
        fuel += add-1;
    }

    cout << price << '\n';
}