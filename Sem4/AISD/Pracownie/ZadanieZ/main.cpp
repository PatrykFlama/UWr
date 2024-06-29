#include <bits/stdc++.h>
using namespace std;


int main(){
    ios_base::sync_with_stdio(false);cin.tie(0);
    int a, b;
    cin >> a >> b;

    if (b < a) swap(a, b);

    while(a <= b){
        cout << a << '\n';
        ++a;
    }
}
