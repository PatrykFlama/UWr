#include <bits/stdc++.h>
using namespace std;
#define ll long long

int quick_pow_mod(ll a, ll b, ll m) {
    if(b == 0) return 1;        // not mandatory
    if(b == 1) return a;

    const ll qpm = quick_pow_mod(a, b/2, m);
    if(b%2) return a*(qpm*qpm % m) % m;
    return qpm*qpm % m;
}

int main(){
    ios_base::sync_with_stdio(false);
    cin.tie(0);

    int t; cin >> t;
    while(t--) {
        int a, b, m;
        cin >> a >> b >> m;
        cout << quick_pow_mod(a, b, m) << '\n';
    }
}