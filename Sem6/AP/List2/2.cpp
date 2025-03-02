#include <bits/stdc++.h>
using namespace std;

typedef long long ll;
constexpr int MOD = 1e9+7;
constexpr int L = 1e6+2;
ll fact[L];
ll inv_fact[L];

/*
(p-2)! = 1 mod p
1/k! = (p-2)!/k! = (p-2)(p-3)...(k+1)
n!/(k!(n-k)!) = n(n-1)...(n-k+1) (p-2)(p-3)...(k+1)
*/

ll fast_pow(ll x, ll y) {
    ll res = 1;
    while(y > 0) {
        if(y % 2) res = (res * x) % MOD;
        x = (x * x) % MOD;
        y /= 2;
    }
    return res;
}

void precalc() {
    fact[0] = 1;
    for(int i = 1; i < L; i++) {
        fact[i] = (fact[i-1] * i) % MOD;
    }

    inv_fact[L-1] = fast_pow(fact[L-1], MOD - 2);
    for(int i = L - 2; i >= 0; i--) {
        inv_fact[i] = (inv_fact[i + 1] * (i + 1)) % MOD;
    }
}

ll binom(ll a, ll b) {
    if(b > a || b < 0) return 0;
    return (((fact[a] * inv_fact[b]) % MOD) * inv_fact[a - b]) % MOD;
}

int main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);

    // precalc fact
    precalc();
    

    int t; cin >> t;
    while(t--) {
        ll a, b; cin >> a >> b;
        cout << binom(a, b) << '\n';
    }
}