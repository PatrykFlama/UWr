#include <bits/stdc++.h>
using namespace std;

typedef long long ll;
typedef pair<int, int> pii;
typedef pair<ll, ll> pll;

constexpr int L = 1e4+5;
ll A[L], P[L];


// T=O(log n)
pll ext_eucl(ll a, ll b) {
    if (b == 0) return {1, 0};
    pll p = ext_eucl(b, a % b);
    return {p.second, p.first - a / b * p.second};
}

// T=O(log n)
ll mod_inv(ll a, ll m) {
    pll p = ext_eucl(a, m);
    return (p.first % m + m) % m;
}


int main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);

    int t; cin >> t;
    while(t--) {
        int k; cin >> k;

        ll MOD = 1;
        for(int i = 0; i < k; i++) {
            ll ai, pi; cin >> pi >> ai;
            A[i] = ai;
            P[i] = pi;
            MOD *= pi;
        }

        ll res = 0;
        for(int i = 0; i < k; i++) {
            const ll Pi = MOD / P[i];
            const ll Pi_inv = mod_inv(Pi, P[i]);
            res = (res + A[i] * Pi % MOD * Pi_inv) % MOD;
        }
        cout << res << '\n';
    }
}