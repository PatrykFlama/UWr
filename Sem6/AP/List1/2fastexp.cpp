#include <bits/stdc++.h>
using namespace std;

typedef long long ll;

constexpr int MOD = 1e9;


int main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);

    ll A;
    ll N;
    cin >> A >> N;
    
    ll res = 1;
    while(N > 0) {
        if(N & 1)
            res = (res * A) % MOD;
        A = (A * A) % MOD;
        N >>= 1;
    }

    cout << res << '\n';
}