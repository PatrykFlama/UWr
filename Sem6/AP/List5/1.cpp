// SPARSE TABLE

#include <bits/stdc++.h>
#include <bit>
using namespace std;

#define cerr if(1) cout
#define fst first
#define snd second
typedef long long ll;
typedef pair<int, int> pii;
typedef pair<int, pii> piii;


constexpr int L = 2e5+5;
constexpr int K = 20;
int st[K][L];

int qlog2(unsigned long long i) {
    return i ? __builtin_clzll(1ll) - __builtin_clzll(i) : -1;
}


int main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);

    int n, q; cin >> n >> q;
    for(int i = 0; i < n; i++) {
        cin >> st[0][i];
    }

    // precompute
    for(int i = 1; i <= K; i++) {
        for(int j = 0; j + (1 << i) <= n; j++) {
            st[i][j] = min(st[i - 1][j], st[i - 1][j + (1 << (i - 1))]);
        }
    }

    // query
    while(q--) {
        int l, r; cin >> l >> r;
        l--, r--;
        int x = qlog2(r - l + 1);
        cout << min(st[x][l], st[x][r - (1 << x) + 1]) << '\n';
    }
}