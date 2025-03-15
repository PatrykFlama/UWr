#include <bits/stdc++.h>
using namespace std;

#define cerr if(0) cout
typedef long long ll;

constexpr int L = 2e5+5;
int tab[L];


int main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);

    // read table
    int n; cin >> n;
    for(int i = 0; i < n; i++) {
        cin >> tab[i];
    }

    // preprocess
    // precalc results only for b < sqrtn that gives us O(n*sqrt(n)) complexity
    int sqrtn = ceil(sqrt(n));
    ll precalc[n+3][sqrtn+3];

    for(int a = n-1; a >= 0; a--) {
        for(int b = 1; b <= sqrtn; b++) {
            precalc[a][b] = tab[a] + (a+b < n ? precalc[a + b][b] : 0);
            cerr << a << ", " << b << " = " << precalc[a][b] << " | " << tab[a] << " + " << (a+b < n ? precalc[a + b][b] : 0) << "\n";
        }
    }

    for(int i =0; i < n; i++){for(int j = 0; j < sqrtn; j++) cerr << precalc[i][j] << " "; cerr << "\n";}

    // answer queries
    int q; cin >> q;
    while(q--) {
        int a, b; cin >> a >> b;
        a--;

        if(b <= sqrtn) {
            cout << precalc[a][b] << "\n";
        } else {
            ll res = 0;
            while(a < n) {
                res += tab[a];
                a += b;
            }
            cout << res << "\n";
        }
    }
}