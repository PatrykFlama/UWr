#include <bits/stdc++.h>
using namespace std;

#define cerr if(0) cout
#define fst first
#define snd second
typedef long long ll;
typedef pair<int, int> pii;

// dp on bitmasks - mask x declares taken positions, that gives ~1e6 states
constexpr int N = 20;

int tab[N][N];
int dp[1<<N];

int solve(int n) {
    for(int i = 0; i < n; i++) {
        int best = 0;
        // for all worker-masks try to fit worker w here, unless he's already taken
        for(int w = 0; w < n; w++) {
            for(int m = (1 << n) - 1; m >= 0; m--) {
                if(m & (1<<w) || __builtin_popcount(m) != i) continue; // already taken or not enough / too many workers

                const int nm = m | (1<<w); // add worker w to the mask
                dp[nm] = max(dp[nm], dp[m] + tab[w][i]);
                best = max(best, dp[nm]);
            }
        }
        cerr << "best for task " << i << ": " << best << '\n';
    }

    return dp[(1 << n) - 1];
}

int main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);

    int n; cin >> n;

    for(int i = 0; i < n; i++) {    // task
        for(int j = 0; j < n; j++) {    // worker
            cin >> tab[j][i];
        }
    }

    int res = solve(n);

    cout << res << '\n';
}