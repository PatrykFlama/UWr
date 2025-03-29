#include <bits/stdc++.h>
using namespace std;

#define cerr if(1) cout
#define fst first
#define snd second
typedef long long ll;
typedef pair<int, int> pii;


constexpr int N = 20;
constexpr int M = N << 1;
constexpr int MOD = 1e9+7;

vector<int> graph[N];
int dp[N][(1<<N)];  // dp[i][mask] = number of ways to reach node i with mask visited
int n, m;

void solve() {
    dp[0][1] = 1;

    for(int m = 1; m < (1 << n); m++) {
        for(int v = 0; v < n; v++) {
            if(!(m & (1 << v))) continue;  // if v is not in mask - unreachable

            for(int u : graph[v]) {
                if(m & (1 << u)) continue;  // if u is already visited

                int nm = m | (1 << u);
                dp[u][nm] = (dp[u][nm] + dp[v][m]) % MOD;
            }
        }
    }
}


int main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);

    cin >> n >> m;

    for (int i = 0; i < m; ++i) {
        int u, v;
        cin >> u >> v;
        graph[u-1].push_back(v-1);
    }

    solve();

    cout << dp[n-1][(1<<n)-1] << '\n';
}