#include <bits/stdc++.h>
using namespace std;

#define cerr if(1) cout
#define fst first
#define snd second
typedef long long ll;
typedef pair<int, int> pii;

#define minimize(a, b) a = min(a, b)

constexpr int MAXN = 500+2;
ll dist[MAXN][MAXN];


int main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);

    int n, m, q; cin >> n >> m >> q;

    for (int i = 1; i <= n; ++i) {
        for (int j = 1; j <= n; ++j) {
            dist[i][j] = LONG_MAX;
        }
        dist[i][i] = 0;
    }
    
    for (int i = 0; i < m; i++) {
        int u, v;
        ll c;
        cin >> u >> v >> c;
        minimize(dist[u][v], c);
        minimize(dist[v][u], c);
    }


    // calc F-W
    for (int k = 1; k <= n; k++) {
        for (int i = 1; i <= n; i++) {
            for (int j = 1; j <= n; j++) {
                if (dist[i][k] != LONG_MAX && dist[k][j] != LONG_MAX) {
                    minimize(dist[i][j], dist[i][k] + dist[k][j]);
                }
            }
        }
    }

    
    // ans q
    for (int i = 0; i < q; i++) {
        int u, v; cin >> u >> v;
        if (dist[u][v] == LONG_MAX) cout << -1 << '\n';
        else cout << dist[u][v] << '\n';
    }
}