#include <bits/stdc++.h>
using namespace std;

#define cerr if(1) cout
#define fst first
#define snd second
typedef long long ll;
typedef pair<int, int> pii;
typedef pair<int, ll> pill;
typedef pair<ll, int> plli;
typedef pair<ll, ll> pll;

constexpr int MAXN = 2500+2;
constexpr int MAXM = 5000+2;
pair<ll, pii> edges[MAXM];
int parent[MAXN];
ll dist[MAXN];


int main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);

    int n, m; cin >> n >> m;

    for (int i = 0; i < m; i++) {
        int u, v;
        long long w;
        cin >> u >> v >> w;
        edges[i] = {-w, {u, v}};
    }

    for (int i = 0; i <= n; i++) {
        dist[i] = LLONG_MAX;
        parent[i] = -1;
    }
    dist[1] = 0;

    int x = -1;
    for (int i = 0; i <= n; i++) {
        x = -1;
        for (int j = 0; j < m; j++) {
            auto [w, e] = edges[j];
            auto [u, v] = e;
            if (dist[u] != LLONG_MAX && dist[v] > dist[u] + w) {
                dist[v] = dist[u] + w;
                parent[v] = u;
                x = v;
            }
        }
    }

    if (x == -1) {
        // during last run no edge was relaxed
        // no negtive (positive) cycle - we can obtain the best distance with simple path
        cout << -dist[n] << "\n";
    } else {    // there exists a negative cycle (which in original graph is a positive cycle)
        cout << "-1\n";
    }
}
