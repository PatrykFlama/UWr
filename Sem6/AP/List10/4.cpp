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

constexpr int MAXN = 2500;
vector<pii> adj[MAXN];
vector<int> dist(MAXN, INT_MAX);
vector<int> parent(MAXN, -1);


int main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);

    int n, m; cin >> n >> m;
    for (int i = 0; i < m; ++i) {
        int u, v, c; cin >> u >> v >> c;
        adj[u].push_back({v, c});
    }

    // bellman-ford
    dist[1] = 0;
    for (int i = 1; i < n; ++i) {
        for (int u = 1; u <= n; ++u) {
            for (auto [v, c] : adj[u]) {
                if (dist[u] != INT_MAX && dist[v] > dist[u] + c) {
                    dist[v] = dist[u] + c;
                    parent[v] = u;
                }
            }
        }
    }
}