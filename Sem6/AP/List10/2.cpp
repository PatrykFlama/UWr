#include <bits/stdc++.h>
using namespace std;

#define cerr if(1) cout
#define fst first
#define snd second
typedef long long ll;
typedef pair<int, int> pii;
typedef pair<int, ll> pill;
typedef pair<ll, int> plli;

constexpr int MAXN = (1e5 + 5) * 2;
ll dist[MAXN];
vector<pii> adj[MAXN];
int n, m;


int main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);

    cin >> n >> m;
    for (int i = 0; i < m; ++i) {
        int u, v, c;
        cin >> u >> v >> c;
        adj[u].push_back({v, c});
        adj[u].push_back({v+n, c/2});
        adj[u+n].push_back({v+n, c});
    }

    for (int i = 1; i <= n+n; ++i) {
        dist[i] = LONG_MAX;
    }

    priority_queue<plli, vector<plli>, greater<plli>> pq;
    pq.push({0, 1});
    dist[1] = 0;

    while (!pq.empty()) {
        const auto [d, v] = pq.top();
        pq.pop();

        if (d > dist[v]) continue;

        for (auto& edge : adj[v]) {
            const auto [u, c] = edge;

            if (dist[v] + c < dist[u]) {
                dist[u] = dist[v] + c;
                pq.push({dist[u], u});
            }
        }
    }

    cout << min(dist[n], dist[n+n]) << '\n';
}