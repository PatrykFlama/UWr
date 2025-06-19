#include <bits/stdc++.h>
using namespace std;

#define cerr if(1) cout
#define fst first
#define snd second
typedef long long ll;
typedef pair<int, int> pii;

/*
if there is positive cycle we can obtain infinite result, but it must be reachable from start and must reach end  
thus we will only traverse nodes that can be reached from start and can reach end  
then we use Bellman-Ford algorithm to find shortest path, and if there is a negative (with negated weights thus positive) cycle
*/

int n, m;

vector<tuple<int, int, int>> edges;
vector<vector<int>> G, GR;

vector<bool> reachable_from_start, reachable_to_end;

void dfs(int v, const vector<vector<int>>& graph, vector<bool>& visited) {
    visited[v] = true;
    for (int v : graph[v]) {
        if (!visited[v]) dfs(v, graph, visited);
    }
}



int main() {
    ios::sync_with_stdio(0);
    cin.tie(0);

    cin >> n >> m;
    G.resize(n + 1);
    GR.resize(n + 1);

    // construct graph and reversed graph
    for (int i = 0; i < m; ++i) {
        int v, u, w; cin >> v >> u >> w;
        edges.emplace_back(v, u, -w);
        G[v].push_back(u);
        GR[u].push_back(v);
    }


    // check reachability
    reachable_from_start.assign(n + 1, false);
    reachable_to_end.assign(n + 1, false);
    dfs(1, G, reachable_from_start);
    dfs(n, GR, reachable_to_end);

    // Bellman-Ford algorithm
    vector<ll> dist(n + 1, LLONG_MAX);
    dist[1] = 0;

    for (int i = 0; i < n - 1; ++i) {
        for (auto [u, v, w] : edges) {
            if (dist[u] != LLONG_MAX && dist[v] > dist[u] + w) {
                dist[v] = dist[u] + w;
            }
        }
    }

    // check for negative cycles reachable from start and reaching end
    for (auto [u, v, w] : edges) {
        if (dist[u] != LLONG_MAX && dist[v] > dist[u] + w) {
            if (reachable_from_start[u] && reachable_to_end[v]) {
                cout << -1 << '\n';
                return 0;
            }
        }
    }

    cout << -dist[n] << '\n';   // if no negative cycle, print the shortest path distance
}
