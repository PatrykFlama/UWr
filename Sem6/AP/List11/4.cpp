#include <bits/stdc++.h>
using namespace std;

#define cerr if(0) cout
#define fst first
#define snd second
typedef long long ll;
typedef pair<int, int> pii;
typedef pair<int, ll> pill;
typedef pair<ll, int> plli;
typedef pair<ll, ll> pll;


class Graph {
public:
    vector<unordered_set<int>> adj;
    vector<int> deg;

    Graph(int n) {   
        adj.resize(n + 1);
        deg.resize(n + 1, 0);
    }

    void addEdge(int u, int v) {
        adj[u].insert(v);
        adj[v].insert(u);
        deg[v]++, deg[u]++;
    }
};


vector<int> path;
void dfs_euler(int v, Graph &g) {
    while (!g.adj[v].empty()) {
        int u = *g.adj[v].begin();

        g.adj[v].erase(u);
        g.adj[u].erase(v);

        dfs_euler(u, g);
    }

    path.push_back(v);
}


int main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);

    int n, m; cin >> n >> m;
    Graph g(n);

    for (int i = 0; i < m; i++) {
        int u, v;
        cin >> u >> v;
        g.addEdge(u, v);
    }

    // check if all reachable
    int reachable_edges = g.adj[1].size();
    vector<bool> vis(n+1, 0);
    queue<int> q;
    q.push(1);
    vis[1] = 1;

    while(!q.empty()) {
        int v = q.front();
        q.pop();

        for (int u : g.adj[v]) {
            if (!vis[u]) {
                vis[u] = 1;
                q.push(u);
                reachable_edges += g.adj[u].size();
            }
        }
    }

    if (reachable_edges/2 != m) {
        cout << "IMPOSSIBLE\n";
        cerr << "1\n";
        return 0;
    }
    
    // check if all deg even
    for (int v = 1; v <= n; v++) {
        if (g.deg[v]%2) {
            cout << "IMPOSSIBLE\n";
            cerr << "2\n";
            return 0;
        }
    }
    
    // find euler cycle
    dfs_euler(1, g);
    
    if (path.size() != m + 1) {
        cout << "IMPOSSIBLE\n";
        cerr << "3\n";
        return 0;
    }

    for (int v = path.size()-1; v >= 0; v--) 
        cout << path[v] << ' ';
    cout << '\n';
}
