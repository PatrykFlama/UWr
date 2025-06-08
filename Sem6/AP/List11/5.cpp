#include <bits/stdc++.h>
using namespace std;

#define cerr if(0) cerr
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
    vector<int> indeg, outdeg;

    Graph(int n) {   
        adj.resize(n + 1);
        indeg.resize(n + 1, 0);
        outdeg.resize(n + 1, 0);
    }

    void addEdge(int u, int v) {
        adj[u].insert(v);
        indeg[v]++, outdeg[u]++;
    }

        vector<vector<int>> undirected;
    void buildUndirected() {
        undirected.resize(adj.size());
        for (int u = 1; u < (int)adj.size(); u++) {
            for (int v : adj[u]) {
                undirected[u].push_back(v);
                undirected[v].push_back(u);
            }
        }
    }

    bool isWeaklyConnected(int start) {
        buildUndirected();
        vector<bool> visited(adj.size(), false);
        queue<int> q;
        q.push(start);
        visited[start] = true;

        while (!q.empty()) {
            int v = q.front(); q.pop();
            for (int u : undirected[v]) {
                if (!visited[u]) {
                    visited[u] = true;
                    q.push(u);
                }
            }
        }

        for (int i = 1; i < (int)adj.size(); i++) {
            if ((indeg[i] + outdeg[i] > 0) && !visited[i])
                return false;
        }

        return true;
    }
};


vector<int> path;
void dfs_euler(int v, Graph &g) {
    while (!g.adj[v].empty()) {
        int u = *g.adj[v].begin();

        g.adj[v].erase(u);

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

    // check if all in weakly cc
    // actually we check that later

    // check if all deg even
    int start_n = 1, end_n = n;
    bool f_start = 0, f_finish = 0;
    for (int v = 1; v <= n; v++) {
        if (g.indeg[v] != g.outdeg[v]) {
            if (g.indeg[v] + 1 == g.outdeg[v] && !f_start) {
                f_start = 1;
                start_n = v;
                continue;
            } else if (g.indeg[v] == g.outdeg[v] + 1 && !f_finish) {
                f_finish = 1;
                end_n = v;
                continue;
            }

            cout << "IMPOSSIBLE\n";
            cerr << "2\n";
            return 0;
        }
    }

    // if (!g.isWeaklyConnected(1)) {
    //     cout << "IMPOSSIBLE\n";
    //     return 0;
    // }

    if (start_n != 1 || end_n != n) {
        cout << "IMPOSSIBLE\n";
        return 0;
    }
    
    // find euler cycle
    dfs_euler(1, g);
    
    if (path.size() != m + 1) {
        cout << "IMPOSSIBLE\n";
        cerr << "3\n";
        return 0;
    }

    cerr << path.size() << ": \n";
    for (int v = path.size()-1; v >= 0; v--) 
        cout << path[v] << ' ';
    cout << '\n';
}
