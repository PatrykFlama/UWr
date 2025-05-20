#include <bits/stdc++.h>
using namespace std;

#define cerr if(1) cout
#define fst first
#define snd second
typedef long long ll;
typedef pair<int, int> pii;


class Graph {
public:
    vector<vector<int>> adj;

    Graph(int n) {   
        adj.resize(n + 1);
    }

    void addEdge(int u, int v) {
        adj[u].push_back(v);
    }
};


class SCC {
public:
    Graph g;
    vector<int> sccId;

    SCC(int n) : g(n) {}
    SCC(Graph& g) : g(g) {}

    void calcSCC() {
        int n = g.adj.size();
        vector<int> postOrder;
        
        vector<bool> vis(n, false);
        for (int i = 1; i < n; i++) {
            if (!vis[i]) {
                dfs(i, vis, g.adj, postOrder);
            }
        }

        vector<vector<int>> gT(n);
        for (int u = 1; u < n; u++) {
            for (int v : g.adj[u]) {
                gT[v].push_back(u);
            }
        }

        vis.assign(n, false);
        // for (int i = 0; i < n; i++) cout << postOrder[i] << ' ';
        // cout << '\n';
        reverse(postOrder.begin(), postOrder.end());
        // for (int i = 0; i < n; i++) cout << postOrder[i] << ' ';
        // cout << '\n';

        sccId.assign(n, -1);

        for (int i = 0; i < n; i++) {
            int v = postOrder[i];
            if (!vis[v]) {
                vector<int> component;
                dfs(v, vis, gT, component);
                for (int u : component) {
                    sccId[u] = i;
                }
            }
        }
    }

    void dfs(int u, vector<bool>& vis, vector<vector<int>> &adj, vector<int>& post) {
        vis[u] = true;
        for (int v : adj[u]) {
            if (!vis[v]) {
                dfs(v, vis, adj, post);
            }
        }
        post.push_back(u);
    }
};


int main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);

    int n, m; cin >> n >> m;
    Graph g(n);

    for (int i = 0; i < m; i++) {
        int u, v; cin >> u >> v;
        g.addEdge(u, v);
    }

    SCC scc(g);
    scc.calcSCC();
    // for (int i = 1; i <= n; i++) {
    //     cout << scc.sccId[i] << ' ';
    // }
    // cout << '\n';

    vector<int> reindex(n + 1, -1);
    int index = 1;

    for (int i = 1; i <= n; i++) {
        if (reindex[scc.sccId[i]] == -1) {
            reindex[scc.sccId[i]] = index++;
        }
        scc.sccId[i] = reindex[scc.sccId[i]];
    }

    cout << index - 1 << '\n';
    for (int i = 1; i <= n; i++) {
        cout << scc.sccId[i] << ' ';
    }
    cout << '\n';
}