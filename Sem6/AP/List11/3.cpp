#include <bits/stdc++.h>
using namespace std;

#define cerr if(1) cout
#define fst first
#define snd second
typedef long long ll;
typedef pair<int, int> pii;


// scc->dag + topo + dp on topo from the end



class Graph {
public:
    vector<vector<int>> adj;
    vector<int> cost;

    Graph(int n) {   
        adj.resize(n + 1);
        cost.resize(n + 1);
    }

    void addEdge(int u, int v) {
        adj[u].push_back(v);
    }
};


class SCC {
public:
    Graph g;
    vector<int> sccId;
    int cnt = 0;

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
        reverse(postOrder.begin(), postOrder.end());

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

            cnt = i;
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

    for (int v = 1; v <= n; v++) {
        cin >> g.cost[v];
    }

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

    // calc supernode coins
    vector<ll> sccCoins(scc.cnt, 0);
    for (int v = 1; v <= n; v++) {
        sccCoins[scc.sccId[v]] += g.cost[v];
    }

    // build DAG from SCC
    vector<vector<int>> dag(scc.cnt);
    vector<int> indegree(scc.cnt, 0);
    set<pair<int, int>> edgeSet; // avoid duplicates

    for (int u = 1; u <= n; u++) {
        int cu = scc.sccId[u];

        for (int v : g.adj[u]) {
            int cv = scc.sccId[v];

            if (cu != cv && edgeSet.emplace(cu, cv).second) {
                dag[cu].push_back(cv);
                indegree[cv]++;
            }
        }
    }

    // toposort + DP
    vector<ll> dp(scc.cnt, 0);
    queue<int> q;

    for (int i = 0; i < scc.cnt; i++) {
        if (indegree[i] == 0) {
            dp[i] = sccCoins[i];
            q.push(i);
        }
    }

    ll res = 0;
    while (!q.empty()) {
        int u = q.front(); q.pop();
        
        for (int v : dag[u]) {
            if (dp[v] < dp[u] + sccCoins[v]) {
                dp[v] = dp[u] + sccCoins[v];
            }

            if (--indegree[v] == 0) {
                q.push(v);
            }
        }

        res = max(res, dp[u]);
    }

    cout << res << '\n';
}
