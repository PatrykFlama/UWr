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
    vector<vector<pair<int, bool>>> adj;
    vector<int> resEuler;

    Graph(int n, int m) {
        adj.resize(n + 1);
        resEuler.reserve(m+5);
    }

    void addEdge(int u, int v) {
        adj[u].push_back({v, 0});
    }

    void calcEuler(int v) {
        for (int i = 0; i < adj[v].size(); i++) {
            const auto [u, vis] = adj[v][i];

            if (vis) continue;

            adj[v][i].snd = 1;
            calcEuler(u);
        }

        cerr << v << ' ';
        resEuler.push_back(v);
    }
};

int main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);

    int n, m; cin >> n >> m;
    Graph g(n, m);

    for (int i = 0; i < m; i++) {
        int u, v;
        cin >> u >> v;
        g.addEdge(u, v);
    }

    // g.addEdge(n+1, 1);
    // g.addEdge(n, n+2);

    g.calcEuler(1);

    if (g.resEuler[0] != n || g.resEuler.size() != m+1) {
        cout << "IMPOSSIBLE\n";
        return 0;
    }
    cerr << "POSSIBLE:\n";

    cerr << g.resEuler.size() << ": \n";
    for (int v = g.resEuler.size()-1; v >= 0; v--) 
        cout << g.resEuler[v] << ' ';
    cout << '\n';
}
