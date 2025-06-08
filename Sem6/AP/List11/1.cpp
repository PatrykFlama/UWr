#include <bits/stdc++.h>
using namespace std;

#define cerr if(1) cout
#define fst first
#define snd second
typedef long long ll;
typedef pair<int, int> pii;

// impossible if there is a cycle

constexpr int MAX_N = 1e5+5, MAX_M = 2e5 + 5;
vector<int> adj[MAX_N];
int indeg[MAX_N];


int main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);


    int n, m; cin >> n >> m;
    for (int i = 0; i < m; i++) {
        int u, v; cin >> u >> v;
        adj[u].push_back(v);
        indeg[v]++;
    }

    priority_queue<int, vector<int>, greater<int>> q;
    int vis = 0;
    for (int i = 1; i <= n; i++) {
        if (indeg[i] == 0) {
            q.push(i);
            vis++;
        }
    }


    vector<int> res;
    res.reserve(n);
    while (!q.empty()) {
        int u = q.top(); q.pop();
        res.push_back(u);

        for (int v : adj[u]) {
            indeg[v]--;
            if (indeg[v] == 0) {
                q.push(v);
                vis++;
            }
        }
    }

    if (vis != n) {
        cout << "IMPOSSIBLE\n";
        return 0;
    }

    for (int x : res) cout << x << ' ';
    cout << '\n';
}
