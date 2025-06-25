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

int main() {
    ios::sync_with_stdio(0);
    cin.tie(0);

    int n, m; cin >> n >> m;
    vector<vector<pair<int, int>>> adj(n + 1);
    vector<int> in_deg(n + 1), out_deg(n + 1);
    vector<bool> used(m, false);

    for (int i = 0; i < m; ++i) {
        int u, v; cin >> u >> v;
        adj[u].emplace_back(v, i);
        out_deg[u]++;
        in_deg[v]++;
    }

    // Euler path conditions
    bool ok = (out_deg[1] == in_deg[1] + 1) && (in_deg[n] == out_deg[n] + 1);
    for (int i = 2; i < n && ok; ++i)
        if (in_deg[i] != out_deg[i]) ok = false;

    if (!ok) {
        cout << "IMPOSSIBLE\n";
        return 0;
    }

    // dfs
    vector<int> path;
    stack<int> st;
    vector<int> idx(n + 1, 0);
    st.push(1);

    while (!st.empty()) {
        int v = st.top();
        while (idx[v] < adj[v].size() && used[adj[v][idx[v]].snd])
            idx[v]++;
        if (idx[v] == adj[v].size()) {
            path.push_back(v);
            st.pop();
        } else {
            auto [u, i] = adj[v][idx[v]++];
            used[i] = true;
            st.push(u);
        }
    }

    if (path.size() != m + 1 || path.back() != 1 || path.front() != n) {
        cout << "IMPOSSIBLE\n";
        return 0;
    }

    for (auto it = path.rbegin(); it != path.rend(); ++it)
        cout << *it << ' ';
    cout << '\n';
}
