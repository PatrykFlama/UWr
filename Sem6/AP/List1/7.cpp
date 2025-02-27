#include <bits/stdc++.h>
using namespace std;

typedef long long ll;

vector<vector<int>> graph;
vector<bool> vis;


int main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);


    int n, m;
    cin >> n >> m;

    graph.resize(n);

    for(int i = 0; i < m; i++) {
        int a, b;
        cin >> a >>b;
        graph[a-1].push_back(b-1);
        graph[b-1].push_back(a-1);
    }

    vis.resize(n, false);

    int cc = 0;
    for(int i = 0; i < n; i++) {
        if(vis[i]) continue;
        cc++;

        stack<int> s;
        s.push(i);
        vis[i] = true;

        while(!s.empty()) {
            int v = s.top();
            s.pop();

            for(int u : graph[v]) {
                if(vis[u]) continue;

                vis[u] = true;
                s.push(u);
            }
        }
    }

    cout << cc-1 << '\n';
}