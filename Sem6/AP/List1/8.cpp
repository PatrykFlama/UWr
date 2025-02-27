#include <bits/stdc++.h>
using namespace std;

typedef long long ll;

vector<vector<int>> graph;
vector<int> dist;


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

    dist.resize(n, -1);
    dist[0] = 0;

    queue<int> q;
    q.push(0);

    while(!q.empty()) {
        int v = q.front();
        q.pop();

        for(int u : graph[v]) {
            if(dist[u] != -1) continue;

            dist[u] = dist[v] + 1;
            q.push(u);
        }
    }

    for(int i = 1; i < n; i++) {
        cout << dist[i] << ' ';
    }
    cout << '\n';
}