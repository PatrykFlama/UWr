#include <bits/stdc++.h>
using namespace std;

typedef long long ll;

constexpr int L = 2e6+2;
vector<int> adj[L];
int nodes[L];
int n;


inline void calc_nodes(int v) {
    nodes[v] = 1;

    for(int u : adj[v]) {
        if(nodes[u] == 0) calc_nodes(u);
        nodes[v] += nodes[u];
    }
}


int main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);

    cin >> n;
    for(int i = 1; i < n; i++) {
        int father; cin >> father;
        adj[father-1].push_back(i);
    }

    calc_nodes(0);

    for(int i = 0; i < n; i++) 
        cout << nodes[i]-1 << ' ';
    cout << '\n';
}
