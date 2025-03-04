#include <bits/stdc++.h>
using namespace std;

#define cerr if(0) cout
typedef long long ll;
constexpr int L = 1e5 + 2;

struct NodeData {
    int min_v = INT_MAX, max_v = INT_MIN;
    int edges;
};

int parent[L];
NodeData nd[L]; // applies only to root nodes


int uf_find(int x) {
    return parent[x] == x ? 
        x : 
        parent[x] = uf_find(parent[x]);
}

void uf_union(int a, int b) {
    a = uf_find(a);
    b = uf_find(b);

    if(a != b) {
        parent[a] = b;
        nd[b].min_v = min(nd[a].min_v, nd[b].min_v);
        nd[b].max_v = max(nd[a].max_v, nd[b].max_v);
        nd[b].edges += nd[a].edges + 1;
    } else {
        nd[a].edges++;
    }
}

ll diversity_factor(int x) {
    x = uf_find(x);
    cerr << x << ": " << nd[x].min_v << ' ' << nd[x].max_v << ' ' << nd[x].edges << '\n';
    return (ll)(nd[x].max_v - nd[x].min_v) * (ll)nd[x].edges;
}


int main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);

    int n, m; cin >> n >> m;

    for(int i = 1; i <= n; i++) {
        parent[i] = i;
        nd[i].min_v = nd[i].max_v = i;
        nd[i].edges = 0;
    }

    while(m--) {
        int a, b; cin >> a >> b;

        uf_union(a, b);
        cout << diversity_factor(a) << '\n';
    }
}