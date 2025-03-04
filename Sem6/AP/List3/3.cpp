#include <bits/stdc++.h>
using namespace std;

typedef long long ll;

/*
UF with union by size, gives O(nlog*n)
additionally online calc n of cc
*/

constexpr int L = 1e5+3;
int parent[L], sub_size[L]; // sub size is calculated only for root of the tree


int uf_find(int x) {
    if(parent[x] == x) return x;
    return parent[x] = uf_find(parent[x]);
}

void uf_union(int a, int b) {
    a = uf_find(a);
    b = uf_find(b);

    if(a == b) return;

    if(sub_size[a] < sub_size[b]) swap(a, b);
    parent[b] = a;
    sub_size[a] += sub_size[b];
}


int main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);

    int V, E; cin >> V >> E;
    int cc = V;
    
    for(int i = 0; i < V; i++) {
        parent[i] = i;
        sub_size[i] = 1;
    }

    int max_cc = 1;
    while(E--) {
        int a, b; cin >> a >> b;

        if(uf_find(a) != uf_find(b)) {
            cc--;
            uf_union(a, b);
            max_cc = max(max_cc, sub_size[uf_find(a)]);
        } else {
            uf_union(a, b);
        }

        cout << cc << ' ' << max_cc << '\n';
    }
}