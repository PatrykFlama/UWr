#include <bits/stdc++.h>
using namespace std;

typedef long long ll;

constexpr int L = 1e6+5;

/*
lets keep corelation of parities on the forest;
parity array is parity with respect to the root of the set
the xor operation simply checks if parity is the same or not
*/

class UF {
public:
    vector<int> parent, parity;

    UF(int n) {
        parent.resize(n + 1);
        parity.resize(n + 1, 0);

        for(int i = 1; i <= n; i++)
            parent[i] = i;
    }

    int find(int x) {
        if(parent[x] == x) return x;

        const int root = find(parent[x]);
        parity[x] ^= parity[parent[x]];
        return parent[x] = root;
    }

    bool uni(int a, int b, int par) {
        int ra = find(a);
        int rb = find(b);

        if(ra == rb) {
            return (parity[a] ^ parity[b]) == par;
        }

        parent[rb] = ra;
        parity[rb] = parity[a] ^ parity[b] ^ par;

        return true;
    }
};

int main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);

    int n, m;
    cin >> n >> m;

    UF uf(n);

    int max_valid = 0;
    for(int i = 0; i < m; i++) {
        int a, b, p; cin >> a >> b >> p;

        if(!uf.uni(a, b, p)) {
            break;
        }

        max_valid++;
    }

    cout << max_valid << '\n';
}
