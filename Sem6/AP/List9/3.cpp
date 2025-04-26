#include <bits/stdc++.h>
using namespace std;

#define cerr if (1) cout
#define fst first
#define snd second
typedef long long ll;
typedef pair<int, int> pii;

constexpr int N = 2e5 + 5;


namespace BinaryLifting {

struct Node {
    int depth = -1;
    vector<int> jump;  // 2^i th ancestor
};

class Tree {
public:
    vector<Node> nodes;
    vector<vector<int>> adj;
    int maxLog;


    void dfs(int v, int parent) {
        nodes[v].jump[0] = parent;
        nodes[v].depth = (parent == -1 ? 0 : nodes[parent].depth + 1);

        for (int u : adj[v]) {
            if (u != parent) {
                dfs(u, v);
            }
        }
    }

    Tree(int n) : nodes(n + 1), adj(n + 1) {
        maxLog = 0;
        while ((1 << maxLog) <= n) ++maxLog;

        for (auto& node : nodes) {
            node.jump.assign(maxLog, -1);
        }
    }

    void addEdge(int v, int u) {
        adj[v].push_back(u);
        adj[u].push_back(v);
    }

 
    void preprocess() {
        // find order of the tree (parents+depth)
        dfs(1, -1);

        // fill the jump table
        for (int j = 1; j < maxLog; ++j) {
            for (int v = 1; v < nodes.size(); v++) {
                const int mid = nodes[v].jump[j - 1];
                if (mid != -1) {
                    nodes[v].jump[j] = nodes[mid].jump[j - 1];
                }
            }
        }
    }

    int findAncestor(int v, int k) const {
        if (nodes[v].depth < k) {
            return -1;
        }

        for (int i = 0; i < maxLog; i++) {
            if (k & (1 << i)) {
                v = nodes[v].jump[i];
                if (v == -1) break;
            }
        }
        return v;
    }

    int findLCA(int u, int v) const {
        if (nodes[u].depth < nodes[v].depth) {
            swap(u, v);
        }

        // bring u and v to the same depth
        u = findAncestor(u, nodes[u].depth - nodes[v].depth);
        if (u == v) {
            return u;
        }

        // find the LCA
        for (int j = maxLog - 1; j >= 0; --j) {
            if (nodes[u].jump[j] != -1 && nodes[u].jump[j] != nodes[v].jump[j]) {
                u = nodes[u].jump[j];
                v = nodes[v].jump[j];
            }
        }

        return nodes[u].jump[0];
    }
};

}  // namespace BinaryLifting


class CalcPaths {
public:
    BinaryLifting::Tree tree;
    // vector<int> pathCountBuff;  // pathCountBuff[i] = number of paths that start at i and go to root (first phase of pathCount)
    vector<int> pathCount;      // pathCount[i] = number of paths passing through i

    CalcPaths(int n) : tree(n) {
        pathCount.assign(n + 1, 0);
    }

    void addPath(int u, int v) {
        const int lca = tree.findLCA(u, v);

        pathCount[u]++;
        pathCount[v]++;
        pathCount[lca]--;
        if (tree.nodes[lca].jump[0] != -1) {
            pathCount[tree.nodes[lca].jump[0]]--;
        }
    }

    void pushPathsBuff(int v, int parent) {  // dfs
        int count = pathCount[v];

        for (int u : tree.adj[v]) {
            if (u != parent) {
                pushPathsBuff(u, v);
                count += pathCount[u];
            }
        }

        pathCount[v] = count;
    }

    int getPathCount(int v) const {
        return pathCount[v];
    }
};

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n, q;
    cin >> n >> q;

    CalcPaths cp(n);

    for (int i = 0; i < n - 1; i++) {
        int u, v;
        cin >> u >> v;
        cp.tree.addEdge(u, v);
    }

    cp.tree.preprocess();

    while (q--) {
        int u, v;
        cin >> u >> v;
        cp.addPath(u, v);
    }

    cp.pushPathsBuff(1, -1);  // push paths to the root

    for (int i = 1; i <= n; i++) {
        cout << cp.getPathCount(i) << ' ';
    }
    cout << '\n';
}
