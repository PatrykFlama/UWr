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
private:
    vector<Node> nodes;
    int maxLog;

public:
    Tree(int n) : nodes(n + 1) {
        maxLog = 0;
        while ((1 << maxLog) <= n) ++maxLog;
        
        for (auto& node : nodes) {
            node.jump.assign(maxLog, -1);
        }
    }

    void addEdge(int child, int parent) {
        nodes[child].jump[0] = parent;
    }

    void preprocess() {
        nodes[1].depth = 0;  // root

        for (int v = 2; v < nodes.size(); v++) {
            const int p = nodes[v].jump[0];
            if (p != -1) {
                nodes[v].depth = nodes[p].depth + 1;
            }
        }

        for (int j = 1; j < maxLog; ++j) {
            for (int v = 1; v < nodes.size(); v++) {
                const int mid = nodes[v].jump[j - 1];
                if (mid != -1) {
                    nodes[v].jump[j] = nodes[mid].jump[j - 1];
                }
            }
        }
    }

    int findAncestor(int v, int k) {
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
};

}  // namespace BinaryLifting

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n, q;
    cin >> n >> q;

    BinaryLifting::Tree tree(n);

    for (int i = 2; i <= n; i++) {
        int p;
        cin >> p;
        tree.addEdge(i, p);
    }

    tree.preprocess();

    while (q--) {
        int v, k;
        cin >> v >> k;
        cout << tree.findAncestor(v, k) << '\n';
    }

    return 0;
}
