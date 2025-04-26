/*
here we have implemented binary lifting with linear space usage and O(log n) query time
*/

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
    Node* parent = 0;
    Node* jump = 0;
    int depth = -1;
};

class Tree {
private:
    vector<Node> nodes;
    vector<vector<int>> children;

    void dfs(int v) {
        Node* node = &nodes[v];
        if (!node->parent) {
            node->depth = 0;
            node->jump = node;
        } else {
            node->depth = node->parent->depth + 1;
            if (!node->parent->jump ||
                node->parent->jump->depth != node->parent->depth - (1 << __builtin_ctz(node->parent->depth))) {
                node->jump = node->parent;
            } else {
                node->jump = node->parent->jump;
            }
        }
        for (int u : children[v]) {
            dfs(u);
        }
    }

public:
    Tree(int n) : nodes(n + 1), children(n + 1) {}

    void addEdge(int child, int parent) {
        nodes[child].parent = &nodes[parent];
        children[parent].push_back(child);
    }

    void preprocess() {
        dfs(1);
    }

    int findAncestor(int v, int k) {
        Node* node = &nodes[v];
        if (node->depth < k) {
            return -1;
        }
        int targetDepth = node->depth - k;
        while (node->depth > targetDepth) {
            if (node->jump->depth < targetDepth) {
                node = node->parent;
            } else {
                node = node->jump;
            }
        }
        return static_cast<int>(node - &nodes[0]);
    }
};

}  // namespace BinaryLifting

int main() {
    ios::sync_with_stdio(0);
    cin.tie(0);

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
}
