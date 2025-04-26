#include <bits/stdc++.h>
using namespace std;

#define cerr \
    if (1) cout
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

void dfs(Node* v) {
    if (!v->parent) { // root
        v->depth = 0;
        v->jump = v;
    } else {
        v->depth = v->parent->depth + 1;
        if (v->parent->jump == nullptr || 
            v->parent->jump->depth != v->parent->depth - (1 << __builtin_ctz(v->parent->depth))) {
            v->jump = v->parent;
        } else {
            v->jump = v->parent->jump;
        }
    }
}

void preprocess(vector<Node>& nodes, const vector<vector<int>>& children) {
    vector<int> stack = {1};
    while (!stack.empty()) {
        int v = stack.back();
        stack.pop_back();
        dfs(&nodes[v]);
        for (int u : children[v]) {
            stack.push_back(u);
        }
    }
}


Node* find(Node* v, int d) {
    while (v->depth > d) {
        if (v->jump->depth < d) {
            v = v->parent;
        } else {
            v = v->jump;
        }
    }

    return v;
}

} // namespace BinaryLifting


int main() {
    ios::sync_with_stdio(0);
    cin.tie(0);

    int n, q;
    cin >> n >> q;

    vector<Node> nodes(n + 1);
    vector<vector<int>> children(n + 1);

    nodes[1] = {nullptr, nullptr, 0}; // root

    for (int i = 2; i <= n; ++i) {
        int p;
        cin >> p;
        nodes[i].parent = &nodes[p];
        children[p].push_back(i);
    }

    while (q--) {
        int v, k;
        cin >> v >> k;
        if (nodes[v].depth < k) {
            cout << -1 << '\n';
        } else {
            cout << (find(&nodes[v], nodes[v].depth - k) - &nodes[0]) << '\n';
        }
    }

    return 0;
}
