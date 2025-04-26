#include <bits/stdc++.h>
using namespace std;

#define cerr if(1) cout
#define fst first
#define snd second
typedef long long ll;
typedef pair<int, int> pii;


// now we want to keep pair{leftmost val, cnt of 'alive' vals}

class Node {
public:
    ll val, cnt;
    Node() : val(0), cnt(0) {}
    Node(ll _val, ll _cnt) : val(_val), cnt(_cnt) {}
};

class SegmentTree {
private:
    Node *tree;
    // vector<Node> tree;
    int n;
    Node NEUTRAL;
    function<Node(Node, Node)> op;

    void update(int v, int start, int end, int idx, Node val) {
        if (start == end) {
            tree[v] = val;
        } else {
            int mid = (start + end) / 2;
            if (idx <= mid) {
                update(2 * v, start, mid, idx, val);
            } else {
                update(2 * v + 1, mid + 1, end, idx, val);
            }
            tree[v] = op(tree[2 * v], tree[2 * v + 1]);
        }
    }

    Node query(int v, int start, int end, int l, int r) {
        if (r < l) {
            return NEUTRAL;
        }
        if (l == start && end == r) {
            return tree[v];
        }

        int mid = (start + end) / 2;
        return op(query(2 * v, start, mid, l, min(r, mid)),
                    query(2 * v + 1, mid + 1, end, max(l, mid+1), r));
    }

    void build(const vector<Node>& tab, int v, int start, int end) {
        if (start == end) {
            tree[v] = tab[start];
        } else {
            int mid = (start + end) / 2;
            build(tab, 2 * v, start, mid);
            build(tab, 2 * v + 1, mid + 1, end);
            tree[v] = op(tree[2 * v], tree[2 * v + 1]);
        }
    }

    Node find(int v, int start, int end, function<Node(Node, Node)> &cmp) {
        if (start == end) {
            return tree[v];
        } else {
            int mid = (start + end) / 2;
            Node left = find(2 * v, start, mid, cmp);
            Node right = find(2 * v + 1, mid + 1, end, cmp);
            return cmp(left, right);
        }
    }

    pair<Node, int> find(int v, int start, int end, function<bool(Node, Node)> &cmp) {
        if (start == end) {
            return {tree[v], start};
        } else {
            bool go_left = cmp(tree[2 * v], tree[2 * v + 1]);
            const int mid = (start + end) / 2;

            if(go_left) {
                return find(2 * v, start, mid, cmp);
            } else {
                return find(2 * v + 1, mid + 1, end, cmp);
            }
        }
    }

public:
    // SegmentTree(int n, Node _neutral) : n(n), NEUTRAL(_neutral) {
    //     tree = new Node[4 * n];
    //     fill(tree, tree + 4 * n, _neutral);
    // }

    SegmentTree(const vector<Node>& tab, Node _neutral, function<Node(Node, Node)> _op) : 
            n(tab.size()), NEUTRAL(_neutral), op(_op) {
        tree = new Node[4 * n];
        fill(tree, tree + 4 * n, _neutral);
        // tree.resize(4 * n, _neutral);
        build(tab, 1, 0, n - 1);
    }

    ~SegmentTree() {
        delete[] tree;
    }
    
    void change_op(function<Node(Node, Node)> _op) {
        op = _op;
    }
    
    //? update(idx, val) - update value at idx to val
    void update(int idx, Node val) {
        update(1, 0, n - 1, idx, val);
    }

    //? query(l, r) - query op in range [l, r]
    Node query(int l, int r) {
        return query(1, 0, n - 1, l, r);
    }

    //? find_leaf(cmp) - compare 2 leaf nodes found in subtrees
    Node find(function<Node(Node, Node)> cmp) {
        return find(1, 0, n - 1, cmp);
    }
    //? find_leaf(cmp) - compare 2 subtree nodes and decide if should go left
    pair<Node, int> find(function<bool(Node, Node)> cmp) {
        return find(1, 0, n - 1, cmp);
    }
};
    

// Node.cnt = idx, Node.val = depth
// segment tree will take min over depth, to find lca
const int MAXN = 2e5 + 5;
vector<int> adj[MAXN];
vector<int> euler;  // time of entry in euler tour
int depth[MAXN];
int first[MAXN];    // first appearance of a node in euler tour

void dfs(int v, int p, int d) {     // calc depth and euler tour
    depth[v] = d;
    first[v] = euler.size();
    euler.push_back(v);

    for (int u : adj[v]) {
        if (u != p) {
            dfs(u, v, d + 1);
            euler.push_back(v);
        }
    }
}


int main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);

    int n, q;
    cin >> n >> q;

    for (int i = 0; i < n - 1; ++i) {
        int u, v;
        cin >> u >> v;
        adj[u].push_back(v);
        adj[v].push_back(u);
    }

    dfs(1, -1, 0);

    vector<Node> tab;
    for (int v : euler) {
        tab.emplace_back(depth[v], v);
    }

    SegmentTree st(tab, Node(1e9, -1), [](Node a, Node b) {
        return (a.val < b.val) ? a : b;
    });

    while (q--) {
        int u, v;
        cin >> u >> v;
        int l = first[u];
        int r = first[v];
        if (l > r) swap(l, r);

        Node lca_node = st.query(l, r);
        int lca = lca_node.cnt;

        cout << depth[u] + depth[v] - 2 * depth[lca] << '\n';
    }
}