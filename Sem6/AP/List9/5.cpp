#include <bits/stdc++.h>
using namespace std;

#define cerr \
    if (1) cout
#define fst first
#define snd second
typedef long long ll;
typedef pair<int, int> pii;

namespace SegmentTree {

struct Node {
    int val;

    Node(int _val = 0) : val(_val) {}

    bool operator==(const Node& other) const {
        return val == other.val;
    }
    bool operator!=(const Node& other) const {
        return !(*this == other);
    }
};

class LazySegmentTree {
   private:
    vector<Node> tree;
    vector<Node> lazy;
    int n;
    Node NEUTRAL;

    //? applied to children, to calculate parent
    function<Node(const Node&, const Node&)> op;
    //? lazy_op(node, lazy val) - applies lazy value to node
    function<Node(const Node&, const Node&)> lazy_op;

    void push(int v, int start, int end) {
        if (lazy[v] != NEUTRAL) {
            if (start != end) {
                lazy[2 * v] = lazy_op(lazy[2 * v], lazy[v]);
                lazy[2 * v + 1] = lazy_op(lazy[2 * v + 1], lazy[v]);
            }

            tree[v] = lazy_op(tree[v], lazy[v]);
            lazy[v] = NEUTRAL;
        }
    }

    void update(int v, int start, int end, int l, int r, Node val) {
        push(v, start, end);

        if (r < start || end < l)
            return;

        if (l <= start && end <= r) {
            lazy[v] = lazy_op(lazy[v], val);
            push(v, start, end);
            return;
        }

        int mid = (start + end) / 2;
        update(2 * v, start, mid, l, r, val);
        update(2 * v + 1, mid + 1, end, l, r, val);
        tree[v] = op(tree[2 * v], tree[2 * v + 1]);
    }

    Node query(int v, int start, int end, int l, int r) {
        push(v, start, end);

        if (r < start || end < l)
            return NEUTRAL;

        if (l <= start && end <= r)
            return tree[v];

        int mid = (start + end) / 2;
        return op(
            query(2 * v, start, mid, l, r),
            query(2 * v + 1, mid + 1, end, l, r));
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

   public:
    LazySegmentTree(int n, Node _neutral,
                    function<Node(const Node&, const Node&)> _op, function<Node(const Node&, const Node&)> _lazy_op) : n(n), NEUTRAL(_neutral), op(_op), lazy_op(_lazy_op) {
        tree.assign(4 * n, _neutral);
        lazy.assign(4 * n, _neutral);
    }

    LazySegmentTree(const vector<Node>& tab, Node _neutral,
                    function<Node(const Node&, const Node&)> _op, function<Node(const Node&, const Node&)> _lazy_op) : n(tab.size()), NEUTRAL(_neutral), op(_op), lazy_op(_lazy_op) {
        tree.assign(4 * n, _neutral);
        lazy.assign(4 * n, _neutral);
        build(tab, 1, 0, n - 1);
    }

    //? update(l, r, val) - add val to range [l, r]
    void update(int l, int r, Node val) {
        update(1, 0, n - 1, l, r, val);
    }

    //? query(l, r) - query with op function
    Node query(int l, int r) {
        return query(1, 0, n - 1, l, r);
    }
};

}  // namespace SegmentTree


// now we want to perform range updates on euler tour;
// note the first and last visit of a node - all nodes between are its children
// thus simply on changin value of v into x_new, we can remove x_old from range [first, last] and add x_new to this range

constexpr int MAXN = 2e5 + 5;

class SubtreeSum {
   public:
    SegmentTree::LazySegmentTree segTree;
    vector<int> firstVisit, lastVisit;
    vector<int> eulerTour;
    vector<vector<int>> adj;
    vector<int> values;

    SubtreeSum(int n) : segTree(1, SegmentTree::Node(0),
        [](SegmentTree::Node a, SegmentTree::Node b) { return SegmentTree::Node(a.val + b.val); },
        [](SegmentTree::Node a, SegmentTree::Node b) { return SegmentTree::Node(a.val + b.val); }
    ) {
        firstVisit.assign(n + 1, -1);
        lastVisit.assign(n + 1, -1);
        adj.resize(n + 1);
        values.assign(n + 1, 0);
    }

    void dfs(int v, int parent) {
        firstVisit[v] = eulerTour.size();
        eulerTour.push_back(v);

        for (int u : adj[v]) {
            if (u != parent) {
                dfs(u, v);
            }
        }

        lastVisit[v] = eulerTour.size() - 1;
    }

    void addEdge(int v, int u) {
        adj[v].push_back(u);
        adj[u].push_back(v);
    }

    void build() {
        dfs(1, -1);

        segTree = SegmentTree::LazySegmentTree(eulerTour.size(), SegmentTree::Node(0), 
            [](SegmentTree::Node a, SegmentTree::Node b) { return SegmentTree::Node(a.val + b.val); }, 
            [](SegmentTree::Node a, SegmentTree::Node b) { return SegmentTree::Node(a.val + b.val); });
    }

    void update(int v, int x) {
        segTree.update(firstVisit[v], lastVisit[v], SegmentTree::Node(x-values[v]));
        values[v] = x;
    }

    SegmentTree::Node query(int v) {
        return segTree.query(firstVisit[v], lastVisit[v]);
    }
    SegmentTree::Node query(int l, int r) {
        return segTree.query(l, r);
    }
};

int main() {
    ios::sync_with_stdio(0);
    cin.tie(0);

    int n, q;
    cin >> n >> q;

    SubtreeSum ss(n);

    // remember initial values
    vector<int> values(n + 1);
    for (int i = 1; i <= n; ++i) {
        cin >> values[i];
    }

    // build euler tree
    for (int i = 0; i < n - 1; ++i) {
        int u, v;
        cin >> u >> v;
        ss.addEdge(u, v);
    }

    ss.build();

    // fill tree with initial values
    for (int i = 1; i <= n; ++i) {
        ss.update(i, values[i]);
    }

    // answer queries
    while (q--) {
        int type;
        cin >> type;
        if (type == 1) {
            int v, x;
            cin >> v >> x;
            ss.update(v, x);
        } else {
            int v;
            cin >> v;
            cout << ss.query(v).val << '\n';
        }
    }
}
