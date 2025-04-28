#include <bits/stdc++.h>
using namespace std;

#define cerr \
    if (1) cout
#define fst first
#define snd second
typedef long long ll;
typedef pair<int, int> pii;

#define int long long


namespace SegmentTree {

struct Node {
    int val;

    Node(int _val = 0) : val(_val) {}
};

class SegmentTree {
   private:
    vector<Node> tree;
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
                  query(2 * v + 1, mid + 1, end, max(l, mid + 1), r));
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
    SegmentTree(int _n, Node _neutral, function<Node(Node, Node)> _op)
        : n(_n), NEUTRAL(_neutral), op(_op) {
        tree.resize(4 * n, NEUTRAL);
    }

    SegmentTree(const vector<Node>& tab, Node _neutral, function<Node(Node, Node)> _op)
        : n(tab.size()), NEUTRAL(_neutral), op(_op) {
        tree.resize(4 * n, NEUTRAL);
        build(tab, 1, 0, n - 1);
    }

    void update(int idx, Node val) {
        update(1, 0, n - 1, idx, val);
    }

    Node query(int l, int r) {
        return query(1, 0, n - 1, l, r);
    }
};

}  // namespace SegmentTree

constexpr int MAXN = 2e5 + 5;

class PathSum {
   public:
    SegmentTree::SegmentTree segTree;
    vector<int> firstVisit, lastVisit;
    vector<int> eulerTour;
    vector<vector<int>> adj;
    vector<int> values;

    PathSum(int n, const vector<int>& _values)
        : segTree(1, SegmentTree::Node(0), [](SegmentTree::Node a, SegmentTree::Node b) { return SegmentTree::Node(a.val + b.val); }),
          values(_values) {
        firstVisit.resize(n + 1);
        lastVisit.resize(n + 1);
        adj.resize(n + 1);
    }

    void dfs(int v, int parent) {
        firstVisit[v] = eulerTour.size();
        eulerTour.push_back(v);
        
        for (int u : adj[v]) {
            if (u != parent) {
                dfs(u, v);
            }
        }
        
        eulerTour.push_back(v);
        lastVisit[v] = eulerTour.size() - 1;
    }

    void addEdge(int v, int u) {
        adj[v].push_back(u);
        adj[u].push_back(v);
    }

    void build() {
        dfs(1, -1);

        vector<SegmentTree::Node> initialSegValues(eulerTour.size(), SegmentTree::Node(0));
        for (int i = 1; i < lastVisit.size(); i++) {
            initialSegValues[firstVisit[i]] = SegmentTree::Node(values[i]);
            initialSegValues[lastVisit[i]] = SegmentTree::Node(-values[i]);
        }
        
        segTree = SegmentTree::SegmentTree(initialSegValues, SegmentTree::Node(0),
                                           [](SegmentTree::Node a, SegmentTree::Node b) { return SegmentTree::Node(a.val + b.val); });
    }

    void update(int v, int x) {
        segTree.update(firstVisit[v], SegmentTree::Node(x));
        segTree.update(lastVisit[v], SegmentTree::Node(-x));
        values[v] = x;
    }

    int query(int v) {
        return segTree.query(0, firstVisit[v]).val;
    }
};


int32_t main() {
    ios::sync_with_stdio(0);
    cin.tie(0);

    int n, q;
    cin >> n >> q;

    vector<int> values(n + 1);
    for (int i = 1; i <= n; i++) {
        cin >> values[i];
    }

    PathSum ps(n, values);

    for (int i = 0; i < n - 1; i++) {
        int u, v;
        cin >> u >> v;
        ps.addEdge(u, v);
    }

    ps.build();

    while (q--) {
        int type;
        cin >> type;
        if (type == 1) {
            int v, x;
            cin >> v >> x;
            ps.update(v, x);
        } else {
            int v;
            cin >> v;
            cout << ps.query(v) << '\n';
        }
    }
}
