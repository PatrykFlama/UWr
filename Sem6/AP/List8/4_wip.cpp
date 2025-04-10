#include <bits/stdc++.h>
using namespace std;

#define cerr if(1) cout
#define fst first
#define snd second
typedef long long ll;
typedef pair<int, int> pii;


struct Node {
    ll val;

    Node(ll val = 0) : val(val) {}

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
    vector<Node> add_lazy;
    vector<Node> set_lazy;
    int n;
    Node NEUTRAL;
    Node NULL_NODE;

    //? applied to children, to calculate parent
    function<Node(const Node&, const Node&)> op;
    //? lazy_op(node, add_lazy val) - applies add_lazy value to node 
    function<Node(const Node&, const Node&)> lazy_op;
    //? range_val(val, range size) - result value of the range
    function<Node(const Node&, const int)> range_val;

    //? push(v, start, end) - push value to children (set has higher priority than add)
    void push(int v, int start, int end) {
        if (set_lazy[v] != NULL_NODE) {
            if (start != end) {
                set_lazy[2 * v] = set_lazy[v];
                set_lazy[2 * v + 1] = set_lazy[v];

                // add_lazy[2 * v] = NEUTRAL;
                // add_lazy[2 * v + 1] = NEUTRAL;
            }

            tree[v] = range_val(set_lazy[v], end - start + 1);
            add_lazy[v] = NEUTRAL;
            set_lazy[v] = NULL_NODE;
        }

        if (add_lazy[v] != NEUTRAL) {
            if (start != end) {
                add_lazy[2 * v] = lazy_op(add_lazy[2 * v], add_lazy[v]);
                add_lazy[2 * v + 1] = lazy_op(add_lazy[2 * v + 1], add_lazy[v]);
            }

            tree[v] = lazy_op(tree[v], range_val(add_lazy[v], end - start + 1));
            add_lazy[v] = NEUTRAL;
        }
    }

    void update_add(int v, int start, int end, int l, int r, Node val) {
        push(v, start, end);

        if (r < start || end < l)
            return;

        if (l <= start && end <= r) {
            add_lazy[v] = lazy_op(add_lazy[v], val);
            push(v, start, end);
            return;
        }

        int mid = (start + end) / 2;
        update_add(2 * v, start, mid, l, r, val);
        update_add(2 * v + 1, mid + 1, end, l, r, val);
        tree[v] = op(tree[2 * v], tree[2 * v + 1]);
    }

    void update_set(int v, int start, int end, int l, int r, Node val) {
        push(v, start, end);

        if (r < start || end < l)
            return;

        if (l <= start && end <= r) {
            set_lazy[v] = val;
            push(v, start, end);
            return;
        }

        int mid = (start + end) / 2;
        update_set(2 * v, start, mid, l, r, val);
        update_set(2 * v + 1, mid + 1, end, l, r, val);
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
            query(2 * v + 1, mid + 1, end, l, r)
        );
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
    //? LazySegmentTree(n, neutral, null_node, op, lazy_op, range_val)
    LazySegmentTree(int n, Node _neutral, Node _null_node,
                    function<Node(const Node&, const Node&)> _op, 
                    function<Node(const Node&, const Node&)> _lazy_op,
                    function<Node(const Node&, const int)> _range_val) :
                n(n), NEUTRAL(_neutral), NULL_NODE(_null_node), 
                op(_op), lazy_op(_lazy_op), range_val(_range_val) {
        tree.assign(4 * n, NEUTRAL);
        add_lazy.assign(4 * n, NEUTRAL);
        set_lazy.assign(4 * n, NULL_NODE);
    }

    //? LazySegmentTree(tab, neutral, null_node, op, lazy_op, range_val)
    LazySegmentTree(const vector<Node>& tab, Node _neutral, Node _null_node,
                    function<Node(const Node&, const Node&)> _op, 
                    function<Node(const Node&, const Node&)> _lazy_op,
                    function<Node(const Node&, const int)> _range_val) :
                n(tab.size()), NEUTRAL(_neutral), NULL_NODE(_null_node), 
                op(_op), lazy_op(_lazy_op), range_val(_range_val) {
        tree.assign(4 * n, NEUTRAL);
        add_lazy.assign(4 * n, NEUTRAL);
        set_lazy.assign(4 * n, NULL_NODE);
        build(tab, 1, 0, n - 1);
    }

    //? update(l, r, val, should values be overwritten) - add val to range [l, r]
    void update(int l, int r, Node val, bool is_set = false) {
        if (is_set) {
            update_set(1, 0, n - 1, l, r, val);
        } else {
            update_add(1, 0, n - 1, l, r, val);
        }
    }

    //? query(l, r) - query with op function
    Node query(int l, int r) {
        return query(1, 0, n - 1, l, r);
    }
};




int32_t main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);

    int n, q; cin >> n >> q;

    vector<Node> a(n);
    for(int i = 0; i < n; i++) {
        cin >> a[i].val;
    }

    LazySegmentTree st(a, {0}, {-1},
        [](const Node& a, const Node& b) {
            return Node{ a.val + b.val };
        },
        [](const Node& val, const Node& lazy_val) {
            return Node{ val.val + lazy_val.val };
        },
        [](const Node& val, const int size) {
            return Node{ val.val * size };
        }
    );

    while (q--) {
        int op, x, y; cin >> op >> x >> y;
        --x, --y;

        if (op == 1) {
            int v; cin >> v;
            st.update(x, y, Node{v}, false);
        } else if(op == 2) {
            int v; cin >> v;
            st.update(x, y, Node{v}, true);
        } else {
            Node res = st.query(x, y);
            cout << res.val << '\n';
        }
    }
}