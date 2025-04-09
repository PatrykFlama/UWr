#include <bits/stdc++.h>
using namespace std;

#define cerr if(1) cout
#define fst first
#define snd second
typedef long long ll;
typedef pair<int, int> pii;


// now we want to keep pair{max no rooms, max hotel idx}

class Node {
public:
    ll rooms, idx;
    Node() : rooms(0), idx(0) {}
    Node(ll _rooms, ll _idx) : rooms(_rooms), idx(_idx) {}
};

class SegmentTree {
private:
    Node *tree;
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

    Node find(int v, int start, int end, function<bool(Node, Node)> &cmp) {
        if (start == end) {
            return tree[v];
        } else {
            int mid = (start + end) / 2;
            bool go_left = cmp(tree[2 * v], tree[2 * v + 1]);

            if (go_left) {
                return find(2 * v, start, mid, cmp);
            } else {
                return find(2 * v + 1, mid + 1, end, cmp);
            }
        }
    }

public:
    SegmentTree(int n, Node _neutral) : n(n), NEUTRAL(_neutral) {
        tree = new Node[4 * n];
        fill(tree, tree + 4 * n, _neutral);
    }

    SegmentTree(const vector<Node>& tab, Node _neutral, function<Node(Node, Node)> _op) : 
            n(tab.size()), NEUTRAL(_neutral), op(_op) {
        tree = new Node[4 * n];
        fill(tree, tree + 4 * n, _neutral);
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

    //? find_leaf(cmp) - find leaf with cmp function
    Node find(function<bool(Node, Node)> cmp) {
        return find(1, 0, n - 1, cmp);
    }
};
    
    


int main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);

    int n, q; cin >> n >> q;

    vector<Node> a(n);
    for (int i = 0; i < n; i++) {
        cin >> a[i].rooms;
        a[i].idx = i + 1;
    }

    SegmentTree st(a, Node(0, 0), [](Node a, Node b) {
        return Node{
            max(a.rooms, b.rooms),
            a.rooms >= b.rooms ? a.idx : b.idx
        };
    });

    while (q--) {
        int x; cin >> x;

        Node first_hotel = st.find([x](Node a, Node b) {
            if (a.rooms >= x) return true;
            return false;
        });

        if (first_hotel.rooms < x) {
            cout << 0 << ' ';
            continue;
        }

        st.update(first_hotel.idx - 1, Node(first_hotel.rooms - x, first_hotel.idx));

        cout << first_hotel.idx << ' ';
    }

    cout << '\n';
}