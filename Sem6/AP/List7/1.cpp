#include <bits/stdc++.h>
using namespace std;

#define cerr if(1) cout
#define fst first
#define snd second
typedef long long ll;
typedef pair<int, int> pii;


constexpr int N = 2e5+3;
constexpr int M = 1e6+3;

#define int long long

// https://cp-algorithms.com/data_structures/segment_tree.html
class SegmentTree {
private:
    // vector<int> tree;
    int *tree;
    int n;
    int NEUTRAL = 0;
    function<int(int, int)> op = [](int a, int b) { return a + b; };

    void update(int v, int start, int end, int idx, int val) {
        if (start == end) {
            tree[v] = val;
        } else {
            int mid = (start + end) / 2;
            if (idx <= mid) {
                update(2 * v, start, mid, idx, val);
            } else {
                update(2 * v + 1, mid + 1, end, idx, val);
            }
            tree[v] = tree[2 * v] + tree[2 * v + 1];
        }
    }

    int query(int v, int start, int end, int l, int r) {
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

    void build(const vector<int>& tab, int v, int start, int end) {
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
    SegmentTree(int n, int _neutral=0) : n(n), NEUTRAL(_neutral) {
        tree = new int[4 * n];
        fill(tree, tree + 4 * n, _neutral);
    }

    SegmentTree(const vector<int>& tab, int _neutral=0) : n(tab.size()), NEUTRAL(_neutral) {
        tree = new int[4 * n];
        fill(tree, tree + 4 * n, _neutral);
        build(tab, 1, 0, n - 1);
    }

    ~SegmentTree() {
        delete[] tree;
    }
    
    void change_neutral(int _neutral) {
        NEUTRAL = _neutral;
        fill(tree, tree + 4 * n, _neutral);
    }
    void change_op(function<int(int, int)> _op) {
        op = _op;
    }
    
    //? update(idx, val) - update value at idx to val
    void update(int idx, int val) {
        update(1, 0, n - 1, idx, val);
    }

    //? query(l, r) - query op in range [l, r]
    int query(int l, int r) {
        return query(1, 0, n - 1, l, r);
    }
};


int32_t main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);

    int n; cin >> n;
    vector<int> tab(n);
    for (int i = 0; i < n; ++i) {
        cin >> tab[i];
    }

    SegmentTree st(M);

    int res = 0;
    for (int i = 0; i < n; ++i) {
        st.update(tab[i], st.query(tab[i], tab[i]) + 1);
        res += st.query(tab[i] + 1, M - 1);
    }

    cout << res << '\n';
}
