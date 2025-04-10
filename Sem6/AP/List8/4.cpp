#include <bits/stdc++.h>
using namespace std;

#define cerr if(1) cout
#define fst first
#define snd second
typedef long long ll;
typedef pair<int, int> pii;


struct Node {
    ll sum;
    ll to_add;
    ll to_set;
    bool has_set;

    Node(ll val = 0) : sum(val), to_add(0), to_set(0), has_set(false) {}
};

class LazySegmentTree {
private:
    int n;
    vector<Node> tree;

    void push(int v, int l, int r) {
        // set has higher priority than add
        if (tree[v].has_set) {
            // apply lazy set value
            tree[v].sum = (r - l + 1) * tree[v].to_set;

            // push to children
            if (l != r) {
                apply_set(2 * v, tree[v].to_set);
                apply_set(2 * v + 1, tree[v].to_set);
            }

            // reset the lazy values
            tree[v].to_set = 0;
            tree[v].has_set = false;
            tree[v].to_add = 0;
        }

        if (tree[v].to_add != 0) {
            // apply
            tree[v].sum += (r - l + 1) * tree[v].to_add;

            // push
            if (l != r) {
                apply_add(2*v, tree[v].to_add);
                apply_add(2*v+1, tree[v].to_add);
            }

            // reset
            tree[v].to_add = 0;
        }
    }

    void apply_add(int v, ll val) {
        if (tree[v].has_set) {
            tree[v].to_set += val;
        } else {
            tree[v].to_add += val;
        }
    }

    void apply_set(int v, ll val) {
        tree[v].to_set = val;
        tree[v].has_set = true;
        tree[v].to_add = 0;
    }

    void build(const vector<ll>& tab, int v, int start, int end) {
        if (start == end) {
            tree[v] = Node(tab[start]);
        } else {
            int mid = (start + end) / 2;
            build(tab, 2 * v, start, mid);
            build(tab, 2 * v + 1, mid + 1, end);
            tree[v].sum = tree[2 * v].sum + tree[2 * v + 1].sum;
        }
    }

    void update_add(int v, int start, int end, int l, int r, ll val) {
        push(v, start, end);
        if (r < start || end < l) return;
        if (l <= start && end <= r) {
            apply_add(v, val);
            push(v, start, end);
            return;
        }

        int mid = (start + end) / 2;
        update_add(2*v, start, mid, l, r, val);
        update_add(2*v+1, mid+1, end, l, r, val);
        tree[v].sum = tree[2*v].sum + tree[2*v+1].sum;
    }

    void update_set(int v, int start, int end, int l, int r, ll val) {
        push(v, start, end);
        if (r < start || end < l) return;
        if (l <= start && end <= r) {
            apply_set(v, val);
            push(v, start, end);
            return;
        }

        int mid = (start + end) / 2;
        update_set(2*v, start, mid, l, r, val);
        update_set(2*v+1, mid+1, end, l, r, val);
        tree[v].sum = tree[2*v].sum + tree[2*v+1].sum;
    }

    ll query(int v, int start, int end, int l, int r) {
        push(v, start, end);
        if (r < start || end < l) return 0;
        if (l <= start && end <= r) return tree[v].sum;
        int mid = (start + end) / 2;
        return query(2 * v, start, mid, l, r) + 
               query(2 * v + 1, mid + 1, end, l, r);
    }

public:
    LazySegmentTree(const vector<ll>& a) {
        n = a.size();
        tree.resize(4 * n);
        build(a, 1, 0, n - 1);
    }

    void add_range(int l, int r, ll val) {
        update_add(1, 0, n - 1, l, r, val);
    }

    void set_range(int l, int r, ll val) {
        update_set(1, 0, n - 1, l, r, val);
    }

    ll query(int l, int r) {
        return query(1, 0, n - 1, l, r);
    }
};

int main() {
    ios::sync_with_stdio(false);
    cin.tie(0);

    int n, q; cin >> n >> q;

    vector<ll> a(n);
    for (int i = 0; i < n; ++i){
        cin >> a[i];
    }

    LazySegmentTree st(a);

    while (q--) {
        int t, l, r; cin >> t >> l >> r;

        if (t == 1) {
            ll v; cin >> v;
            st.add_range(l - 1, r - 1, v);
        } else if (t == 2) {
            ll v; cin >> v;
            st.set_range(l - 1, r - 1, v);
        } else if (t == 3) {
            cout << st.query(l - 1, r - 1) << '\n';
        }
    }
}

