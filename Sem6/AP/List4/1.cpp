#include <bits/stdc++.h>
using namespace std;

#define cerr if(0) cout
typedef long long ll;

constexpr int L = 2e5+5;


class SqrtTree {
public:
    int n, n_buck, buck_size;
    vector<int> arr;
    vector<ll> summ;

    SqrtTree(int n) : n(n) {
        arr.resize(n);

        n_buck = ceil(sqrt(n));
        summ.resize(n_buck);

        buck_size = ceil((double)n / n_buck);
    }

    void build() {
        for(int i = 0; i < n; i++) {
            summ[i / buck_size] += arr[i];
        }
    }

    void update(int pos, int val) {
        summ[pos / buck_size] += -arr[pos] + val;
        arr[pos] = val;
    }

    ll query(int _from, int _to) {
        int from = _from, to = _to;
        ll sum = 0;

        // calc prefix
        while(from % buck_size != 0 && from <= to) {
            cerr << from << ":" << arr[from] << ' ';
            sum += arr[from++];
        }
        
        cerr << " | ";
        
        // calc blocks
        while(from + buck_size <= to) {
            cerr << from << ":" << summ[from / buck_size] << ' ';
            sum += summ[from / buck_size];
            from += buck_size;
        }

        cerr << " | ";

        // calc suffix
        while(from <= to) {
            cerr << from << ":" << arr[from] << ' ';
            sum += arr[from++];
        }
        cerr << '\n';

        return sum;
    }
};


int main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);

    int n, q; cin >> n >> q;
    SqrtTree st(n);

    for(int i = 0; i < n; i++) {
        cin >> st.arr[i];
    }
    st.build();

    while(q--) {
        int op, a, b; cin >> op >> a >> b;

        if(op == 1) { // update
            st.update(a-1, b);
        } else { // query
            cout << st.query(a-1, b-1) << '\n';
        }
    }
}