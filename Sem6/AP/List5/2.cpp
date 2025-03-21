// DIVIDE & CONQUER

#include <bits/stdc++.h>
#include <bit>
using namespace std;

#define cerr if(1) cout
#define fst first
#define snd second
typedef long long ll;
typedef pair<int, int> pii;

constexpr int L = 2e5 + 5;
int tab[L], ans[L];

struct Query {
    int l, r, ptr;
};

void solve(int l, int r, vector<Query> &qrs) {
    if(l == r) {
        for(auto q : qrs) {
            ans[q.ptr] = tab[l];
        }
        return;
    }

    // filter queries
    int mid = (l+r) / 2;
    vector<Query> qleft, qright, qmid;

    for(auto q : qrs) {
        if(q.r <= mid) 
            qleft.push_back(q);
        else if(q.l > mid) 
            qright.push_back(q);
        else 
            qmid.push_back(q);
    }

    solve(l, mid, qleft);
    solve(mid + 1, r, qright);

    // answer split queries
    vector<int> lmin(mid - l + 3, INT_MAX);
    vector<int> rmin(r - mid + 3, INT_MAX);

    lmin[0] = tab[mid];
    for(int i = mid - 1; i >= l; i--)
        lmin[mid - i] = min(lmin[mid - i - 1], tab[i]);
    
    rmin[0] = tab[mid + 1];
    for(int i = mid + 2; i <= r; i++)
        rmin[i - mid - 1] = min(rmin[i - mid - 2], tab[i]);

    for(auto &q : qmid) {
        const int minLeft = lmin[mid - q.l];
        const int minRight = rmin[q.r - mid - 1];
        ans[q.ptr] = min(minLeft, minRight);
    }
}

int main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);

    int n, q;
    cin >> n >> q;
        
    for(int i = 0; i < n; i++) {
        cin >> tab[i];
    }

    vector<Query> qrs(q);
    for(int i = 0; i < q; i++) {
        qrs[i].ptr = i;

        cin >> qrs[i].l >> qrs[i].r;
        qrs[i].l--; qrs[i].r--;
    }

    solve(0, n - 1, qrs);

    for(int i = 0; i < q; i++) {
        cout << ans[i] << '\n';
    }
}
