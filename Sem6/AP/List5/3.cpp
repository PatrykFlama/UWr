// MONOTONIC QUEUE

#include <bits/stdc++.h>
using namespace std;

#define cerr if(1) cout
#define fst first
#define snd second
typedef long long ll;
typedef pair<int, int> pii;


constexpr int L = 2e5 + 5;
int tab[L], ans[L];
pii q[L];
int q_ptr = 0;

struct Query {
    int l, r, ptr;
};


void push_queue(int val, int idx) {
    while(q_ptr > 0 && q[q_ptr - 1].fst >= val) {
        q_ptr--;
    }

    q[q_ptr++] = {val, idx};
}

int find_min(int idx) {
    int l = 0, r = q_ptr - 1;
    while(l < r) {
        int mid = (l+r) / 2;
        if(q[mid].snd >= idx) {
            r = mid;
        } else {
            l = mid + 1;
        }
    }
    return q[l].fst;
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
    
    // we only need to sort by the end, to analyze all queries ending in r at once
    sort(qrs.begin(), qrs.end(), [&](const Query &a, const Query &b) {
        return a.r < b.r;
    });


    int tab_ptr = 0;
    int q_ptr = 0;
    while(tab_ptr < n && q_ptr < q) {
        // add current tab element to queue
        push_queue(tab[tab_ptr], tab_ptr);

        // process all queries ending in tab_ptr
        while(q_ptr < q && qrs[q_ptr].r == tab_ptr) {
            ans[qrs[q_ptr].ptr] = find_min(qrs[q_ptr].l);
            q_ptr++;
        }

        tab_ptr++;
    }

    // print answers
    for(int i = 0; i < q; i++) {
        cout << ans[i] << '\n';
    }
}