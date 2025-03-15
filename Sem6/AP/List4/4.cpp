#include <bits/stdc++.h>
using namespace std;

#define cerr if(0) cout
#define fst first
#define snd second
typedef long long ll;
typedef pair<int, int> pii;

//! https://web.archive.org/web/20221206004910/https://blog.anudeep2011.com/mos-algorithm/

constexpr int L = 1e5+5;
int tab[L];
pii queries[L];


unordered_map<int, int> cnt;    //TODO change to array
int ans = 0;
int currentL = 1, currentR = 0;


void add(int x) {
    cnt[x]++;

    if(cnt[x] == x) {
        ans++;
    } else if(cnt[x] == x+1) {
        ans--;
    }
}

void remove(int x) {
    cnt[x]--;

    if(cnt[x] == x) {
        ans++;
    } else if(cnt[x] == x-1) {
        ans--;
    }
}


int main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);

    int n, q; cin >> n >> q;
    for(int i = 1; i <= n; i++) {
        cin >> tab[i];
    }

    for(int i = 0; i < q; i++) {
        cin >> queries[i].fst >> queries[i].snd;
    }


    // sort queries: tab is divided into blocks, sort by query block number in the first place, then by query end
    const int block_size = ceil(sqrt(n));
    sort(queries, queries+q, [&](pii a, pii b) {
        if(a.fst/block_size != b.fst/block_size) return a.fst < b.fst;
        return a.snd < b.snd;
    });

    for(int i = 0; i < q; i++) {
        const auto [queryL, queryR] = queries[i];
        cerr << "query " << i << " [" << queryL << ", " << queryR << "]\n";

        
        while(currentL < queryL) {
            remove(tab[currentL]);
            currentL++;
        }
        while(currentL > queryL) {
            currentL--;
            add(tab[currentL]);
        }
        while(currentR < queryR) {
            currentR++;
            add(tab[currentR]);
        }
        while(currentR > queryR) {
            remove(tab[currentR]);
            currentR--;
        }
        
        for(auto [a, b] : cnt) {
            cerr << a << ' ' << b << '\n';
        }


        cout << ans << '\n';
    }
}