// #pragma GCC optimize("Ofast","unroll-loops","omit-frame-pointer","inline")  //Optimization flags
// #pragma GCC option("march=native","tune=native","no-zero-upper")            //Enable AVX
// #pragma GCC target("avx2")                                                  //Enable AVX
// #include <x86intrin.h>                                                      //AVX/SSE Extensions

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
pair<int, pii> queries[L];
int res[L];

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
        cin >> queries[i].snd.fst >> queries[i].snd.snd;
        queries[i].fst = i;
    }


    // sort queries: tab is divided into blocks, sort by query block number in the first place, then by query end
    const int block_size = ceil(sqrt(n));
    sort(queries, queries+q, [&](pair<int, pii> a, pair<int, pii> b) {
        if(a.snd.fst/block_size != b.snd.fst/block_size) return a.snd.fst < b.snd.fst;
        return a.snd.snd < b.snd.snd;
    });

    for(int i = 0; i < q; i++) {
        const int ptr = queries[i].fst;
        const auto [queryL, queryR] = queries[i].snd;
        cerr << "query " << i << " [" << queryL << ", " << queryR << "]\n";
        
        while(currentL > queryL) {
            currentL--;
            add(tab[currentL]);
        }
        while(currentR < queryR) {
            currentR++;
            add(tab[currentR]);
        }
        while(currentL < queryL) {
            remove(tab[currentL]);
            currentL++;
        }
        while(currentR > queryR) {
            remove(tab[currentR]);
            currentR--;
        }
        
        for(auto [a, b] : cnt) {
            cerr << a << ' ' << b << '\n';
        }

        res[ptr] = ans;
    }

    for(int i = 0; i < q; i++) {
        cout << res[i] << '\n';
    }
}
