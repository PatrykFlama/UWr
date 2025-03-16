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

// instead of umap we can first convert all _numbers from tab to [1, n] range and remember for them target cnt
// then simply calculate on the converted array
unordered_map<int, int> _numbers;
int target_cnt[L];
int cnt[L];
int ans = 0;
int currentL = 1, currentR = 0;


void add(int x) {
    cnt[x]++;

    if(cnt[x] == target_cnt[x]) {
        ans++;
    } else if(cnt[x] == target_cnt[x]+1) {
        ans--;
    }
}

void remove(int x) {
    cnt[x]--;

    if(cnt[x] == target_cnt[x]) {
        ans++;
    } else if(cnt[x] == target_cnt[x]-1) {
        ans--;
    }
}


int main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);

    int n, q; cin >> n >> q;
    for(int i = 1; i <= n; i++) {
        // read number
        cin >> tab[i];

        // convert it to [1, n] range if new, remember target cnt
        if(_numbers.find(tab[i]) == _numbers.end()) {
            _numbers[tab[i]] = _numbers.size();
            target_cnt[_numbers[tab[i]]] = tab[i];
        }

        // save the new number to tab
        tab[i] = _numbers[tab[i]];
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

        res[ptr] = ans;
    }

    for(int i = 0; i < q; i++) {
        cout << res[i] << '\n';
    }
}
