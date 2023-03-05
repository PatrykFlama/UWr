/* #region SUPERFOR */
#define GET_MACRO(_1, _2, _3, _4, NAME, ...) NAME
#define _FOR3(i, a, n, inc) for (auto i = (a); (inc) > 0 ? i < (n) : i >= (n); i += (inc))
#define _FOR2(i, a, n) _FOR3(i, a, n, 1)
#define _FOR1(i, n) _FOR2(i, 0, n)
#define _FOR0(n) _FOR1(i, n)
#define FOR(...) GET_MACRO(__VA_ARGS__, _FOR3, _FOR2, _FOR1, _FOR0)(__VA_ARGS__)    //? (/name/, //from//, to, ///inc///)
#include <bits/stdc++.h>
#define cerr if (debug) cout
using namespace std;
/* #endregion */
const int N = 1e5+5;
vector< pair<string, array<int, 3> > > words;  //? word, index/cost/group
vector<int> groups[N];  //? [group idx][0=min cost / 1..=words idx]


int find(string &s, int n){
    int l = 1, r = n;
    while(l < r){
        int mid = (l+r+1)/2;
        if(words[mid].first.compare(s) > 0){
            r = mid-1;
        } else{
            l = mid;
        }
    }
    return l;
}

int main(){
    int n, k, m; cin >> n >> k >> m;
    
    words.push_back({" ", {0,0,0}});
    FOR(i, 1, n+1){
        string s; cin >> s;
        // add_word(s, i);
        words.push_back({s, {i, 0, 0}});
    }
    FOR(i, 1, n+1){
        int cost; cin >> cost;
        words[i].second[1] = cost;
    }
    FOR(group_idx, 1, k+1){
        int group_size; cin >> group_size;
        groups[group_idx].push_back(INT_MAX);
        FOR(group_size){
            int idx; cin >> idx;
            groups[group_idx].push_back(idx);
            groups[group_idx][0] = min(groups[group_idx][0], words[idx].second[1]);
            words[idx].second[2] = group_idx;
        }
    }

    sort(words.begin(), words.end());

    long long total_cost = 0;
    FOR(m){
        string s; cin >> s;
        int word_index = find(s, n);
        int group_index = words[word_index].second[2];
        int new_cost = groups[group_index][0];

        total_cost += new_cost;
    }

    cout << total_cost << '\n';
}