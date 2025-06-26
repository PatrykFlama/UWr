#include <bits/stdc++.h>
using namespace std;

#define cerr if(1) cout
#define fst first
#define snd second
typedef long long ll;
typedef pair<int, int> pii;
typedef pair<int, ll> pill;
typedef pair<ll, int> plli;
typedef pair<ll, ll> pll;

constexpr int L = 1e6+2;
constexpr ll MOD = 1e9+7;
constexpr ll EXP = 71;
ll powers[L];

string pattern, text;

int char_to_int(char c) {
    return c - 'a' + 1;
}

void calc_powers() {
    powers[0] = 1;
    for (int i = 1; i < L; i++) {
        powers[i] = (powers[i-1] * EXP) % MOD;
    }
}

ll calc_hash(const string &s, int sz) {
    ll res = 0;
    for (int i = 0; i < sz; i++) {
        res = (((res * EXP) % MOD) + (char_to_int(s[i]))) % MOD;
    }
    return res;
}

ll move_next(const string &s, int ptr, ll h, int pat_len) {
    h = (((h - (powers[pat_len - 1] * (char_to_int(s[ptr]))) % MOD) + MOD) % MOD);
    h = (((h * EXP) % MOD) + (char_to_int(s[ptr + pat_len]))) % MOD;
    return h;
}

int compare(const string &pat, const string &txt, int start) {
    for (int i = 0; i < pat.size(); i++) {
        if (pat[i] != txt[start + i]) return 0;
    }
    return 1;
}

int main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);

    cin >> text >> pattern;

    if (pattern.size() > text.size()) {
        cout << 0 << '\n';
        return 0;
    }

    calc_powers();
    int cnt = 0;
    int pat_len = pattern.size();

    ll hash_patt = calc_hash(pattern, pat_len);
    ll hash_text = calc_hash(text, pat_len);

    if (hash_patt == hash_text) {
        cnt += compare(pattern, text, 0);
    }

    for (int i = 0; i < text.size() - pat_len; i++) {
        hash_text = move_next(text, i, hash_text, pat_len);

        if (hash_patt == hash_text) {
            cnt += compare(pattern, text, i + 1);
        }
    }

    cout << cnt << '\n';
}
