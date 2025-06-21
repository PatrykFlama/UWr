#include <bits/stdc++.h>
using namespace std;

#define cerr if(0) cout
#define fst first
#define snd second
typedef long long ll;
typedef pair<int, int> pii;
typedef pair<int, ll> pill;
typedef pair<ll, int> plli;
typedef pair<ll, ll> pll;


constexpr int L = 1e6+4;
int pi[L];

void calc_pref(string &s) {
    for (int i = 1; i < s.size(); i++) {
        int ptr = pi[i - 1];

        while (ptr > 0 && s[i] != s[ptr]) {
            ptr = pi[ptr - 1];
        }

        if (s[i] == s[ptr]) {
            ptr++;
        }

        pi[i] = ptr;
    }
}


int main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);

    string s; cin >> s;

    calc_pref(s);

    vector<int> res;
    int ptr = pi[s.size()-1];
    while (ptr > 0) {
        res.push_back(ptr);
        ptr = pi[ptr - 1];
    }

    for (int i = res.size()-1; i >= 0; i--) cout << res[i] << ' ';
    cout << '\n';
}

