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


constexpr int L = 2e6+4;
int pi[L];
int pat_n = 0;

int calc_pref(string &s) {
    for (auto c : s) cerr << c << " ";
    cerr << '\n';
    cerr << "0 ";

    int cnt = 0;

    for (int i = 1; i < s.size(); i++) {
        int ptr = pi[i - 1];

        while (ptr > 0 && s[i] != s[ptr]) {
            ptr = pi[ptr - 1];
        }

        if (s[i] == s[ptr]) {
            ptr++;
        }

        pi[i] = ptr;
        if (i > pat_n && ptr == pat_n) {
            cnt++;
        }

        cerr << ptr << ' ';
    }
    cerr << '\n';

    return cnt;
}


int main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);

    string t, p; cin >> t >> p;
    string s = p + "#" + t;
    pat_n = p.size();

    int res = calc_pref(s);
    cout << res << '\n';
}
