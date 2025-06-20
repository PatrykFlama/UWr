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
int pi[L];


int calc_pi(string &t, string &p, bool save_pi = false) {
    int pattern_fit = 0;
    int sub_ptr = 0;

    int pattern_length = p.size() - 1;
    for (int ptr = 0; ptr < t.size(); ptr++) {
        while (sub_ptr > 0 && p[sub_ptr + 1] != t[ptr]) {
            sub_ptr = pi[sub_ptr];
        }

        if (p[sub_ptr + 1] == t[ptr]) {
            sub_ptr++;
        }

        if (save_pi) {
            pi[ptr + 1] = sub_ptr;
        }

        if (!save_pi && sub_ptr == pattern_length) {
            pattern_fit++;
            sub_ptr = pi[sub_ptr];
        }
    }

    return pattern_fit;
}



int main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);

    string t, p; cin >> t >> p;
    p = " " + p;

    calc_pi(p, p, true);
    int res = calc_pi(t, p);
    cout << res << '\n';
}