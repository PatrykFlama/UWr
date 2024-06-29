#include <bits/stdc++.h>
using namespace std;
#define ll long long
#define pii pair<int, int>
#define pll pair<ll, ll>
#define maximize(a, b) a = max(a, b)
#define minimize(a, b) a = min(a, b)
#define value first
#define weight second
const int MAX_C = 100 + 5, MAX_F = 1e6 + 5;
int F;      //? max weight
int C;      //? number of coins
pii coins[MAX_C];       //? coins[i] = {value, weight}
pll dp[MAX_F];          //? dp[i] = {max value, min value}
pii cnt[MAX_C];         //? reconstruct[i] = number of i-th coins taken for {max value, min value}
//* space complexity: [MAX_F * 2 + 2 * MAX_C * 2 = 2e6 + 400] * 4B ~ 8MB


void solve() {
    for (int f = 0; f <= F; f++) {
        for (int c = 0; c < C; c++) {
            // if there exists state from which we can reach f and this state was reachable
            if (f >= coins[c].weight && dp[f - coins[c].weight].first != -1) {
                maximize(dp[f].first,  dp[f - coins[c].weight].value + (ll)coins[c].value);
                minimize(dp[f].second, dp[f - coins[c].weight].weight + (ll)coins[c].value);
            }
        }
    }
}

void reconstruct() {
    // max
    int f = F;
    while (f > 0) {
        for (int c = 0; c < C; c++) {
            int best_c = -1;
            if (f >= coins[c].weight && dp[f - coins[c].weight].first != -1 && 
                dp[f].first == dp[f - coins[c].weight].first + (ll)coins[c].value) {
                if(best_c == -1) best_c = c;
                else best_c = coins[c].value > coins[best_c].value ? c : best_c;
            }
            cnt[best_c].first++;
            f -= coins[best_c].weight;
        }
    }

    // min
    f = F;
    while (f > 0) {
        for (int c = 0; c < C; c++) {
            int best_c = -1;
            if (f >= coins[c].weight && dp[f - coins[c].weight].second != INT_MAX && 
                dp[f].second == dp[f - coins[c].weight].second + (ll)coins[c].value) {
                if (best_c == -1) best_c = c;
                else best_c = coins[c].value < coins[best_c].value ? c : best_c;
            }
            cnt[best_c].second++;
            f -= coins[best_c].weight;
        }
    }
}


int32_t main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);

    cin >> F >> C;
    fill(dp+1, dp + F + 1, pii(-1, INT_MAX));

    for (int i = 0; i < C; i++) {
        cin >> coins[i].value >> coins[i].weight;
    }

    solve();

    if (dp[F].first == -1) {
        cout << "NIE\n";
    } else {
        cout << "TAK\n";
        reconstruct();

        // min
        cout << dp[F].second << '\n';
        for (int i = 0; i < C; i++) {
            cout << cnt[i].second << ' ';
        }
        cout << '\n';
        
        // max
        cout << dp[F].first << '\n';
        for (int i = 0; i < C; i++) {
            cout << cnt[i].first << ' ';
        }
        cout << '\n';
    }
}
