// version: O(k*l^2) we keep first and last dp to calculate next
#include <bits/stdc++.h>
using namespace std;
// #define DEBUG
const int K = 1e2+1, L = 1e3+1;
int dp[K][L];   //? dp[i][j] - best score for i buttons whre last button is from j
int suff[L];


int32_t main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);

    // read data
    int k, l;
    cin >> k >> l;
    for(int i = 0; i < l; i++) {
        cin >> dp[0][i];
    }

    // calc suffix sum
    suff[l-1] = dp[0][l-1];
    for(int i = l-2; i >= 0; i--) {
        suff[i] = suff[i+1] + dp[0][i];
    }

    // dp base
    int base_val = 0;
    for(int i = 0; i < l; i++) {
        base_val += dp[0][i] * (i+1);
    }
    for(int i = 0; i < l; i++) {
        dp[1][i] = base_val;
    }

    // calc dp
    for(int i = 2; i <= k; i++) {
        for(int j = i-1; j < l; j++) {
            int min_val = INT_MAX;
            for(int x = i-2; x < j; x++) {
                min_val = min(min_val, dp[i-1][x] - suff[j] * (j-x));
            }
            
            dp[i][j] = min_val;
        }
    }

    // debug
    #ifdef DEBUG
    cout << "Suff:\n";
    for(int i = 0; i < l; i++) {
        cout << suff[i] << ' ';
    }
    cout << "\nDP:\n";
    for(int i = 0; i <= k; i++) {
        for(int j = 0; j < l; j++) {
            if(dp[i][j] < 10)
                cout << dp[i][j] << "  ";
            else
                cout << dp[i][j] << ' ';
        }
        cout << '\n';
    }
    cout << "\nResult:\n";
    #endif

    // get result
    int res = INT_MAX, ptr;
    for(int i = k-1; i < l; i++) {
        if(dp[k][i] < res) {
            res = dp[k][i];
            ptr = i;
        }
    }

    // backtracking
    stack<int> res_q;
    while(k > 0) {
        res_q.push(l - ptr);
        // for(int i = ptr-1; i >= 0; i--) {
        for(int i = 0; i < ptr; i++) {
            if(dp[k][ptr] == dp[k-1][i] - suff[ptr] * (ptr-i)) {
                l = ptr;
                ptr = i;
                break;
            }
        }
        k--;
    }

    // print result
    cout << res << '\n';
    while(!res_q.empty()) {
        cout << res_q.top() << ' ';
        res_q.pop();
    }
}
