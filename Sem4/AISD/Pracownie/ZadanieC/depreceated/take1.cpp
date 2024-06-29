#include <bits/stdc++.h>
using namespace std;
#define pii pair<int, int>
const int MAX_WEIGHT = 1e6 + 1;
const int MAX_NOMINALS = 101;
int weight;
int number_of_nominals;
pii nominals[MAX_NOMINALS];   //? {value, weight}           //* 4*2*100 = 800B
int dp[MAX_WEIGHT][2];        //? {biggest, smallest} value //* 2*4*1e6 = 8MB
int parent[MAX_WEIGHT][2];                                  //* 2*4*1e6 = 8MB
int cnt_reconstruct[MAX_NOMINALS][2];                       //* 4*100*2 = 800B
// TODO optimize same nominals/weights?


void solve() {      //* O(n*w) = 1e8
    for (int w = 0; w < weight; w++) {
        if(w > 0 && dp[w][0] == 0) continue;
        for (int c = 0; c < number_of_nominals; c++) {
            const int ptr = w + nominals[c].second;
            if (ptr <= weight) {
                if (dp[ptr][0] < dp[w][0] + nominals[c].first) {
                    dp[ptr][0] = dp[w][0] + nominals[c].first;
                    parent[ptr][0] = c;
                }
                if (dp[ptr][1] > dp[w][1] + nominals[c].first) {
                    dp[ptr][1] = dp[w][1] + nominals[c].first;
                    parent[ptr][1] = c;
                }
            }
        }
    }
}

void reconstruct(int path) {
    int ptr = weight;
    while (ptr > 0) {
        ++cnt_reconstruct[nominals[parent[ptr][path]].first][path];
        ptr -= nominals[parent[ptr][path]].second;
    }
}


int main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);

    cin >> weight >> number_of_nominals;
    for (int i = 0; i < number_of_nominals; i++) {
        cin >> nominals[i].first >> nominals[i].second;
    }

    for (int i = 1; i <= weight; i++)
        dp[i][1] = INT_MAX;

    solve();

    if(dp[weight][0] == 0) {
        cout << "NIE\n";
        return 0;
    }
    cout << "TAK\n";

    reconstruct(0);
    reconstruct(1);

    cout << dp[weight][1] << "\n";
    for(int i = 0; i < number_of_nominals; i++) {
        cout << cnt_reconstruct[nominals[i].first][1] << " ";
    }
    cout << "\n";

    cout << dp[weight][0] << "\n";
    for(int i = 0; i < number_of_nominals; i++) {
        cout << cnt_reconstruct[nominals[i].first][0] << " ";
    }
    cout << "\n";
}
