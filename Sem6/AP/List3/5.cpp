#include <bits/stdc++.h>
using namespace std;

typedef long long ll;


// we can simplify problem so that all point lay either on 1 or 0
// if 2 points have even dist then they lay on the same spot
// otherwise they lay on different spots
// first point can be either on 1 or 0 (wlg)

/*
1 3 0
3 4 1
4 6 0
3 6 1
4 5 0
1 5 0
5 7 1

1 2 3 4 5 6 7
0   0 1 1 1  
*/

constexpr int L = 1e6+5;
int tab[L];

int main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);

    int n, m; cin >> n >> m;
    bool found = false;
    
    // place points on 1 or 2
    int cnt = 0;
    while(m--) {
        int a, b, diff; cin >> a >> b >> diff;
        if(found) continue;

        if(tab[a] == 0) {
            if(tab[b] == 0) {
                tab[a] = tab[b] = (diff ? 1 : 2);       //! it cant be arbitrary here
            } else {
                tab[a] = (diff ? tab[b] : 3 - tab[b]);
            }
        } else {
            if(tab[b] == 0) {
                tab[b] = (diff ? tab[a] : 3 - tab[a]);
            } else if(abs(tab[a] - tab[b]) == diff) {    //! check condition
                continue;
            } else {
                found = true;
            }
        }
        
        cnt++;
    }

    cout << cnt << '\n';
}