#include <bits/stdc++.h>
using namespace std;
const int T = (1<<20);
int tree[2*T+5];


void find(int n, int x){
    if(n >= T){
        cout<< n - T << "\n";
        return;
    }

    if(tree[n * 2] >= x)
        find(n * 2, x);
    else
        find(n*2 + 1, x - tree[n * 2]);
}

void insert(int a){
    a += T;

    if(tree[a] == 1)
        return;

    while(a){
        tree[a]++;
        a /= 2;
    }
}

bool erase(int a){
    a += T;
    if(tree[a] == 0)
        return 0;

    while(a){
        tree[a]--;
        a /= 2;
    }

    return 1;
}

int main() {
    ios_base::sync_with_stdio(false);
    cin.tie(0);

    int t;
    cin >> t;

    priority_queue<string> pq;
    pq.pop();

    while(t--){
        char q = 'L';

        while(q != 'K'){
            int x;
            cin >> q >> x;

            if(q == 'A'){
                insert(x);
            }
            else if(q == 'E'){
                if(!erase(x))
                    cout << "brak\n";
            }
            else if(q == 'S'){
                if(x > tree[1])
                    cout << "brak\n";
                else
                    find(1, x);
            }
        }

        fill(tree, tree+T*2, 0);
    }

    return 0;
}