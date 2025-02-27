#include <bits/stdc++.h>
using namespace std;

#define GET_FOR_MACRO(_1, _2, _3, _4, NAME, ...) NAME
#define _FOR3(i, a, n, inc) for(auto i = (a); (inc) > 0 ? i < (n) : i >= (n); i += (inc))
#define _FOR2(i, a, n) _FOR3(i, a, n, 1)
#define _FOR1(i, n) _FOR2(i, 0, n)
#define _FOR0(n) _FOR1(i, n)
#define FOR(...) GET_FOR_MACRO(__VA_ARGS__, _FOR3, _FOR2, _FOR1, _FOR0)(__VA_ARGS__)

typedef long long ll;

constexpr int L = 1e5+3;
int N;
int tab[L];


void quicksort(int l, int r) {
    if(l >= r) return;

    int _l = l, _r = r;
    int pivot = tab[(l+r)/2];
    // int pivot = rand() % (r-l+1) + l;
    
    while(_l <= _r) {
        while(tab[_l] < pivot) _l++;
        while(tab[_r] > pivot) _r--;
        if(_l <= _r) {
            swap(tab[_l], tab[_r]);
            _l++;
            _r--;
        }
    }
    quicksort(l, _r);
    quicksort(_l, r);
}


int main() {
    // srand(time(0));
    ios_base::sync_with_stdio(0);
    cin.tie(0);

    cin >> N;
    FOR(i, N) {
        cin >> tab[i];
    }

    quicksort(0, N-1);

    FOR(i, N) {
        cout << tab[i] << ' ';
    }
    cout << '\n';
}