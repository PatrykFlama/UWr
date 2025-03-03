#include <bits/stdc++.h>
using namespace std;

// https://www2.im.uj.edu.pl/LeszekPieniazek/DU/Alg/test-6.html

typedef long long ll;
const int L = 1e6+3;
int phi[L];

void preprocess() { // according to formula
    for(int i = 0; i < L; i++) phi[i] = i;

    for(int i = 2; i < L; i++) {    // find prime numbers
        if(phi[i] == i) {   // works like eratosthenes sieve
            for(int j = i; j <= L; j += i) {
                phi[j] = (ll)phi[j] * (ll)(i - 1) / (ll)i;
            }
        }
    }
}

int main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);

    preprocess();

    int t; cin >> t;
    while (t--) {
        int x; cin >> x;
        cout << phi[x] << '\n';
    }
}