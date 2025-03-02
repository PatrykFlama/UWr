#include <bits/stdc++.h>
using namespace std;

typedef long long ll;

const int L = 1e6+2;
vector<int> primes;

void gen_primes() {
    primes.reserve(L);
    vector<bool> is_prime(L, true);
    is_prime[0] = is_prime[1] = false;

    for(int i = 2; i * i < L; i++) {
        if(is_prime[i]) {
            primes.push_back(i);

            for(int j = i * i; j < L; j += i) {
                is_prime[j] = false;
            }
        }
    }
}

int main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);

    gen_primes();

    int t; cin >> t;
    while(t--) {
        int a; cin >> a;

        int fact = 1;
        for(int p : primes) {
            if(p > a) break;
            if(a % p != 0) continue;

            int cnt = 0;
            while(a % p == 0) {
                a /= p;
                cnt++;
            }

            fact *= (cnt + 1);
        }

        cout << fact << '\n';
    }
}