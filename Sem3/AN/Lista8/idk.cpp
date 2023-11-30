#include <bits/stdc++.h>
using namespace std;

// find NIFS3
//TODO 

double lambda(int k, vector<double>& x){
    return (x[k] - x[k-1])/(x[k+1] - x[k-1]);
}

double dk(int k, vector<double>& x, vector<double>& y){
    double t1 = (y[k+1] - y[k])/(x[k+1] - x[k]);
    double t2 = (y[k] - y[k-1])/(x[k] - x[k-1]);
    return 6 * (t1 - t2)/(x[k+1] - x[k-1]);
}

// fins M (moments)
vector<double> calc_moments(int n, vector<double>& x, vector<double>& y){
    vector<double> q, p, u, M;
    q.reserve(n), p.reserve(n), u.reserve(n), M.resize(n);
    q.push_back(0), p.push_back(0), u.push_back(0);

    for(int k = 1; k < n; k++){
        double lk = lambda(k, x);
        p.push_back(lk * q[k-1] + 2);
        q.push_back((lk-1)/p[k]);
        u.push_back((dk(k, x, y) - lk * u[k-1])/p[k]);
    }

    M[n-1] = u[n-1];
    for(int k = n-2; k >= 0; k--){
        M[k] = u[k] - q[k] * M[k+1];
    }

    return M;
}

//TODO wzorek z repet