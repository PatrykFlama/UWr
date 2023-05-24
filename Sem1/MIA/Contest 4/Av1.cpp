/* #region SUPERFOR */
#define GET_MACRO(_1, _2, _3, _4, NAME, ...) NAME
#define _FOR3(i, a, n, inc) for (auto i = (a); (inc) > 0 ? i < (n) : i >= (n); i += (inc))
#define _FOR2(i, a, n) _FOR3(i, a, n, 1)
#define _FOR1(i, n) _FOR2(i, 0, n)
#define _FOR0(n) _FOR1(i, n)
#define FOR(...) GET_MACRO(__VA_ARGS__, _FOR3, _FOR2, _FOR1, _FOR0)(__VA_ARGS__)    //? (/name/, //from//, to, ///inc///)
#include <bits/stdc++.h>
using namespace std;
/* #endregion */
#define cerr if(1) cout

const int L = 2e5+5;
int strength[L];    //? prefix sum strength table, str[0] = 0
int ptr = 1;        //? pointer for strength table <= n, so we are standing on ptr soldier
int n;              //? amt of soldiers
int act_sold_health;


void calc_hit(int hit){
    cerr << "Hit: " << hit << ' ';
    if(hit < act_sold_health){
        cerr << "< soldier health: " << act_sold_health << '\n';
        act_sold_health -= hit;
        return;
    }
    hit -= act_sold_health, ptr++;
    if(ptr > n){
        cerr << "no more soldiers to kill: " << ptr << ' ' << n << '\n';
        ptr = 1;
        act_sold_health = strength[ptr];
        return;
    }
    cerr << "New hit: " << hit << ' ';

    int l = ptr, r = n;         // binsearch hit soldiers
    while(l < r){
        int mid = (l+r+1)/2;
        
        if(strength[mid]-strength[ptr-1] > hit){        // strengths in (ptr, mid> 
            r = mid-1;
        } else{
            l = mid;
        }
    }

    int left_hp = hit - (strength[l-1] - strength[ptr-1]);
    act_sold_health = strength[l] - strength[l-1] - left_hp;
    cerr << "found soldier from " << ptr << " to " << l << ", the difference is " << strength[l] - strength[ptr-1] << " which leaves him " << act_sold_health << ".\n";

    ptr = l;
    if(act_sold_health < 0){
        ptr = 1;
        act_sold_health = strength[1];
    }
    if(act_sold_health == 0){
        ptr++;
        act_sold_health = strength[ptr];
    }

    return;
}

int main(){
    int q; cin >> n >> q;
    
    FOR(n){     // create prefix sum table for strength
        int temp; cin >> temp;
        strength[i+1] = strength[i] + temp;
    }
    act_sold_health = strength[ptr];

    FOR(q){
        int hit; cin >> hit;
        calc_hit(hit);
        cout << n-ptr+1 << '\n';
    }
}