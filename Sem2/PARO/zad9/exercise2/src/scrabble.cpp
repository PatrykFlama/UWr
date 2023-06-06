#include "scrabble.hpp"
#include <bits/stdc++.h>

const int letter_values[26] = {1,3,3,2,1,4,2,4,1,8,5,1,3,1,1,3,10,1,1,1,1,4,4,8,4,10};

int calculateScore(std::string s){
    int res = 0;
    for(auto i : s) res += letter_values[i-'A'];
    return res;
}



