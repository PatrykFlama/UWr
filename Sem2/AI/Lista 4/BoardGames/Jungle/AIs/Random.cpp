#pragma once
#include <bits/stdc++.h>
using namespace std;
#include "Jungle.cpp"

class Random{
public: 
    Random(){
        srand(time(NULL));
    }

    pair<int, pair<int, int>> gen_next_move(Jungle* state, int N = 2e4){            // {piece, {dirx, diry}}
        vector<pair<int, pair<int, int>>> legal_moves = state->get_legal_moves();
        return legal_moves[rand() % legal_moves.size()];
    }
};