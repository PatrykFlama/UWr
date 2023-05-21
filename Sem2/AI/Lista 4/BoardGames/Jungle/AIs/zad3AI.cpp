#pragma once
#include <bits/stdc++.h>
using namespace std;
#include "Jungle.cpp"


class zad3AI{
    int main_player;
public:
    pair<int, pair<int, int>> gen_next_move(Jungle* state, int N = 3){            // {piece, {dirx, diry}}
        main_player = state->player;
        vector<pair<int, pair<int, int>>> legal_moves = state->get_legal_moves();
        if(legal_moves.size() == 0) return {0, {0, 0}};

        pair<int, pair<int, int>> best_move;
        int max_score = INT_MIN;

        // cerr << "Looking thru moves: ";
        for(auto [piece, dir] : legal_moves){
            Jungle temp = state->gen_next_state(piece, dir);
            int score = random(&temp, N);
            if(score > max_score){
                max_score = score;
                best_move = {piece, dir};
            }
            // cerr << score << ' ';
        }
        // cerr << '\n';

        return best_move;
    }

    inline int rate(int total_res, int games_played){
        return (games_played == 0 ? total_res : 100*((double)total_res)/games_played);
    }

    int random(Jungle* state, int max_games){
        vector<pair<int, pair<int, int>>> starting_legal_moves = state->get_legal_moves();
        if(state->terminal(starting_legal_moves)) return state->result(main_player)*max_games;

        Jungle here = *state;
        int total_res = 0, games_played = 0;
        vector<pair<int, pair<int, int>>> legal_moves;
        while(games_played < max_games){
            legal_moves = here.get_legal_moves();
            if(here.terminal(legal_moves)) {
                total_res += here.result(main_player);
                ++games_played;
                here = *state;      // copy 
                legal_moves = starting_legal_moves;
            }

            auto [piece, dir] = legal_moves[rand() % legal_moves.size()];
            here.execute_move(piece, dir);
        }

        return total_res;
    }
};
