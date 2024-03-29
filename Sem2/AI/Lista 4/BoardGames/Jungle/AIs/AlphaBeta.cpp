#pragma once
#include <bits/stdc++.h>
using namespace std;
#include "Jungle.cpp"

class AlphaBeta{
    int main_player;
    const int  MAX_DEPTH = 3;
public:
    pair<int, pair<int, int>> gen_next_move(Jungle *state){
        main_player = state->player;
        return alphabeta_root(state);
    }

    pair<int, pair<int, int>> alphabeta_root(Jungle *state){        // returns best move
        int best_score = INT_MIN;
        pair<int, pair<int, int>> best_move = {-1, {-1, -1}};
        vector<pair<int, pair<int, int>>> legal_moves = state->get_legal_moves();

        for(auto [piece, dir] : legal_moves){
            Jungle next_state = state->gen_next_state(piece, dir);
            int score = alphabeta(&next_state, MAX_DEPTH, 1-main_player, INT_MIN, INT_MAX);
            // cout << AnimalNames[piece] << ' ' << dir.first << ' ' << dir.second << ' ' << score << '\n';
            if(score > best_score){
                best_score = score;
                best_move = {piece, dir};
            }
        }

        return best_move;
    }

    int alphabeta(Jungle *state, int depth, int player, int alpha, int beta){
        vector<pair<int, pair<int, int>>> legal_moves = state->get_legal_moves();

        if(state->terminal(legal_moves)){
            state->swap_players();
            legal_moves = state->get_legal_moves();
            if(state->terminal(legal_moves)) return state->heuristic_result(main_player);
            else return alphabeta(state, depth - 1, 1 - player, alpha, beta);
        }

        if(depth <= 0) return state->heuristic_result(main_player);

        int best_score = (player == main_player ? INT_MIN : INT_MAX);
        for(auto [piece, dir] : legal_moves){       // that could be optimized by moving player type check outside of the loop
            Jungle next_state = state->gen_next_state(piece, dir);
            int score = alphabeta(&next_state, depth - 1, 1 - player, alpha, beta);

            if(player != main_player){
                best_score = min(best_score, score);
                beta = min(beta, score);
            }
            else{
                best_score = max(best_score, score);
                alpha = max(alpha, score);
            }

            if(alpha >= beta) break;
        }

        return best_score;
    }
};
