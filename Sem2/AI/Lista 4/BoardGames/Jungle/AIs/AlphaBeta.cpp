#pragma once
#include <bits/stdc++.h>
using namespace std;
#include "Jungle.cpp"
int blob =0;

class AlphaBeta{
    int main_player;
    const int  MAX_DEPTH = 3;
    const int  MAX_PLAYER = 1, MIN_PLAYER = 0;
public:
    /* #region */
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
            int score = alphabeta(&next_state, MAX_DEPTH, MIN_PLAYER, INT_MIN, INT_MAX);
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
            if(state->terminal(legal_moves)) return state->result(main_player);
            else return alphabeta(state, depth - 1, 1 - player, alpha, beta);
        }

        if(depth <= 0) return state->heuristic_result(main_player);

        int best_score = (player == MAX_PLAYER ? INT_MIN : INT_MAX);
        for(auto [piece, dir] : legal_moves){
            Jungle next_state = state->gen_next_state(piece, dir);
            int score = alphabeta(&next_state, depth - 1, 1 - player, alpha, beta);

            if(player == MIN_PLAYER){
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
    /* #endregion */


/* #region *//*
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
            score += reinforced_random(&temp, N);
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
        if(state->terminal(starting_legal_moves)) return state->heuristic_result(main_player)*max_games;

        Jungle here = *state;
        int total_res = 0, games_played = 0;
        vector<pair<int, pair<int, int>>> legal_moves;
        while(games_played < max_games){
            legal_moves = here.get_legal_moves();
            if(here.terminal(legal_moves)) {
                total_res += here.heuristic_result(main_player);
                ++games_played;
                here = *state;      // copy 
                legal_moves = starting_legal_moves;
            }

            auto [piece, dir] = legal_moves[rand() % legal_moves.size()];
            here.execute_move(piece, dir);
        }

        // cerr << "Games played: " << games_played << " res: " << rate(total_res, games_played) << '\n';
        return total_res;
    }

    int reinforced_random(Jungle* state, int max_games){
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

            int best_piece;
            pair<int, int> best_dir;
            int max_score = INT_MIN;
            int heur = heuristics(&here);
            for(auto [piece, dir] : legal_moves){
                pair<int, int> pos = here.pieces[here.player][piece];
                int score = heur - dist(pos.first, pos.second, (here.player ? upper_den : lower_den)) +
                                   dist(pos.first + dir.first, pos.second + dir.second, 
                                                  (here.player ? upper_den : lower_den));
                if(score > max_score){
                    max_score = score;
                    best_piece = piece;
                    best_dir = dir;
                }
            }
            here.execute_move(best_piece, best_dir);
        }

        // cerr << "Games played: " << games_played << " res: " << total_res << '\n';
        return total_res;
    }

    int heuristics(Jungle *state){
        int res = 0;
        for(auto [x, y] : state->pieces[main_player]){
            res += dist(x, y, (state->player ? upper_den : lower_den));
        }
        return res;
    }

    inline int dist(int x, int y, pair<int, int> den){
        return sqrt((x+den.first)*(x+den.first) + (y+den.second)*(y+den.second));
    }
*//* #endregion */
};
