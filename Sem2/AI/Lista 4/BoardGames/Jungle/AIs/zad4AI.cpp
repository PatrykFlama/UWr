#pragma once
#include <bits/stdc++.h>
using namespace std;
#include "Jungle.cpp"

class zad4AI{
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
};