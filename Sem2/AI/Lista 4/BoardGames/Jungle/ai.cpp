#include <bits/stdc++.h>
using namespace std;
#include "AIs/Jungle.cpp"
#include "AIs/Random.cpp"
#include "AIs/zad3AI.cpp"
#include "AIs/AlphaBeta.cpp"
#include "AIs/MCTS.cpp"

void validator_loop();
void game_loop();


int main() {
    game_loop();
}


void game_loop(){
    const bool debug = false;
    const bool display = true;
    srand(time(NULL));
    Jungle game;
    zad3AI ai0;
    AlphaBeta ai1;
    int MAX_GAMES = 10;
    int win_counter[2] = {0, 0};

    while(MAX_GAMES--){
        int turn = rand()%2;
        game.reset();
        if(turn) game.swap_players();
        int turns = 0;

        if(debug) cout << "Begins ai" << turn << '\n' << game << "\n\n";
        while(not game.game_won()){
            turns++;
            if(!turn){
                auto move = ai0.gen_next_move(&game);
                if(debug) cout << "ai0_low move: " << AnimalNames[move.first] << ' ' << move.second.first << ' ' << move.second.second << ' '; 
                auto [myxs, myys] = game.pieces[game.player][move.first];
                if(debug) cout << "(" << myxs << ' ' << myys << ' ' << myxs+move.second.first << ' ' << myys+move.second.second << ")\n";

                game.execute_move(move);
            } else{
                auto move = ai1.gen_next_move(&game);
                if(debug) cout << "ai1_cap move: " << AnimalNames[move.first] << ' ' << move.second.first << ' ' << move.second.second << ' '; 
                auto [myxs, myys] = game.pieces[game.player][move.first];
                if(debug) cout << "(" << myxs << ' ' << myys << ' ' << myxs+move.second.first << ' ' << myys+move.second.second << ")\n";

                game.execute_move(move);
            }
            if(debug) cout << game << '\n';
            turn = 1-turn;
        }
        if(game.get_legal_moves().size() == 0){
            if(display) cout << "draw in " << turns << "turns\n" << game << '\n';
            continue;
        }
        win_counter[1-turn]++;
        if(display) cout << "ai" << 1-turn << " won in " << turns << "turns\n" << game << '\n';
        if(debug) cout << "---------------------------------\n\n";
    }

    cout << "ai0 wins: " << win_counter[0] << '\n';
    cout << "ai1 wins: " << win_counter[1] << '\n';
}



void rdy(){
    printf("RDY\n");
    fflush(stdout);
}
void ido(int xs, int ys, int xd, int yd){
    printf("IDO %d %d %d %d\n", xs, ys, xd, yd);
    fflush(stdout);
}

void validator_loop(){
    Jungle game;    // defaults to starting at the bottom of board
    AlphaBeta ai;
    rdy();

    while(true){
        string cmd; cin >> cmd;

        if(cmd == "HEDID"){
            double time_for_move, time_for_game; 
            cin >> time_for_move >> time_for_game;

            int xs, ys, xd, yd;
            cin >> xs >> ys >> xd >> yd;
            if(xs == -1) continue;      // opponent passed
            game.execute_move(game.get_piece_type(xs, ys), {xd, yd});

            auto [piece, dir] = ai.gen_next_move(&game);
            auto [myxs, myys] = game.pieces[game.player][piece];
            ido(myxs, myys, myxs+dir.first, myys+dir.second);
            cerr << "AI move: " << AnimalNames[piece] << ' ' << dir.first << ' ' << dir.second << '\n';
            for(auto [piece, dir] : game.get_legal_moves()){
                cerr << AnimalNames[piece] << ' ' << dir.first << ' ' << dir.second << '\n';
            }
            cerr << '\n';
            game.execute_move(piece, dir);
        } else if(cmd == "UGO"){
            double time_for_move, time_for_game; 
            cin >> time_for_move >> time_for_game;

            auto [piece, dir] = ai.gen_next_move(&game);
            auto [myxs, myys] = game.pieces[game.player][piece];
            ido(myxs, myys, myxs+dir.first, myys+dir.second);
            cerr << "AI move: " << AnimalNames[piece] << ' ' << dir.first << ' ' << dir.second << '\n';
            game.execute_move(piece, dir);
        } else if(cmd == "ONEMORE"){
            game.reset();
            rdy();
        } else if(cmd == "BYE"){
            break;
        }
    }
}

