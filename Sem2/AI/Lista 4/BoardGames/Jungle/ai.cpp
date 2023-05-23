#include <bits/stdc++.h>
using namespace std;
#include "AIs/Jungle.cpp"
#include "AIs/Random.cpp"
#include "AIs/zad3AI.cpp"
#include "AIs/AlphaBeta.cpp"
#include "AIs/MCTS.cpp"

void validator_loop();      // yeah, well, that ain't gonna fly here
void game_loop();


int main() {
    game_loop();
}


void game_loop(){
    const bool display_res = true;
    const bool display_res_board = false;
    const bool debug = false;
    srand(time(NULL));
    Jungle game;
    zad3AI ai0;
    MCTS ai1;
    int MAX_GAMES = 100;        // games to be played
    int MAX_TURNS = 500;        // maximum turns before game is considered a draw
    int win_counter[2] = {0, 0};

    ai1.run_mcts_for(4*1000, &game);

    while(MAX_GAMES--){
        int turn = rand()%2;
        game.reset();
        if(turn) game.swap_players();
        int turns = 0;

        if(debug) cerr << "Begins ai" << turn << '\n' << game << "\n\n";
        while(not game.game_won()){
            turns++;
            if(!turn){
                auto move = ai0.gen_next_move(&game);
                if(debug) cerr << "ai0_low move: " << AnimalNames[move.first] << ' ' << move.second.first << ' ' << move.second.second << ' '; 
                auto [myxs, myys] = game.pieces[game.player][move.first];
                if(debug) cerr << "(" << myxs << ' ' << myys << ' ' << myxs+move.second.first << ' ' << myys+move.second.second << ")\n";

                game.execute_move(move);
            } else{
                auto move = ai1.gen_next_move(&game, 500);
                if(debug) cerr << "ai1_cap move: " << AnimalNames[move.first] << ' ' << move.second.first << ' ' << move.second.second << ' '; 
                auto [myxs, myys] = game.pieces[game.player][move.first];
                if(debug) cerr << "(" << myxs << ' ' << myys << ' ' << myxs+move.second.first << ' ' << myys+move.second.second << ")\n";

                game.execute_move(move);
            }
            if(debug) cerr << game << '\n';
            turn = 1-turn;

            if(turns > MAX_TURNS) break;
        }
        if(game.get_legal_moves().size() == 0 || turns > MAX_TURNS){
            if(display_res) cout << MAX_GAMES << ": draw in " << turns << " turns\n";
            if(display_res_board) cout << game << '\n';
            continue;
        }
        win_counter[1-turn]++;
        if(display_res) cout << MAX_GAMES << ": ai" << 1-turn << " won in " << turns << " turns\n";
        if(display_res_board) cout << game << '\n';
        if(debug) cerr << "---------------------------------\n\n";
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
    #define cerr if(true) cerr
    Jungle game;    // defaults to starting at the bottom of board
    MCTS ai;
    #define MCTS_TIME 1*1000        // in milliseconds
    // game.swap_players();
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

            #ifdef MCTS_TIME
            auto [piece, dir] = ai.gen_next_move(&game, MCTS_TIME);
            #else
            auto [piece, dir] = ai.gen_next_move(&game);
            #endif

            auto [myxs, myys] = game.pieces[game.player][piece];
            ido(myxs, myys, myxs+dir.first, myys+dir.second);
            cerr << "AI move: " << AnimalNames[piece] << ' ' << dir.first << ' ' << dir.second << '\n';
            // for(auto [piece, dir] : game.get_legal_moves()){
            //     cerr << AnimalNames[piece] << ' ' << dir.first << ' ' << dir.second << '\n';
            // }
            // cerr << '\n';
            game.execute_move(piece, dir);
        } else if(cmd == "UGO"){
            // game.swap_players();
            double time_for_move, time_for_game; 
            cin >> time_for_move >> time_for_game;

            #ifdef MCTS_TIME
            auto [piece, dir] = ai.gen_next_move(&game, MCTS_TIME);
            #else
            auto [piece, dir] = ai.gen_next_move(&game);
            #endif
            
            auto [myxs, myys] = game.pieces[game.player][piece];
            ido(myxs, myys, myxs+dir.first, myys+dir.second);
            cerr << "AI move: " << AnimalNames[piece] << ' ' << dir.first << ' ' << dir.second << '\n';
            game.execute_move(piece, dir);
        } else if(cmd == "ONEMORE"){
            game.reset();
            // game.swap_players();
            rdy();
        } else if(cmd == "BYE"){
            break;
        }
    }
}

