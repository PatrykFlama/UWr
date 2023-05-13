#include <bits/stdc++.h>
using namespace std;

/*
black player starts
starting sheme:
...WB...
...BW...
*/

class Reversi{
    const int DIRS[8][2] = {{-1, -1}, {-1, 0}, {-1, 1}, {0, 1},
                            {1, 1},   {1, 0},  {1, -1}, {0, -1}};

public:
    bool player[8][8];          // active player player's pieces
    bool opponent[8][8];        // opponent's pieces

    Reversi(){}
    Reversi(bool black_starts){
        reset(black_starts);
    }

    void reset(bool black_starts){
        memset(player, 0, sizeof(player));
        memset(opponent, 0, sizeof(opponent));
        if(black_starts){
            opponent[3][3] = opponent[4][4] = true;
            player[3][4] = player[4][3] = true;   
        } else{
            player[3][3] = player[4][4] = true;
            opponent[3][4] = opponent[4][3] = true;   
        }
    }

    /* #region //* setters getters */
    inline void set_player_cell(int x, int y, bool val){
        player[x][y] = val;
    }
    inline bool get_player_cell(int x, int y){
        return player[x][y];
    }
    inline void set_opponent_cell(int x, int y, bool val){
        opponent[x][y] = val;
    }
    inline bool get_opponent_cell(int x, int y){
        return opponent[x][y];
    }
    /* #endregion */

    void swap_players(){
        swap(player, opponent);
    }

    void make_move(int x, int y){
        set_player_cell(x, y, true);
        for(auto [dx, dy] : DIRS){
            if(can_beat(x, y, {dx, dy})){
                int tx = x + dx, ty = y + dy;
                while(get_opponent_cell(tx, ty)){
                    set_opponent_cell(tx, ty, false);
                    set_player_cell(tx, ty, true);
                    tx += dx;
                    ty += dy;
                }
            }
        }
    }

    inline bool safe(int x, int y){
        return x >= 0 && x < 8 && y >= 0 && y < 8;
    } 

    bool can_beat(int x, int y, pair<int, int> dir){
        int dx = dir.first, dy = dir.second;
        x += dx, y += dy;

        if(!safe(x, y) || !get_opponent_cell(x, y)) return false;
        while(safe(x, y) && get_opponent_cell(x, y)){
            x += dx;
            y += dy;
        }

        if(safe(x, y) && get_player_cell(x, y)) return true;
        return false;
    }

    vector<pair<int, int>> free_cells(){
        // long long occupied_cells = (player | opponent);
        vector<pair<int, int>> res;

        for(int x = 0; x < 8; x++){
            for(int y = 0; y < 8; y++){
                if(get_player_cell(x, y) || get_opponent_cell(x, y)) continue;
                for(auto [dx, dy] : DIRS){
                    if(can_beat(x, y, {dx, dy})){
                        res.push_back({x, y});
                        break;
                    }
                }
            }
        }

        return res;
    }

    int result(){
        int res = 0;
        for(int i = 0; i < 8; i++) for(int j = 0; j < 8; j++) {
            if(player[i][j]) res++;
            else if(opponent[i][j]) res--;
        }

        return res;
    }

    bool terminal(){
        return free_cells().empty();
    }

    void play(int x, int y){
        make_move(x, y);
        swap_players();
    }

    Reversi gen_next_state(int x, int y){
        Reversi res;
        for(int i = 0; i < 8; i++) for(int j = 0; j < 8; j++) res.player[i][j] = player[i][j];
        for(int i = 0; i < 8; i++) for(int j = 0; j < 8; j++) res.opponent[i][j] = opponent[i][j];
        res.make_move(x, y);
        res.swap_players();
        return res;
    }

    friend ostream &operator<<(ostream &out, const Reversi &board){
        for(int i = 0; i < 8; i++){
            for(int j = 0; j < 8; j++){
                out << (board.player[i][j] ? 'P' : board.opponent[i][j] ? 'O' : '.');
            }
            cout << '\n';
        }
        return out;
    }
};

class MiniMax{
    const int max_depth = 4;
    const int MAX = 1, MIN = 0;
public:

    pair<int, int> decision(Reversi state){        // returns best move
        int best_score = INT_MIN;
        pair<int, int> best_move = {-1, -1};
        for(auto [x, y] : state.free_cells()){
            int score = minimax(state.gen_next_state(x, y), max_depth, MIN);
            if(score > best_score){
                best_score = score;
                best_move = {x, y};
            }
        }

        return best_move;
    }

    int minimax(Reversi state, int depth, int player){
        if(state.terminal()) return state.result();
        if(depth == 0) return state.result();        // TODO heuristic(state)

        int min_score = INT_MAX, max_score = INT_MIN;
        for(auto [x, y] : state.free_cells()){
            int score = minimax(state.gen_next_state(x, y), depth - 1, 1 - player);
            min_score = min(min_score, score);
            max_score = max(max_score, score);
        }

        if(player == MIN){
            return min_score;
        } else{
            return max_score;
        }
    }
};


void say(string what){
    printf("%s\n", what.c_str());
    fflush(stdout);
}

void say(string what, int x, int y){
    printf("%s %d %d\n", what.c_str(), x, y);
    fflush(stdout);
}

int main(){
    Reversi game(false);
    MiniMax ai;
    string cmd = "";
    say("RDY");
    
    while(cmd != "BYE"){
        cin >> cmd;

        if(cmd == "UGO"){
            double time_for_move, time_for_game; 
            cin >> time_for_move >> time_for_game;
            game.reset(true);

            auto p = ai.decision(game);
            say("IDO", p.first, p.second);
            game.make_move(p.first, p.second);
            // cerr << game << '\n';
            game.swap_players();
        } else if(cmd == "HEDID"){
            double time_for_move, time_for_game; 
            cin >> time_for_move >> time_for_game;

            int x, y; cin >> x >> y;
            if(x == -1 && y == -1) continue;
            game.make_move(x, y);
            game.swap_players();
            
            auto p = ai.decision(game);
            say("IDO", p.first, p.second);
            game.make_move(p.first, p.second);
            // cerr << game << '\n';
            game.swap_players();
        } else if(cmd == "ONEMORE"){
            game.reset(false);
            say("RDY");
        }
    }
}

