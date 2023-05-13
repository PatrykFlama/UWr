#include <bits/stdc++.h>
using namespace std;

/*
black player starts
starting sheme:
...WB...
...BW...
*/

class ReversiBoard{
    const int DIRS[8][2] = {{-1, -1}, {-1, 0}, {-1, 1}, {0, 1},
                            {1, 1},   {1, 0},  {1, -1}, {0, -1}};

public:
    bool player[8][8];          // active player player's pieces
    bool opponent[8][8];        // opponent's pieces

    ReversiBoard(){}
    ReversiBoard(bool black_starts){
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

    void make_move(int x, int y){
        set_player_cell(x, y, true);
        for(auto [dx, dy] : DIRS){
            if(can_beat(x, y, {dx, dy})){
                while(get_opponent_cell(x, y)){
                    set_opponent_cell(x, y, false);
                    set_player_cell(x, y, true);
                    x += dx;
                    y += dy;
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

    ReversiBoard next_state(int x, int y){
        ReversiBoard res;
        for(int i = 0; i < 8; i++) for(int j = 0; j < 8; j++) res.player[i][j] = player[i][j];
        for(int i = 0; i < 8; i++) for(int j = 0; j < 8; j++) res.opponent[i][j] = opponent[i][j];
        swap(res.player, res.opponent);
        return res;
    }

    friend ostream &operator<<(ostream &out, const ReversiBoard &board){
        for(int i = 0; i < 8; i++){
            for(int j = 0; j < 8; j++){
                out << (board.player[i][j] ? 'P' : board.opponent[i][j] ? 'O' : '.');
            }
            cout << '\n';
        }
        return out;
    }
};

class Game{

};

class MiniMax{

};


int main(){
    ReversiBoard rb(true);
    cout << rb << '\n';

    for(pair<int, int> p : rb.free_cells()){
        ReversiBoard newrb = rb.next_state(p.first, p.second);
        cout << p.first << ' ' << p.second << ":\n";
        cout << newrb;
        cout << '\n';
    }
}

