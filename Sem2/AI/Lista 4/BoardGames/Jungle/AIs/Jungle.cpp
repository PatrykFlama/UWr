#pragma once
#include <bits/stdc++.h>
using namespace std;

/* //* GAME RULES
'.' - land
'#' - trap
'*' - den
'~' - water
-------------
R - rat
C - cat
D - dog
W - wolf
J - jaguar
T - tiger
L - lion
E - elephant
----/\/\/\----
animal power is from low to high, except Rat can eat Elephant
animal can beat same or less power animal, except animal in #trap
only Rat can go into the ~water
Rat cant beat from ~water to .land
only Tiger and Lion can jump over ~water (not instanteously, and not over the Rat)
----------------
animal abilities groups:
1. Rat                          - can go into water, can eat Elephant
2. Cat, Dog, Wolf, Jaguar       - can do nothing
3. Tiger, Lion                  - can jump over water
4. Elephant                     - can do nothing
-----------------
STARTING positions:
L.....T
.D...C.
R.J.W.E
.......
.......
.......
e.w.j.r
.c...d.
t.....l
*/


const char board[9][7] = {
    {'.', '.', '#', '*', '#', '.', '.'},
    {'.', '.', '.', '#', '.', '.', '.'},
    {'.', '.', '.', '.', '.', '.', '.'},
    {'.', '~', '~', '.', '~', '~', '.'},
    {'.', '~', '~', '.', '~', '~', '.'},
    {'.', '~', '~', '.', '~', '~', '.'},
    {'.', '.', '.', '.', '.', '.', '.'},
    {'.', '.', '.', '#', '.', '.', '.'},
    {'.', '.', '#', '*', '#', '.', '.'}};
const pair<int, int> upper_den(3, 0);
const pair<int, int> lower_den(3, 8);


enum Animals{
    RAT = 0, CAT, DOG, WOLF, JAGUAR, TIGER, LION, ELEPHANT
};
string AnimalNames[] = {"RAT", "CAT", "DOG", "WOLF", "JAGUAR", "TIGER", "LION", "ELEPHANT"};
char AnimalShort[] = {'R', 'C', 'D', 'W', 'J', 'T', 'L', 'E'};

template <typename T,typename U>
std::pair<T,U> operator+(const std::pair<T,U> & l,const std::pair<T,U> & r) {
    return {l.first+r.first,l.second+r.second};
}


int hash_table[2][8][7][9];     // player, piece, x, y -> random int for zobrist hashing
void randomize_hash_table(){
    srand(time(NULL)-100);
    for(int i=0;i<2;i++) for(int j=0;j<8;j++) for(int x=0;x<7;x++) for(int y=0;y<9;y++) 
        hash_table[i][j][x][y] = rand();
}


class Jungle {
    // player 0 is on bottom, 1 on top
    const int DIRS[4][2] = {{0, 1}, {1, 0}, {0, -1}, {-1, 0}}; // R D L U
public:
    vector<pair<int, int>> pieces[2];   // positions of R C D W J T L E; -1 -1 if eaten (rat can eat elephant)
    int player;
    int hash;
    
    /* #region //* ----constructors---- */
    Jungle() : Jungle(0) {}
    Jungle(int player){
        randomize_hash_table();
        this->player = player;
        reset();
        recalc_hash();
    }
    Jungle(const Jungle &other){
        pieces[0] = other.pieces[0];
        pieces[1] = other.pieces[1];
        player = other.player;
        hash = other.hash;
    }
    Jungle &operator=(const Jungle &other){
        pieces[0] = other.pieces[0];
        pieces[1] = other.pieces[1];
        player = other.player;
        hash = other.hash;
        return *this;
    }
    Jungle(Jungle &&other){
        pieces[0] = move(other.pieces[0]);
        pieces[1] = move(other.pieces[1]);
        player = other.player;
        hash = other.hash;
    }
    Jungle &operator=(Jungle &&other){
        pieces[0] = move(other.pieces[0]);
        pieces[1] = move(other.pieces[1]);
        player = other.player;
        hash = other.hash;
        return *this;
    }
    /* #endregion */

    void reset(){
        //                    R       C       D       W       J       T       L       E
        pieces[1-player] = {{0, 2}, {5, 1}, {1, 1}, {4, 2}, {2, 2}, {6, 0}, {0, 0}, {6, 2}};
        pieces[player].clear();
        pieces[player].reserve(8);
        for(auto [x, y] : pieces[1-player])
            pieces[player].push_back({6 - x, 8 - y});
    }
    void recalc_hash(){
        hash = 0;
        for(int i = 0; i < 8; i++){     // iterate over pieces
            hash ^= hash_table[player][i][pieces[player][i].first][pieces[player][i].second];
            hash ^= hash_table[1-player][i][pieces[1-player][i].first][pieces[1-player][i].second];
        }
    }
    int gen_next_hash(int piece, pair<int, int> dir){
        int new_hash = hash;
        auto [x, y] = pieces[player][piece];
        new_hash ^= hash_table[player][piece][x][y];        // remove current piece at location from hash

        auto [new_x, new_y] = pieces[player][piece] + dir;
        new_hash ^= hash_table[player][piece][new_x][new_y];    // add current piece at new location to hash

        for(int i = 0; i < pieces[1-player].size() ;i++){       // beat opponent pieces
            if(pair<int, int>(x, y) == pieces[1-player][i]){
                new_hash ^= hash_table[1-player][i][x][y];      // remove opponent piece at location from hash
                break;
            }
        }

        return new_hash;
    }

    void swap_players(){
        player = 1-player;
    }

    char get_cell(pair<int, int> d) const {         // returns field type for given position
        return board[d.second][d.first];
    }

    int get_piece_type(int x, int y){
        for(int i = 0; i < pieces[player].size(); i++)
            if(pieces[player][i] == pair<int, int>(x, y))
                return i;
        return 0;       // that should not run
    }

    void move_player_piece(int piece, pair<int, int> dir){      //? moves piece and beats opponent piece on new position
        auto [x, y] = pieces[player][piece];
        hash ^= hash_table[player][piece][x][y];        // remove current piece at location from hash

        auto [new_x, new_y] = pieces[player][piece] + dir;
        pieces[player][piece] = {new_x, new_y};
        hash ^= hash_table[player][piece][new_x][new_y];    // add current piece at new location to hash

        for(int i = 0; i < pieces[1-player].size() ;i++){       // beat opponent pieces
            if(pair<int, int>(new_x, new_y) == pieces[1-player][i]){
                hash ^= hash_table[1-player][i][new_x][new_y];      // remove opponent piece at location from hash
                pieces[1-player][i] = pair<int, int>(-1, -1);
                break;
            }
        }
    }

    /* #region //* ----moves generation---- */
    bool stronger(int player, int opponent){
        return (player > opponent) || (player == RAT && opponent == ELEPHANT);
    }
    bool can_beat(int player_piece, int opponent_piece){
        if(get_cell(pieces[player][player_piece]) == '~' && get_cell(pieces[1-player][opponent_piece]) != '~') return false;
        if(get_cell(pieces[1-player][opponent_piece]) == '#') return true;
        return stronger(player_piece, opponent_piece);
    }

    bool move_safe(int piece, pair<int, int> dir){        // ignoring pieces on board
        pair<int, int> new_pos = pieces[player][piece] + dir;
        if(new_pos.first < 0 || new_pos.first >= 7 || new_pos.second < 0 || new_pos.second >= 9) return false;
        if((player == 0 && new_pos == lower_den) ||
           (player == 1 && new_pos == upper_den)) return false;        // player cant go to his own den
        if(get_cell(new_pos) == '~'){
            if(piece == RAT) return true;
            if(piece == TIGER || piece == LION){        // check if rat is not in line
                if(dir.first == 0)
                    return pieces[player][piece].first != pieces[1-player][RAT].first;
                else
                    return pieces[player][piece].second != pieces[1-player][RAT].second;
            }
            else return false;
        }
        return true;
    }

    bool move_legal(int piece, pair<int, int> dir){
        if(not move_safe(piece, dir)) return false;
        pair<int, int> new_pos = pieces[player][piece] + dir;
        if(piece == TIGER || piece == LION)
            while(get_cell(new_pos) == '~') new_pos = new_pos + dir;      // jump over water

        for(int teammate = 0; teammate < 8; teammate++)
            if(pieces[player][teammate] == new_pos)
                return false;
        for(int opponent = 0; opponent < 8; opponent++)
            if(pieces[1-player][opponent] == new_pos)
                if(not can_beat(player, opponent)) return false;
        return true;
    }

    vector<pair<int, pair<int, int>>> get_legal_moves(){            // {piece, {dirx, diry}}
        vector<pair<int, pair<int, int>>> moves;
        for(int piece = 0; piece < 8; piece++){
            for(auto [x, y] : DIRS){
                if(move_legal(piece, {x, y}))
                    moves.push_back({piece, {x, y}});
            }
        }

        return moves;
    }
    /* #endregion */

    void execute_move(pair<int, pair<int, int>> move){
        execute_move(move.first, move.second);
    }
    void execute_move(int piece, pair<int, int> dir){
        move_player_piece(piece, dir);
        if(piece == TIGER || piece == LION)
            while(get_cell(pieces[player][piece]) == '~')
                move_player_piece(piece, dir);
        swap_players();
    }

    Jungle gen_next_state(int piece, pair<int, int> dir){
        Jungle next_state = *this;
        next_state.execute_move(piece, dir);
        return next_state;
    }

    bool game_won(){
        for(int i = 0; i < pieces[player].size(); i++){
            if((pieces[player][i].first != -1 && get_cell(pieces[player][i]) == '*') ||
               (pieces[1-player][i].first != -1 && get_cell(pieces[1-player][i]) == '*'))
                return true;
        }
        return false;
    }

    bool terminal(vector<pair<int, pair<int, int>>> legal_moves){
        return legal_moves.empty() || game_won();
    }

    int result(int main_player) const { // for mcts, big values for precise avg results
        int res = 0;
        for(int i = 0; i < pieces[main_player].size(); i++){
            if(pieces[main_player][i].first != -1){      // if piece is alive
                if(get_cell(pieces[main_player][i]) == '*') return INT_MAX;
            }
            if(pieces[1-main_player][i].first != -1){
                if(get_cell(pieces[main_player][i]) == '*') return INT_MIN; 
            }
        }
        return -1000000;          // for draw
    }

    int heuristic_result(int main_player){
        //                             R   C   D   W  J  T  L  E
        const int pieces_weights[] = {10,  2,  3,  5, 6, 7, 8, 9};
        const int pieces_weight_scale = 5;
        const int board_placement[] = {1, -1, -1, -1, 1, 1, 1, 1};
        int res = 0;
        for(int i = 0; i < pieces[main_player].size(); i++){
            if(pieces[main_player][i].first != -1){      // if piece is alive
                res += board_placement[i] * dist(pieces[main_player][i], (main_player ? upper_den : lower_den));
                if(get_cell(pieces[main_player][i]) == '*') res += 1000;
            } else{
                res -= pieces_weights[i] * pieces_weight_scale;
            }
            if(pieces[1-main_player][i].first != -1){
                res -= board_placement[i] * dist(pieces[1-main_player][i], (1-main_player ? upper_den : lower_den));
                if(get_cell(pieces[1-main_player][i]) == '*') res -= 1000; 
            } else{
                res += pieces_weights[i] * pieces_weight_scale;
            }
        }
        
        return res;
    }

    inline int dist(pair<int, int> from, pair<int, int> den){
        // return sqrt((from.first+den.first)*(from.first+den.first) + 
        //             (from.second+den.second)*(from.second+den.second));
        return abs(from.first-den.first) + abs(from.second-den.second);
    }

    friend ostream &operator<<(ostream &out, const Jungle &state) {
        char tempboard[9][7];
        for(int i = 0; i < 9; i++) for(int j = 0; j < 7; j++) tempboard[i][j] = board[i][j];

        for(int i = 0 ; i < state.pieces[0].size(); i++){
            if(state.pieces[0][i].first != -1)
                tempboard[state.pieces[0][i].second][state.pieces[0][i].first] = AnimalShort[i]-'A'+'a';
            if(state.pieces[1][i].first != -1)
                tempboard[state.pieces[1][i].second][state.pieces[1][i].first] = AnimalShort[i];
        }
        for(int i = 0; i < 9; i++){
            for(int j = 0; j < 7; j++){
                out << tempboard[i][j];
            }
            out << '\n';
        }
        return out;
    }
};
