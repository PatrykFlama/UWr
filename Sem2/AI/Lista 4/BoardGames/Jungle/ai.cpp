#include <bits/stdc++.h>
using namespace std;

/*
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

const char starting_positions[9][7] = {
    {'L', '.', '.', '.', '.', '.', 'T'},
    {'.', 'D', '.', '.', '.', 'C', '.'},
    {'R', '.', 'J', '.', 'W', '.', 'E'},
    {'.', '.', '.', '.', '.', '.', '.'},
    {'.', '.', '.', '.', '.', '.', '.'},
    {'.', '.', '.', '.', '.', '.', '.'},
    {'e', '.', 'w', '.', 'j', '.', 'r'},
    {'.', 'c', '.', '.', '.', 'd', '.'},
    {'t', '.', '.', '.', '.', '.', 'l'}};

enum Animals{
    RAT = 0, CAT, DOG, WOLF, JAGUAR, TIGER, LION, ELEPHANT
};
enum Jupmers{
    TIGER_JUMP = 0, LION_JUMP
};

template <typename T,typename U>                                                   
std::pair<T,U> operator+(const std::pair<T,U> & l,const std::pair<T,U> & r) {   
    return {l.first+r.first,l.second+r.second};                                    
}


class Jungle {
    const int DIRS[4][2] = {{0, 1}, {1, 0}, {0, -1}, {-1, 0}}; // R D L U
public:
    vector<pair<int, int>> player_pieces;   // positions of R C D W J T L E; -1 -1 if eaten (rat can eat elephant)
    vector<pair<int, int>> opponent_pieces;
    vector<pair<int, int>> player_force_jump_direction; // direction of jump for tiger and lion; 0 0 if no force jump
    vector<pair<int, int>> opponent_force_jump_direction;

    Jungle(){
        reset();
    }

    void reset(){
        player_pieces = {{0, 2}, {6, 1}, {1, 1}, {5, 2}, {2, 2}, {7, 0}, {0, 0}, {7, 2}};
        opponent_pieces.clear();
        opponent_pieces.reserve(8);
        for(auto [x, y] : player_pieces)
            opponent_pieces.push_back({7 - x, 9 - y});
        player_force_jump_direction = {{0, 0}, {0, 0}};
        opponent_force_jump_direction = {{0, 0}, {0, 0}};
    }

    void move_player_piece(int player, pair<int, int> dir){
        auto [x, y] = player_pieces[player];
        player_pieces[player] = {x + dir.first, y + dir.second};
    }
    void move_opponent_piece(int opponent, pair<int, int> dir){
        auto [x, y] = opponent_pieces[opponent];
        opponent_pieces[opponent] = {x + dir.first, y + dir.second};
    }

    char get_cell(pair<int, int> d){
        return board[d.first][d.second];
    }

    bool player_in_range(int player, int opponent){
        for(auto [x, y] : DIRS)
            if(player_pieces[player] + pair<int, int>(player, opponent) == opponent_pieces[opponent])
                return true;
        return false;
    }
    bool stronger(int player, int opponent){
        return (player > opponent) || (player == RAT && opponent == ELEPHANT);
    }

    bool can_beat(int player, int opponent){
        if(!player_in_range(player, opponent)) return false;
        if(get_cell(player_pieces[player]) == '~' && get_cell(opponent_pieces[opponent]) != '~') return false;
        if(get_cell(opponent_pieces[opponent]) == '#') return true;
        return stronger(player, opponent);
    }

    bool move_safe(int player, pair<int, int> dir){        // ignoring pieces on board
        if(dir.first < 0 || dir.first >= 7 || dir.second < 0 || dir.second >= 9) return false;
        if(get_cell(player_pieces[player] + dir) == '~'){
            if(player == RAT || player == TIGER || player == LION) return true;
            else return false;
        }
        return true;
    }
    bool move_legal(int player, pair<int, int> dir){
        if(not move_safe(player, dir)) return false;
        for(int teammate = 0; teammate < 8; teammate++)
            if(player_pieces[teammate] == player_pieces[player] + dir)
                return false;
        for(int opponent = 0; opponent < 8; opponent++)
            if(opponent_pieces[opponent] == player_pieces[player] + dir)
                if(not can_beat(player, opponent)) return false;
        return true;
    }

    vector<pair<int, pair<int, int>>> get_legal_moves(){
        vector<pair<int, pair<int, int>>> moves;
        for(int player = 0; player < 8; player++){
            for(auto [x, y] : DIRS){
                if(move_legal(player, {x, y}))
                    moves.push_back({player, {x, y}});
            }
        }
        return moves;
    }

    Jungle gen_next_state(int piece, pair<int, int> dir){
        Jungle next_state = *this;
        if(piece < 8) next_state.move_player_piece(piece, dir);
        else next_state.move_opponent_piece(piece - 8, dir);
        return next_state;
    }
};


class AI{
public:
    pair<int, pair<int, int>> best_move();
};  


int main() {

}