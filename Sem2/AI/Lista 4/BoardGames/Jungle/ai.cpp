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
    //TODO - memory efficiency upgrade:
    /*
    location now instead of vector<pair<int, int>> is vector<int>,
    so [x, y] = x*7+y

    player and opponent pieces are kept in table/vector
    we keep which player (1 or 0) turn it is
    now to access active player we do: pieces[1-turn]
    */
    vector<pair<int, int>> pieces[2];   // positions of R C D W J T L E; -1 -1 if eaten (rat can eat elephant)
    vector<pair<int, int>> force_jump_direction[2]; // direction of jump for tiger and lion; 0 0 if no force jump
    int player;
    
    Jungle() : Jungle(0) {}
    Jungle(int player){
        this->player = player;
        reset();
    }

    void reset(){
        pieces[player] = {{0, 2}, {6, 1}, {1, 1}, {5, 2}, {2, 2}, {7, 0}, {0, 0}, {7, 2}};
        pieces[1-player].clear();
        pieces[1-player].reserve(8);
        for(auto [x, y] : pieces[player])
            pieces[1-player].push_back({7 - x, 9 - y});
        force_jump_direction[player] = {{0, 0}, {0, 0}};
        force_jump_direction[1-player] = {{0, 0}, {0, 0}};
    }

    void swap_players(){
        player = 1-player;
    }

    void move_player_piece(int player, pair<int, int> dir){
        auto [x, y] = pieces[player][player];
        pieces[player][player] = {x + dir.first, y + dir.second};
    }
    void move_opponent_piece(int opponent, pair<int, int> dir){
        auto [x, y] = pieces[1-player][opponent];
        pieces[1-player][opponent] = {x + dir.first, y + dir.second};
    }

    char get_cell(pair<int, int> d) const {         // returns field type for given position
        return board[d.first][d.second];
    }

    bool player_in_range(int player, int opponent){
        for(auto [x, y] : DIRS)
            if(pieces[player][player] + pair<int, int>(player, opponent) == pieces[1-player][opponent])
                return true;
        return false;
    }
    bool stronger(int player, int opponent){
        return (player > opponent) || (player == RAT && opponent == ELEPHANT);
    }

    bool can_beat(int player, int opponent){
        if(!player_in_range(player, opponent)) return false;
        if(get_cell(pieces[player][player]) == '~' && get_cell(pieces[1-player][opponent]) != '~') return false;
        if(get_cell(pieces[1-player][opponent]) == '#') return true;
        return stronger(player, opponent);
    }

    bool move_safe(int player, pair<int, int> dir){        // ignoring pieces on board
        if(dir.first < 0 || dir.first >= 7 || dir.second < 0 || dir.second >= 9) return false;
        if(get_cell(pieces[player][player] + dir) == '~'){
            if(player == RAT || player == TIGER || player == LION) return true;
            else return false;
        }
        return true;
    }
    bool move_legal(int player, pair<int, int> dir){
        if(not move_safe(player, dir)) return false;
        for(int teammate = 0; teammate < 8; teammate++)
            if(pieces[player][teammate] == pieces[player][player] + dir)
                return false;
        for(int opponent = 0; opponent < 8; opponent++)
            if(pieces[1-player][opponent] == pieces[player][player] + dir)
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

    int result() const {
        int res = 0;
        for(int i = 0; i < pieces[player].size(); i++){
            if(pieces[player][i].first != -1){      // if piece is alive
                res += i;       //? small bonus for alive piece
                if(get_cell(pieces[player][i]) == '*') res += 1000; 
            }
            if(pieces[1-player][i].first != -1){
                res -= i;
                if(get_cell(pieces[player][i]) == '*') res -= 1000; 
            }
        }
        return res;
    }

    int heuristic_result() const {
        // TODO
    }

    bool game_won(){
        for(int i = 0; i < pieces[player].size(); i++){
            if((pieces[player][i].first != -1 && get_cell(pieces[player][i]) == '*') ||
               (pieces[1-player][i].first != -1 && get_cell(pieces[player][i]) == '*'))
                return true;
        }
    }

    bool terminal(vector<pair<int, pair<int, int>>> legal_moves){
        return legal_moves.empty() || game_won();
    }
};


class AI{
public:
    pair<int, pair<int, int>> best_move();
};  


int main() {

}