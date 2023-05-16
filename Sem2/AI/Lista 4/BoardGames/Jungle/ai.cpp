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
        opponent_pieces.reserve(8);
        for(auto [x, y] : player_pieces)
            opponent_pieces.push_back({7 - x, 9 - y});
        player_force_jump_direction = {{0, 0}, {0, 0}};
        opponent_force_jump_direction = {{0, 0}, {0, 0}};
    }

    void move_player_piece(int dx, int dy, int piece_index){
        auto [x, y] = player_pieces[piece_index];
        player_pieces[piece_index] = {x + dx, y + dy};
    }
    void move_opponent_piece(int dx, int dy, int piece_index){
        auto [x, y] = opponent_pieces[piece_index];
        opponent_pieces[piece_index] = {x + dx, y + dy};
    }

    
};

int main() {

}