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

/* //TODO
* optimize new state creation (memore opt or smth)

todo new opt idea: save piece location as bitmask
that will lead to memory/2 usage, faster new state creation and faster hashing
to move player we shift mask by 7*dirx + diry
to transfer positions to bitmask we will also represent map as mask of water, trap, den:
int water, trap, den
! Zobrist hashing
! remember children in node of mcts tree
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
    // player 0 is on bottom, 1 on top
    const int DIRS[4][2] = {{0, 1}, {1, 0}, {0, -1}, {-1, 0}}; // R D L U
public:
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

    char get_cell(pair<int, int> d) const {         // returns field type for given position
        return board[d.second][d.first];
    }

    void move_player_piece(int piece, pair<int, int> dir){
        auto [x, y] = pieces[player][piece];
        pieces[player][piece] = {x + dir.first, y + dir.second};
    }
    void move_opponent_piece(int piece, pair<int, int> dir){
        auto [x, y] = pieces[1-player][piece];
        pieces[1-player][piece] = {x + dir.first, y + dir.second};
    }

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
        if(get_cell(new_pos) == '~'){
            if(player == RAT || player == TIGER || player == LION) return true;
            else return false;
        }
        if((player == 0 && new_pos == pair<int, int>(8, 3)) ||
           (player == 1 && new_pos == pair<int, int>(0, 3))) return false;        // player cant go to his own den
        return true;
    }
    bool move_legal(int piece, pair<int, int> dir){
        if(not move_safe(piece, dir)) return false;
        for(int teammate = 0; teammate < 8; teammate++)
            if(pieces[player][teammate] == pieces[player][piece] + dir)
                return false;
        for(int opponent = 0; opponent < 8; opponent++)
            if(pieces[1-player][opponent] == pieces[player][piece] + dir)
                if(not can_beat(player, opponent)) return false;
        return true;
    }

    vector<pair<int, pair<int, int>>> get_legal_moves(){            // {piece, {dirx, diry}}
        vector<pair<int, pair<int, int>>> moves;
        for(int piece = 0; piece < 8; piece++){
            if(piece == TIGER && force_jump_direction[player][TIGER_JUMP] != pair<int, int>(0, 0)) continue;   // tiger has to jump over water (on his own)
            else if(piece == LION && force_jump_direction[player][LION_JUMP] != pair<int, int>(0, 0)) continue;

            for(auto [x, y] : DIRS){
                if(move_legal(piece, {x, y}))
                    moves.push_back({piece, {x, y}});
            }
        }

        return moves;
    }

    bool game_won(){
        for(int i = 0; i < pieces[player].size(); i++){
            if((pieces[player][i].first != -1 && get_cell(pieces[player][i]) == '*') ||
               (pieces[1-player][i].first != -1 && get_cell(pieces[player][i]) == '*'))
                return true;
        }
    }

    void update_jumpers(){
        if(force_jump_direction[player][TIGER_JUMP] != pair<int, int>(0, 0)){
            move_player_piece(TIGER, force_jump_direction[player][TIGER_JUMP]);
            if(get_cell(pieces[player][TIGER]) != '~') force_jump_direction[player][TIGER_JUMP] = {0, 0};
        }
        if(force_jump_direction[player][LION_JUMP] != pair<int, int>(0, 0)){
            move_player_piece(LION, force_jump_direction[player][LION]);
            if(get_cell(pieces[player][LION]) != '~') force_jump_direction[player][LION_JUMP] = {0, 0};
        }

        if(force_jump_direction[1-player][LION_JUMP] != pair<int, int>(0, 0)){
            move_opponent_piece(TIGER, force_jump_direction[1-player][LION]);
            if(get_cell(pieces[1-player][TIGER]) != '~') force_jump_direction[1-player][TIGER_JUMP] = {0, 0};
        }
        if(force_jump_direction[1-player][LION_JUMP] != pair<int, int>(0, 0)){
            move_opponent_piece(LION, force_jump_direction[1-player][LION]);
            if(get_cell(pieces[1-player][LION]) != '~') force_jump_direction[1-player][LION_JUMP] = {0, 0};
        }
    }

    Jungle gen_next_state(int piece, pair<int, int> dir){       // todo probably change/regen hash here
        Jungle next_state = *this;      // todo is that copying for sure?
        next_state.update_jumpers();
        next_state.move_player_piece(piece, dir);
        if((piece == TIGER || piece == LION) && get_cell(pieces[player][piece]) == '~')
            next_state.force_jump_direction[player][piece == TIGER ? TIGER_JUMP : LION_JUMP] = dir;
        next_state.swap_players();
        return next_state;
    }

    int result() const {
        int res = 0;
        for(int i = 0; i < pieces[player].size(); i++){
            if(pieces[player][i].first != -1){      // if piece is alive
                res += i;       //? small bonus for alive piece should help to differentiate by 'better' and 'worse' wins
                if(get_cell(pieces[player][i]) == '*') res += 1000; 
            }
            if(pieces[1-player][i].first != -1){
                res -= i;
                if(get_cell(pieces[player][i]) == '*') res -= 1000; 
            }
        }
        return res;
    }

    bool terminal(vector<pair<int, pair<int, int>>> legal_moves){
        return legal_moves.empty() || game_won();
    }

    int hash(){
        const int mod = 2e9+11;
        int res = 0;
        for(auto [x,y] : pieces[player]){
            res += x+y*9;
            res %= mod;
            res *= 63;
            res %= mod;
        }
        for(auto [x,y] : pieces[1-player]){
            res += x+y*9;
            res %= mod;
            res *= 63;
            res %= mod;
        }
        return res;         // todo is it any good? any yup, good nope
        // TODO with bitmask positions: xor everything, last bit for player turn?
        //! Zobrist hashing
    }
};


class zad3AI{
public:

};


class Node{
public:
    vector<int> children;   // todo - actually use it
    int times_sampled = 0;
    bool is_leaf = true;
    int avg_value = 0;
};

class MCTS{
    // todo MCTS tree, feg map <hash of state, node class>
    unordered_map<int, Node> tree;
public:
    MCTS(){
        srand(time(NULL));
        tree[Jungle(0).hash()] = Node();
        tree[Jungle(1).hash()] = Node();
    }

    pair<int, pair<int, int>> get_best_move(Jungle* state){            // {piece, {dirx, diry}}
        vector<pair<int, pair<int, int>>> legal_moves = state->get_legal_moves();
        pair<int, pair<int, int>> best_move;
        int max_score = INT_MIN;

        for(auto [piece, dir] : legal_moves){
            int score = tree[state->gen_next_state(piece, dir).hash()].avg_value;
            if(score > max_score){
                max_score = score;
                best_move = {piece, dir};
            }
        }

        return best_move;
    }

    void mcts(Jungle* state){
        // tree traversal phase:
        Node* here = &tree[state->hash()];
        while(not here->is_leaf){       // get leaf node in mcts tree
            vector<pair<int, pair<int, int>>> legal_moves = state->get_legal_moves();
            int max_ucb = INT_MIN;
            Jungle *best_state;
            for(auto [piece, dir] : legal_moves){
                Jungle *temp = &state->gen_next_state(piece, dir);
                if(ucb1(temp, here->times_sampled) > max_ucb){
                    best_state = temp;
                }
            }
            state = best_state;
        }


        if(tree[state->hash()].times_sampled == 0) rollout(state);
        else{   // node expansion
            vector<pair<int, pair<int, int>>> legal_moves = state->get_legal_moves();
            Jungle *temp;
            for(auto [piece, dir] : legal_moves){       // create new nodes for each possible move
                temp = &state->gen_next_state(piece, dir);
                tree[temp->hash()] = Node();
            }
            rollout(temp);      // do rollout for one of those new states
        }
    }

    int ucb1(Jungle* state, int parent_visits){
        Node* here = &tree[state->hash()];
        if(here->times_sampled == 0) return INT_MAX;
        return here->avg_value + 2*sqrt(log(parent_visits)/here->times_sampled);
    }

    int rollout(Jungle* state){
        vector<pair<int, pair<int, int>>> legal_moves = state->get_legal_moves();
        while(not state->terminal(legal_moves)){
            // pick random move
            auto random = rand() % legal_moves.size();
            state = &state->gen_next_state(legal_moves[random].first, legal_moves[random].second);

            legal_moves = state->get_legal_moves();
        }
        return state->result();
    }
};


int main() {
    srand(time(NULL));
}
