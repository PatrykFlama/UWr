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

enum Animals{
    RAT = 0, CAT, DOG, WOLF, JAGUAR, TIGER, LION, ELEPHANT
};

template <typename T,typename U>
std::pair<T,U> operator+(const std::pair<T,U> & l,const std::pair<T,U> & r) {
    return {l.first+r.first,l.second+r.second};
}


class Jungle {
    // player 0 is on bottom, 1 on top
    const int DIRS[4][2] = {{0, 1}, {1, 0}, {0, -1}, {-1, 0}}; // R D L U
public:
    vector<pair<int, int>> pieces[2];   // positions of R C D W J T L E; -1 -1 if eaten (rat can eat elephant)
    int player;
    int hash;           // todo zobrist hashing
    
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
    }

    void move_player_piece(int piece, pair<int, int> dir){      //? moves piece and beats opponent piece on new position
        auto [x, y] = pieces[player][piece];
        pieces[player][piece] = {x + dir.first, y + dir.second};

        for(int i = 0; i < pieces[1-player].size() ;i++){       // beat opponent pieces
            if(pair<int, int>(x, y) == pieces[1-player][i]){
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
        if((player == 0 && new_pos == pair<int, int>(8, 3)) ||
           (player == 1 && new_pos == pair<int, int>(0, 3))) return false;        // player cant go to his own den
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

    void execute_move(int piece, pair<int, int> dir){
        move_player_piece(piece, dir);
        if(piece == TIGER || piece == LION)
            while(get_cell(pieces[player][piece]) == '~')
                move_player_piece(piece, dir);
        swap_players();
    }

    Jungle gen_next_state(int piece, pair<int, int> dir){
        Jungle next_state = *this;      // todo is that copying for sure?
        next_state.execute_move(piece, dir);
        return next_state;
    }

    bool game_won(){
        for(int i = 0; i < pieces[player].size(); i++){
            if((pieces[player][i].first != -1 && get_cell(pieces[player][i]) == '*') ||
               (pieces[1-player][i].first != -1 && get_cell(pieces[1-player][i]) == '*'))
                return true;
        }
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
};


class zad3AI{
public:
    pair<int, pair<int, int>> get_best_move(Jungle* state, int N = 2e4){            // {piece, {dirx, diry}}
        vector<pair<int, pair<int, int>>> legal_moves = state->get_legal_moves();
        pair<int, pair<int, int>> best_move;
        int max_score = INT_MIN;

        int moves_per_state = N/legal_moves.size();
        for(auto [piece, dir] : legal_moves){
            int score = heuristics(&state->gen_next_state(piece, dir), moves_per_state);
            if(score > max_score){
                max_score = score;
                best_move = {piece, dir};
            }
        }

        return best_move;
    }

    int heuristics(Jungle* state, int moves_left){
        Jungle *temp_state = state;
        int total_res = 0, games_played = 0;
        vector<pair<int, pair<int, int>>> legal_moves;
        while(moves_left > 0){
            legal_moves = temp_state->get_legal_moves();
            if(temp_state->terminal(legal_moves)) {
                total_res += temp_state->result();
                ++games_played;
                temp_state = state;
            }

            auto [piece, dir] = legal_moves[rand() % legal_moves.size()];
            temp_state = &temp_state->gen_next_state(piece, dir);
            --moves_left;
        }

        return total_res/games_played;
    }
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
        tree[Jungle(0).hash] = Node();
        tree[Jungle(1).hash] = Node();
    }

    pair<int, pair<int, int>> get_best_move(Jungle* state){            // {piece, {dirx, diry}}
        vector<pair<int, pair<int, int>>> legal_moves = state->get_legal_moves();
        pair<int, pair<int, int>> best_move;
        int max_score = INT_MIN;

        for(auto [piece, dir] : legal_moves){
            int score = tree[state->gen_next_state(piece, dir).hash].avg_value;
            if(score > max_score){
                max_score = score;
                best_move = {piece, dir};
            }
        }

        return best_move;
    }

    void mcts(Jungle* state){
        // tree traversal phase:
        Node* here = &tree[state->hash];
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


        if(tree[state->hash].times_sampled == 0) rollout(state);
        else{   // node expansion
            vector<pair<int, pair<int, int>>> legal_moves = state->get_legal_moves();
            Jungle *temp;
            for(auto [piece, dir] : legal_moves){       // create new nodes for each possible move
                temp = &state->gen_next_state(piece, dir);
                tree[temp->hash] = Node();
            }
            rollout(temp);      // do rollout for one of those new states
        }
    }

    int ucb1(Jungle* state, int parent_visits){
        Node* here = &tree[state->hash];
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


void rdy(){
    printf("RDY\n");
    fflush(stdout);
}
void ido(int xs, int ys, int xd, int yd){
    printf("IDO %d %d %d %d\n", xs, ys, xd, yd);
    fflush(stdout);
}

int main() {
    srand(time(NULL));
    Jungle game;    // defaults to starting at the bottom of board
    zad3AI ai;
    rdy();

    while(true){
        string cmd; cin >> cmd;

        if(cmd == "HEDID"){
            double time_for_move, time_for_game; 
            cin >> time_for_move >> time_for_game;

            int xs, ys, xd, yd;
            cin >> xs >> ys >> xd >> yd;
            if(xs == -1) continue;      // opponent passed
            game.move_player_piece(game.get_piece_type(xs, ys), {xd, yd});
            game.swap_players();

            auto [piece, dir] = ai.get_best_move(&game);
            auto [myxs, myys] = game.pieces[game.player][piece];
            ido(myxs, myys, myxs+dir.first, myys+dir.second);
            game.move_player_piece(piece, dir);
            game.swap_players();
        } else if(cmd == "UGO"){
            double time_for_move, time_for_game; 
            cin >> time_for_move >> time_for_game;

            auto [piece, dir] = ai.get_best_move(&game);
            auto [myxs, myys] = game.pieces[game.player][piece];
            ido(myxs, myys, myxs+dir.first, myys+dir.second);
            game.move_player_piece(piece, dir);
            game.swap_players();
        } else if(cmd == "ONEMORE"){
            game.reset();
            rdy();
        } else if(cmd == "BYE"){
            break;
        }
    }
}
