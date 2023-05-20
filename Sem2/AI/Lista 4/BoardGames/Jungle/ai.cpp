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


class Jungle {
    // player 0 is on bottom, 1 on top
    const int DIRS[4][2] = {{0, 1}, {1, 0}, {0, -1}, {-1, 0}}; // R D L U
public:
    vector<pair<int, int>> pieces[2];   // positions of R C D W J T L E; -1 -1 if eaten (rat can eat elephant)
    int player;
    int hash;           // todo zobrist hashing
    
    /* #region //* ----constructors---- */
    Jungle() : Jungle(0) {}
    Jungle(int player){
        this->player = player;
        reset();
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

    int result(int player) const {
        int res = 0;
        for(int i = 0; i < pieces[player].size(); i++){
            if(pieces[player][i].first != -1){      // if piece is alive
                if(get_cell(pieces[player][i]) == '*') return 3; 
            }
            if(pieces[1-player][i].first != -1){
                if(get_cell(pieces[player][i]) == '*') return -3; 
            }
        }
        return -1;          // -1 for draw
    }

    int heuristic_result(int player){
        //                             R  C  D  W  J  T  L  E
        const int pieces_weights[] = {10, 2, 3, 5, 6, 7, 8, 9};
        int res = 0;
        for(int i = 0; i < pieces[player].size(); i++){
            if(pieces[player][i].first != -1){      // if piece is alive
                res += pieces_weights[i];
                if(get_cell(pieces[player][i]) == '*') res += 100; 
            }
            if(pieces[1-player][i].first != -1){
                res -= pieces_weights[i];
                if(get_cell(pieces[player][i]) == '*') res -= 100; 
            }
        }
        return res;
    }

    bool terminal(vector<pair<int, pair<int, int>>> legal_moves){
        return legal_moves.empty() || game_won();
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


class Random{
public: 
    pair<int, pair<int, int>> gen_next_move(Jungle* state, int N = 2e4){            // {piece, {dirx, diry}}
        vector<pair<int, pair<int, int>>> legal_moves = state->get_legal_moves();
        return legal_moves[rand() % legal_moves.size()];
    }
};

class zad3AI{
    int main_player;
public:
    pair<int, pair<int, int>> gen_next_move(Jungle* state, int N = 3){            // {piece, {dirx, diry}}
        main_player = state->player;
        vector<pair<int, pair<int, int>>> legal_moves = state->get_legal_moves();
        if(legal_moves.size() == 0) return {0, {0, 0}};

        pair<int, pair<int, int>> best_move;
        int max_score = INT_MIN;

        // cerr << "Looking thru moves: ";
        for(auto [piece, dir] : legal_moves){
            Jungle temp = state->gen_next_state(piece, dir);
            int score = random(&temp, N);
            score += reinforced_random(&temp, N);
            if(score > max_score){
                max_score = score;
                best_move = {piece, dir};
            }
            // cerr << score << ' ';
        }
        // cerr << '\n';

        return best_move;
    }

    inline int rate(int total_res, int games_played){
        return (games_played == 0 ? total_res : 100*((double)total_res)/games_played);
    }

    int random(Jungle* state, int max_games){
        vector<pair<int, pair<int, int>>> starting_legal_moves = state->get_legal_moves();
        if(state->terminal(starting_legal_moves)) return state->result(main_player)*max_games;

        Jungle here = *state;
        int total_res = 0, games_played = 0;
        vector<pair<int, pair<int, int>>> legal_moves;
        while(games_played < max_games){
            legal_moves = here.get_legal_moves();
            if(here.terminal(legal_moves)) {
                total_res += here.result(main_player);
                ++games_played;
                here = *state;      // copy 
                legal_moves = starting_legal_moves;
            }

            auto [piece, dir] = legal_moves[rand() % legal_moves.size()];
            here.execute_move(piece, dir);
        }

        // cerr << "Games played: " << games_played << " res: " << rate(total_res, games_played) << '\n';
        return total_res;
    }

    int reinforced_random(Jungle* state, int max_games){
        vector<pair<int, pair<int, int>>> starting_legal_moves = state->get_legal_moves();
        if(state->terminal(starting_legal_moves)) return state->result(main_player)*max_games;

        Jungle here = *state;
        int total_res = 0, games_played = 0;
        vector<pair<int, pair<int, int>>> legal_moves;
        while(games_played < max_games){
            legal_moves = here.get_legal_moves();
            if(here.terminal(legal_moves)) {
                total_res += here.result(main_player);
                ++games_played;
                here = *state;      // copy 
                legal_moves = starting_legal_moves;
            }

            int best_piece;
            pair<int, int> best_dir;
            int max_score = INT_MIN;
            for(auto [piece, dir] : legal_moves){
                Jungle temp = here.gen_next_state(piece, dir);
                int score = heuristics(&temp);
                if(score > max_score){
                    max_score = score;
                    best_piece = piece;
                    best_dir = dir;
                }
            }
            here.execute_move(best_piece, best_dir);
        }

        // cerr << "Games played: " << games_played << " res: " << rate(total_res, games_played) << '\n';
        return total_res;
    }

    int heuristics(Jungle *state){
        int res = 0;
        for(auto [x, y] : state->pieces[main_player]){
            res += dist(x, y, (main_player ? upper_den : lower_den));
        }
        return res;
    }

    inline int dist(int x, int y, pair<int, int> den){
        return sqrt((x+den.first)*(x+den.first) + (y+den.second)*(y+den.second));
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
    int main_player;
public:
    MCTS(){
        srand(time(NULL));
        tree[Jungle(0).hash] = Node();
        tree[Jungle(1).hash] = Node();
    }

    pair<int, pair<int, int>> gen_next_move(Jungle* state){            // {piece, {dirx, diry}}
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
                //! Jungle *temp = &state->gen_next_state(piece, dir);
                // if(ucb1(temp, here->times_sampled) > max_ucb){
                //     best_state = temp;
                // }
            }
            state = best_state;
        }


        if(tree[state->hash].times_sampled == 0) rollout(state);
        else{   // node expansion
            vector<pair<int, pair<int, int>>> legal_moves = state->get_legal_moves();
            Jungle *temp;
            for(auto [piece, dir] : legal_moves){       // create new nodes for each possible move
                //! temp = &(state->gen_next_state(piece, dir));
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
            //! state = &(state->gen_next_state(legal_moves[random].first, legal_moves[random].second));

            legal_moves = state->get_legal_moves();
        }
        return state->result(main_player);
    }
};



int main() {
    bool display = false;
    srand(time(NULL));
    Jungle game;
    Random ai0;
    zad3AI ai1;
    int MAX_GAMES = 10;
    int win_counter[2] = {0, 0};

    while(MAX_GAMES--){
        int turn = rand()%2;
        if(turn) game.swap_players();
        int turns = 0;

        if(display) cout << game << "\n\n";
        while(not game.game_won()){
            turns++;
            if(turn){
                auto move = ai0.gen_next_move(&game);
                if(display) cout << "ai1_cap move: " << AnimalNames[move.first] << ' ' << move.second.first << ' ' << move.second.second << ' '; 
                auto [myxs, myys] = game.pieces[game.player][move.first];
                if(display) cout << "(" << myxs << ' ' << myys << ' ' << myxs+move.second.first << ' ' << myys+move.second.second << ")\n";

                game.execute_move(move);
                turn = 1-turn;
            } else{
                auto move = ai1.gen_next_move(&game);
                if(display) cout << "ai2_low move: " << AnimalNames[move.first] << ' ' << move.second.first << ' ' << move.second.second << ' '; 
                auto [myxs, myys] = game.pieces[game.player][move.first];
                if(display) cout << "(" << myxs << ' ' << myys << ' ' << myxs+move.second.first << ' ' << myys+move.second.second << ")\n";

                game.execute_move(move);
                turn = 1-turn;
            }
            if(display) cout << game << '\n';
        }
        win_counter[1-turn]++;
        cout << "ai" << 1-turn << " won in " << turns << "turns\n" << game << '\n';

        game.reset();
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
            game.execute_move(game.get_piece_type(xs, ys), {xd, yd});

            auto [piece, dir] = ai.gen_next_move(&game);
            auto [myxs, myys] = game.pieces[game.player][piece];
            ido(myxs, myys, myxs+dir.first, myys+dir.second);
            game.execute_move(piece, dir);
        } else if(cmd == "UGO"){
            double time_for_move, time_for_game; 
            cin >> time_for_move >> time_for_game;

            auto [piece, dir] = ai.gen_next_move(&game);
            auto [myxs, myys] = game.pieces[game.player][piece];
            ido(myxs, myys, myxs+dir.first, myys+dir.second);
            game.execute_move(piece, dir);
        } else if(cmd == "ONEMORE"){
            game.reset();
            rdy();
        } else if(cmd == "BYE"){
            break;
        }
    }
}

