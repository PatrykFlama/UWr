#include <bits/stdc++.h>
using namespace std;


class Reversi{
public:
    #define int short int
    const int SIZE = 8;
    const int DIRS[8][2] = {{-1, -1}, {-1, 0}, {-1, 1}, {0, 1},
                            {1, 1},   {1, 0},  {1, -1}, {0, -1}};

    int *board;
    #define board(i, j) board[i*SIZE + j]
    vector<pair<int, int>> free_fields;
    vector<pair<int, int>> move_history;

    Reversi(){
        init_board();
        for(int i = 0; i < SIZE; i++)
            for(int j = 0; j < SIZE; j++)
                if(board(i, j) == -1) free_fields.push_back({i, j});
    }
    ~Reversi(){
        delete[] board;
    }
    Reversi &operator=(const Reversi &other){
        copy(other.board, other.board + SIZE*SIZE, board);
        free_fields = other.free_fields;
        move_history = other.move_history;
        return *this;
    }


    void init_board(){
        board = new int [SIZE*SIZE];
        fill(board, board + SIZE*SIZE, -1);
        board(3, 3) = 1;
        board(4, 4) = 1;
        board(3, 4) = 0;
        board(4, 3) = 0;
    }
    
    vector<pair<int, int>> moves(int player){
        vector<pair<int, int>> res;
        for(auto [x, y] : free_fields){
            for(auto [dx, dy] : DIRS){
                if(can_beat(x, y, {dx, dy}, player)){
                    res.push_back({x, y});
                    break;
                }
            }
        }
        return res;
    }
    
    bool can_beat(int x, int y, vector<int> dir, int player){
        int dx = dir[0], dy = dir[1];
        x += dx, y += dy;

        int cnt = 0;
        while(get(x, y) == 1 - player){
            x += dx;
            y += dy;
            cnt++;
        }
        return cnt > 0 and get(x, y) == player;
    }
    
    int get(int x, int y){
        if(0 <= x < SIZE and 0 <= y < SIZE)
            return board(y, x);
        return -1;
    }
    
    void do_move(pair<int, int> move, int player){
        assert(player == move_history.size() % 2);
        move_history.push_back(move);

        pair<int, int> null_move = {-1, -1};
        if(move == null_move) return;

        int x0 = move.first, y0 = move.second;
        board(y0, x0) = player;
        free_fields.erase(find(free_fields.begin(), free_fields.end(), move));
        
        for(auto [dx, dy] : DIRS){
            int x = x0, y = y0;
            vector<pair<int, int>> to_beat;
            x += dx; y += dy;
            while(get(x, y) == 1 - player){
                to_beat.push_back({x, y});
                x += dx; y += dy;
            }
            if(get(x, y) == player){
                for(auto [nx, ny] : to_beat){
                    board(ny, nx) = player;
                }
            }
        }
    }

    int32_t result(){
        int32_t res = 0;
        for(int i = 0; i < SIZE*SIZE; i++)
            if(board[i] == 0) res--;
            else if(board[i] == 1) res++;
        return res;
    }

    bool terminal(){
        if(free_fields.empty()) return true;
        if(move_history.size() < 2) return false;

        pair<int, int> null_move = {-1, -1};
        return move_history.back() == null_move and move_history[move_history.size() - 2] == null_move;
    }

    Reversi next_state(pair<int, int> move, int player){
        Reversi res = *this;        // TODO: is that copy + copy constructor
        res.do_move(move, player);
        return res;
    }


    #undef board
    #undef int
};


class Player{
public:
    #define int short int
    Reversi game;
    int my_player;

    Player(){
        reset();
    }

    void reset(){
        game = Reversi();
        my_player = 1;
        cout << "RDY" << endl;
    }

    void say(string what){
        cout << what << endl;
    }

    pair<string, vector<string>> hear(){
        string line;
        getline(cin, line);
        stringstream ss(line);
        string cmd;
        ss >> cmd;
        vector<string> args;
        string arg;
        while(ss >> arg) args.push_back(arg);
        return {cmd, args};
    }

    void loop(){
        set<pair<int, int>> CORNERS = { {0,0}, {0,7}, {7,0}, {7,7} };
        // Minimax minimax;

        while(true){
            auto [cmd, args] = hear();
            if(cmd == "HEDID"){
                int unused_move_timeout = stoi(args[0]);
                int unused_game_timeout = stoi(args[1]);
                pair<int, int> move = {stoi(args[2]), stoi(args[3])};
                if(move == pair<int, int>{-1, -1}) move = {-1, -1};
                game.do_move(move, 1 - my_player);
            }
            else if(cmd == "ONEMORE"){
                reset();
                continue;
            }
            else if(cmd == "BYE"){
                break;
            }
            else{
                assert(cmd == "UGO");
                assert(game.move_history.empty());
                my_player = 0;
            }

            vector<pair<int, int>> moves = game.moves(my_player);
            vector<pair<int, int>> better_moves;
            for(auto move : moves){
                if(CORNERS.find(move) != CORNERS.end())
                    better_moves.push_back(move);
            }

            pair<int, int> move;
            if(better_moves.size()){
                move = better_moves[rand() % better_moves.size()];
                game.do_move(move, my_player);
            }
            else if(moves.size()){
                // move = minimax.minimax(game, my_player);
                move = moves[rand() % moves.size()];
                game.do_move(move, my_player);
            }
            else{
                game.do_move({-1, -1}, my_player);
                move = {-1, -1};
            }
            say("IDO " + to_string(move.first) + " " + to_string(move.second));
        }
    }

    #undef int
};




int main(){
    Player player;
    player.loop();
}

