#include "bits/stdc++.h"

using namespace std;

using Clock = std::chrono::high_resolution_clock;

#define mask unsigned long long

const int DEPTH = 1;

#pragma region STUFF

const int DIRECTIONS = 8;
const int BOARD_SIZE = 64, BOARD_LEN = 8;

const int ROW[DIRECTIONS] = {-1, -1, 0, +1, +1, +1, 0, -1};
const int COL[DIRECTIONS] = {0, +1, +1, +1, 0, -1, -1, -1};

const int SHIFT[DIRECTIONS] = {-8, -7, +1, +9, +8, +7, -1, -9};

const mask STARTING_WHITE = (1LL << 27) | (1LL << 36);
const mask STARTING_BLACK = (1LL << 28) | (1LL << 35);

const mask CORNERS = 1 | (1LL << 7) | (1LL << 56) | (1LL << 63);

inline int count(mask m) {
    return __builtin_popcountll(m);
}

int lst(mask m) {
    return __builtin_ctzll(m);
}

mask shift(mask m, int d) {
    return (SHIFT[d] > 0 ? m << (+SHIFT[d]) : m >> (-SHIFT[d]));
}

int get_row(mask m) {
    return lst(m) / BOARD_LEN;
}

int get_col(mask m) {
    return lst(m) % BOARD_LEN;
}

mask get_mask(int row, int col) {
    if(row == -1 || col == -1)  return 0;
    return (1LL << (BOARD_LEN * row + col));
}

bool good_position(int row, int col) {
    return (0 <= row && row < BOARD_LEN) && (0 <= col && col < BOARD_LEN);
}

#pragma endregion STUFF

int CELL_SCORE[BOARD_LEN][BOARD_LEN] = {
        {20, -3, 11,  8,  8,  11, -3,  20},
        {-3, -7, -4,  1,  1, -4,  -7, -3},
        {11, -4,  2,  2,  2,  2,  -4 , 11},
        {8,   1,  2, -3, -3,  2,   1,  8},
        {8,   1,  2, -3, -3,  2,   1,  8},
        {11, -4,  2,  2,  2,  2,  -4,  11},
        {-3, -7, -4,  1,  1, -4,  -7, -3},
        {20, -3, 11,  8,  8,  11, -3,  20}};


class BitBoarding {
public:
    mask current_player, current_opponent;

    BitBoarding(bool starting = false) { reset(starting); } // domyślnie biały (tzn jestem drugi i jestem opponenetem)

    BitBoarding(mask _current_player, mask _current_opponent) : current_player(_current_player), current_opponent(_current_opponent) {}

    void reset(bool starting) {
        if(!starting) {
            current_player = STARTING_WHITE, current_opponent = STARTING_BLACK;
        } else {
            current_player = STARTING_BLACK, current_opponent = STARTING_WHITE;
        }
    }

    mask generate_moves() {
        mask moves = 0;
        mask empty = ~(current_player | current_opponent);
        for(int d = 0; d < DIRECTIONS; d++) {
            mask candidates = current_opponent & shift(current_player, d);
            while(candidates) {
                moves |= empty & shift(candidates, d);
                candidates = current_opponent & shift(candidates, d);
            }
        }
        return moves;
    }

    void make_move(mask move) {
        current_player |= move;
        mask empty = ~(current_player | current_opponent);
        for(int d = 0; d < DIRECTIONS; d++) {
            int row = get_row(move), col = get_col(move);
            int num = 0, flag = 0;
            mask cnt = move;
            while(good_position(row, col)) {
                row += ROW[d], col += COL[d];
                cnt = shift(cnt, d);
                num++;
                if(empty & cnt) { 
                    break;
                } else if(current_player & cnt) {
                    flag = 1;   break;
                }
            }
            row = get_row(move), col = get_col(move);
            cnt = move;
            for(int i = 0; i < num && flag; i++) {
                row += ROW[d], col += COL[d];
                cnt = shift(cnt, d);
                current_player |= cnt;
                current_opponent &= ~(cnt);
            }
        }
    }

    void swap_masks() {
        swap(current_player, current_opponent);
    }

};

class State {
public:
    bool player;
    // player - 1 oznacza, że JA jestem playerem, 0 że opponentem

    BitBoarding board;

    State(bool starting = false) { reset(starting); } // tzn, jestem na początku opponenetem
    State(BitBoarding _board, bool _player) : board(_board), player(_player) {}

    void reset(bool starting) {
        board = BitBoarding(starting);
        player = starting;
    }

    void make_action(mask action) {
        board.make_move(action);
        swap_players();
    }

    State make_new_state(mask action = 0) {
        State new_state = *this;
        new_state.make_action(action);
        return new_state;
    }

    void play_action(int row, int col) {
        make_action(get_mask(row, col));
    }

    mask get_player_actions(bool who = 1) { // 1 - aktualny gracz, 0 - następny gracz
        if(who == 0)    swap_players();
        mask result = board.generate_moves();
        if(who == 0)    swap_players();
        return result;
    }

    bool terminal() {
        return (get_player_actions(1) | get_player_actions(0)) == 0;
    }

    int utility() {
        return fix((count(board.current_player) - count(board.current_opponent)));
    }

    int heuristic_value() {
        int disks_ratio = ratio(count(board.current_player), count(board.current_opponent));
        return 10 * disks_ratio + 800 * eval_corners() + 80 * eval_positions();
    }

private:
    int fix(int value) {
        return value * (player == 1 ? +1 : -1);
    }

    void swap_players() {
        board.swap_masks();
        player = 1 - player;
    }

    int ratio(int player, int opponent) {
        if(player + opponent == 0)    return 0;
        return fix(100 * ((double)(player - opponent)) / (player + opponent));
    }

    int eval_positions() {
        int p0 = 0, p1 = 0;
        mask bit = 1;
        for(int i = 0; i < BOARD_SIZE; i++) {
            for(int j = 0; j < BOARD_SIZE; j++) {
                if(bit & board.current_player) p0 += CELL_SCORE[i][j];
                else if(bit & board.current_opponent) p1 += CELL_SCORE[i][j];
            }
        }
        return fix(p0 - p1);
    }

    int eval_corners() {
        int p0 = count(board.current_player & CORNERS);
        int p1 = count(board.current_opponent & CORNERS);
        if(p0 + p1 == 0)    return 0;
        return ratio(p0, p1);
    }

};

class AI {
    static const int MAX_DEPTH = DEPTH;
    static const bool MAXI = true, MINI = false;
    static const int INF = 1e9;
public:
    mask get_best_actions(State state) {
        return AlphaBetaRoot(state);
    }
    mask AlphaBetaRoot(State state) {
        int max_value = -INF;
        mask move = 0;
        mask actions = state.get_player_actions();
        while(actions) {
            mask action = actions & (~actions + 1);
            int value = AlphaBeta(state.make_new_state(action), MAX_DEPTH, MINI, -INF, +INF);
            if(value > max_value) {
                max_value = value;
                move = action;
            }
            actions &= actions - 1;
        }
        return move;
    }

    mask AlphaBeta(State state, int depth, bool maximizing_player, int alpha, int beta) {
        if(state.terminal()) {
            return state.utility();
        }
        mask actions = state.get_player_actions();
        if(actions == 0) {
            return AlphaBeta(state.make_new_state(), depth, !maximizing_player, alpha, beta);
        }
        if(depth == 0) {
            return state.heuristic_value();
        }
        int value = (maximizing_player ? -INF : +INF);
        while(actions) {
            mask action = actions & (~actions + 1);
            int cnt_value = AlphaBeta(state.make_new_state(action), depth - 1, !maximizing_player, alpha, beta);
            maximizing_player ?        
                value = max(cnt_value, value) : 
                value = min(cnt_value, value);
            maximizing_player ? 
            alpha = max(alpha, value) : 
            beta = min(beta, value);     
            if(alpha >= beta) {
                break;
            }
            actions &= actions - 1;
        }
        return value;
    }
};

void say(string s) {
    cout << s << "\n";
    fflush(stdout);
}

void play(mask move) {
    int row = -1, col = -1;
    if(move != 0) row = get_row(move), col = get_col(move);
    swap(row, col);
    printf("IDO %d %d\n", row, col);
    // cout << "IDO" << " " << row << " " << col << "\n";
    // cout << "(" << move << ")\n";
    fflush(stdout);
}

void wypisz(mask m) {
    cout << "----\n";
    for(int i = 0; i < BOARD_SIZE; i++) {
        if(m & (1LL << i)) {
            mask bit = (1LL << i);
            cout << get_row(bit) << " " << get_col(bit) << "(" << i << ")\n";
        }
    }
    cout << "----\n";
}

void wypisz_stan(State stan) {
    mask bit = 1;
    for(int row = 0; row < BOARD_LEN; row++) {
        for(int col = 0; col < BOARD_LEN; col++) {
            if(stan.player == 0) {
                if(stan.board.current_player & bit) {
                    cout << "1";
                } else if(stan.board.current_opponent & bit) {
                    cout << "2";
                } else {
                    cout << "0";
                }
            } else {
                if(stan.board.current_player & bit) {
                    cout << "2";
                } else if(stan.board.current_opponent & bit) {
                    cout << "1";
                } else {
                    cout << "0";
                }
            }
            bit <<= 1;
        }
        cout << "\n";
    }
}

int main() {
    AI ai;
    State Game;

    mask act = Game.get_player_actions();

    // wypisz(act);
    Game.reset(1);

    say("RDY");
    double t0, t1;
    while(true) {
        string str; cin >> str;
        if(str == "BYE") break;
        if(str == "UGO") {
            cin >> t0 >> t1;
            Game.reset(1);
            // wypisz_stan(Game);
            mask action = ai.get_best_actions(Game);
            Game.make_action(action);
            // wypisz_stan(Game);
            play(action);
        } else if(str == "HEDID") {
            cin >> t0 >> t1;
            int row, col;   cin >> row >> col;

            swap(row, col);

            Game.make_action(get_mask(row, col));

            mask action = ai.get_best_actions(Game);
            Game.make_action(action);
            play(action);

        } else if(str == "ONEMORE") {
            Game.reset(1);
            say("RDY");
        }
    }
}