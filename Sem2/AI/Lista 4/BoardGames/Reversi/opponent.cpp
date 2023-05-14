#include <bits/stdc++.h>

using namespace std;

#define INF 1e9

int weight[][8] = {{20, -3, 11, 8, 8, 11, -3, 20},
{-3, -7, -4, 1, 1, -4, -7, -3},
{11, -4, 2, 2, 2, 2, -4, 11},
{8, 1, 2, -3, -3, 2, 1, 8},
{8, 1, 2, -3, -3, 2, 1, 8},
{11, -4, 2, 2, 2, 2, -4, 11},
{-3, -7, -4, 1, 1, -4, -7, -3},
{20, -3, 11, 8, 8, 11, -3, 20}};

int actualBoard[8][8];
struct State{
    int board[8][8]; // hashing table
};
int licznik = 0;
bool GameOver(State s){
    return false;
}

void initialize_positions(){
    for(int i=0; i<8; i++){
        for(int j=0; j<8; j++){
            actualBoard[i][j] = 0;
        }
    }
    actualBoard[3][3] = 1;
    actualBoard[4][4] = 1;
    actualBoard[3][4] = -1;
    actualBoard[4][3] = -1;
}
void initialize_positionsNIGIGER(){
    for(int i=0; i<8; i++){
        for(int j=0; j<8; j++){
            actualBoard[i][j] = 0;
        }
    }
    actualBoard[3][3] = -1;
    actualBoard[4][4] = -1;
    actualBoard[3][4] = 1;
    actualBoard[4][3] = 1;
}

bool onBoard(int x, int y){
    return x >= 0 and y >= 0 and x < 8 and y < 8;
}

void propagateMove(int x, int y, int deltaX, int deltaY){
    bool is_possible = true;
    if(onBoard(x + deltaX, y + deltaY) and actualBoard[x + deltaX][y + deltaY] != -actualBoard[x][y]){
        is_possible = false; // sprawdzamy, czy 1 pionek w danym kierunku da się zamienić
    }
    int x1 = x + deltaX, y1 = y + deltaY;
    while(true){ // sprawdzamy czy i ile da się zamienic
        if(!onBoard(x1, y1) || actualBoard[x1][y1] == 0){ // poza mapa lub puste pole
            is_possible = false;
            break;
        }
        if(actualBoard[x][y] == actualBoard[x1][y1]){ //znalezlismy pole z naszym pionkeim
            break;
        }
        x1 += deltaX, y1 += deltaY; // pionek przeciwnika, więc próbujemy dalej
    }
    if(is_possible){ // zamieniamy
        x1 = x + deltaX, y1 = y + deltaY;
        //cout << x1 << y1 << "\n";
        while(true){
            if(actualBoard[x][y] == actualBoard[x1][y1]){
                break;
            }
            actualBoard[x1][y1] *= -1;
            x1 += deltaX;
            y1 += deltaY;
        }
    }
}

void printBoard(int pos[8][8]){
    for(int i=0; i<8; i++){
        for(int j=0; j<8; j++){
            if(pos[i][j] == 1){
                cout << "1";
            }
            else if(pos[i][j] == 0){
                cout << ".";
            }
            else{
                cout << "0";
            }
        }
        cout << "\n";
    }
    cout << "\n";
}
void makeMove(int x, int y, bool maxPlayer){
    if(actualBoard[x][y] != 0){
        return;
        //throw logic_error("Attempt of placing a pawn on an occupied square!");
    }
    if(maxPlayer){
        actualBoard[x][y] = 1;
    }
    else{
        actualBoard[x][y] = -1;
    }
    for(int i=0; i<3; i++){
        for(int j=0; j<3; j++){
            if(i == 1 and j == 1){
                continue;
            }
            propagateMove(x, y, i - 1, j - 1);
        }
    }

}

int static_eval(int pos[8][8]){
    int res = 0;
    for(int i=0; i<8; i++){
        for(int j=0; j<8; j++){
            res += (pos[i][j] * weight[i][j]);
        }
    }
    return res;
}


bool possibleMove(int board[8][8], int x, int y, int maxPlayer){

    if(board[x][y] != 0){
        return false;
    }
    for(int i=0; i<3; i++){
        for(int j=0; j<3; j++){
            if(i == 1 and j == 1){
                continue;
            }
            int deltaX = i - 1, deltaY = j - 1;
            int x1 = x + deltaX, y1 = y + deltaY;

            if(onBoard(x1, y1) and board[x1][y1] == -maxPlayer){
                while(true){ // sprawdzamy czy i ile da się zamienic
                    if(!onBoard(x1, y1) || board[x1][y1] == 0){ // poza mapa lub puste pole
                        break;
                    }
                    if(board[x1][y1] == maxPlayer){ //znalezlismy pole z naszym pionkeim
                        return true;
                    }
                     x1 += deltaX, y1 += deltaY; // pionek przeciwnika, więc próbujemy dalej
                }
            }
        }
    }
    return false;
}

vector<State> generatePossibleMoves(State s, int maxPlayer){
    // jesli max player to 1 jesli enemey to -1 powinno być
    vector<State> possibleStates;
    for(int i=0; i<8; i++){
        for(int j=0; j<8; j++){
            if(possibleMove(s.board, i, j, maxPlayer)){
                State newState;
                for(int k=0; k<8; k++){
                    for(int p = 0; p < 8; p++){
                        newState.board[k][p] = s.board[k][p];
                    }
                }
                newState.board[i][j] = maxPlayer;
                possibleStates.push_back(newState);
            }
        }
    }
    return possibleStates;
}


int Minimax(State s, int depth, int alpha, int beta, bool maxPlayer){
    if(GameOver(s)){
        if(maxPlayer){
            return INF;
        }
        else
            return -INF;
    }
    if(depth == 0){
        //cout << "C\n";
        //cout << (static_eval(s.board)) << "\n";
        //printBoard(s.board);
        licznik ++;
        return static_eval(s.board);
    }

    if(maxPlayer){
        int max_result = -INF;
        vector<State> optional_states = generatePossibleMoves(s, 1);
        for(auto new_state : optional_states){
            //cout << "A\n";
            int eval = Minimax(new_state, depth - 1, alpha, beta, false);
            max_result = max(max_result, eval);
            alpha = max(alpha, eval);
            if(beta <= alpha)
                break;
        }
        return max_result;
    }
    else{
        int min_result = INF;
        vector<State> optional_states = generatePossibleMoves(s, -1);
        for(auto new_state : optional_states){
            //cout << "B\n";
            int eval = Minimax(new_state, depth - 1, alpha, beta, true);
            min_result = min(min_result, eval);
            beta = min(beta, eval);
            if(beta <= alpha)
                break;
        }
        return min_result;  
    }
}

void f(){
    State s;
            for(int i=0; i<8; i++){
                for(int j=0; j<8; j++){
                    s.board[i][j] = actualBoard[i][j];
                }
            }
            int res = -INF;
            vector<State> v = generatePossibleMoves(s, 1);
            for(auto state : v){
                int q = Minimax(state, 2, -INF, INF, false);
                if(q > res){
                    res = q;
                    s = state;
                }
            }
            int x = -1;
            int y = -1;
            for(int i=0; i<8; i++){
                for(int j=0; j<8; j++){
                    if(s.board[i][j] != actualBoard[i][j]){
                        x = i; y = j;
                    }
                }
            }
            cout << "IDO " << y << " " << x << "\n";
            makeMove(x, y, true);
}


int main(){
    

    initialize_positions();
    cout << "RDY\n";

    while(true){
        string message; cin >> message;
        if(message == "BYE"){
            break;
        }
        else if(message == "ONEMORE"){
            initialize_positions();
            cout << "RDY\n";
        }
        else if(message == "UGO"){
            double a, b; cin >> a >> b;
            initialize_positionsNIGIGER();
            f();
            //printBoard(actualBoard);   
            //cout << licznik << "\n"; 
        }
        else if(message == "HEDID"){
            double a, b; cin >> a >> b;
            int x, y; cin >> y >> x;
            if(x != -1 and y != -1){
                makeMove(x, y, false);
            }
            f();
            //printBoard(actualBoard);  
        }
        else{
            cout << "ZLY INPUT";
            break;
        }


    }

}
