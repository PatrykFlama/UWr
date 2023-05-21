#pragma once
#include <bits/stdc++.h>
using namespace std;
#include "Jungle.cpp"


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