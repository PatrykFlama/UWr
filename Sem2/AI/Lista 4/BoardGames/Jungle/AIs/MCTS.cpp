#pragma once
#include <bits/stdc++.h>
using namespace std;
#include "Jungle.cpp"


class Node{
public:
    vector<int> children;   // todo - actually use it
    bool is_leaf = true;
    int times_sampled = 0;
    int avg_value = 0;
};

class MCTS{
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

    void run_mcts_for(int miliseocnds, Jungle* state){
        main_player = state->player;
        while(miliseocnds--){       // todo - add time limit calculation
            mcts(state);
        }
    }

    void mcts(Jungle* state){
        // tree traversal phase:
        Node* here = &tree[state->hash];
        vector<Node*> path;
        pair<int, pair<int, int>> best_move;
        path.push_back(here);
        while(not here->is_leaf){       // get leaf node in mcts tree
            int max_ucb = INT_MIN;
            for(auto [piece, dir] : state->get_legal_moves()){
                int next_hash = state->gen_next_hash(piece, dir);
                int curr_ucb = ucb1(next_hash, here->times_sampled);
                if(curr_ucb > max_ucb){
                    best_move = {piece, dir};
                    max_ucb = curr_ucb;
                }
            }

            state->execute_move(best_move);
            here = &tree[state->hash];
            path.push_back(here);
        }


        int result;
        if(tree[state->hash].times_sampled == 0) result = rollout(state);
        else{   // node expansion
            tree[state->hash].is_leaf = false;
            vector<pair<int, pair<int, int>>> legal_moves = state->get_legal_moves();
            for(auto [piece, dir] : legal_moves){       // create new nodes for each possible move
                tree[state->gen_next_hash(piece, dir)] = Node();
            }
            if(legal_moves.size() == 0) result = rollout(state);      // do rollout for one of those new states
            else{
                Jungle next_state = state->gen_next_state(legal_moves[0].first, legal_moves[0].second);
                result = rollout(&next_state);
            }
        }

        // backpropagation phase:
        for(auto node : path){
            node->times_sampled++;
            // node->avg_value += (result - node->avg_value)/node->times_sampled;
            node->avg_value += result;
            if(node->times_sampled != 0) node->avg_value /= 2;
        }
    }

    int ucb1(int node_hash, int parent_visits){
        Node* here = &tree[node_hash];
        if(here->times_sampled == 0) return INT_MAX;
        return here->avg_value + 2*sqrt(log(parent_visits)/here->times_sampled);
    }

    int rollout(Jungle* state){ //todo
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