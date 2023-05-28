#!/usr/bin/env python
# -*- coding: UTF-8 -*-

import random
import sys
import chess

WHITE = 1       # True
BLACK = 0       # False


class Chess:
    def __init__(self):
        self.board = chess.Board()

    def reset(self):
        self.board.reset()
    
    def make_move(self, uci_move):  # assessment: move is legal
        move = chess.Move.from_uci(uci_move)
        self.board.push(move)
        
    def get_legal_moves(self):
        return [str(m) for m in self.board.legal_moves]

    def terminal(self):
        return self.board.is_game_over()
    
    def gen_next_state(self, move):
        next_state = Chess()
        next_state.board = self.board.copy()
        next_state.make_move(move)
        return next_state

    def result(self, player):
        out = self.board.outcome()
        if out is None:
            return None
        if out.winner is None:
            return 0
        if out.winner == WHITE:
            return (+1 if player == WHITE else -1)
        else:
            return (-1 if player == BLACK else +1)
    
    def heuristic_result(self, player):
        WIN_WEIGHT = 1e6
        DRAW_WEIGHT = 1e5
        MATERIAL_WEIGHT = 100
        MOBILITY_WEIGHT = 50

        #region material
        material_res = 0
        piece_values = {
            chess.PAWN: 1,
            chess.KNIGHT: 3,
            chess.BISHOP: 3,
            chess.ROOK: 5,
            chess.QUEEN: 9,
            chess.KING: 0
        }
        for piece in chess.PIECE_TYPES:
            material_res += piece_values[piece] * len(self.board.pieces(piece, WHITE))
            material_res -= piece_values[piece] * len(self.board.pieces(piece, BLACK))
        #endregion

        #region mobility
        mobility_res = 0
        legal_moves = self.get_legal_moves()

        # we prefer pieces to be closer board center, as it gives as more mobility
        center_weights = {"e4": 10, "e5": 10, "d4": 10, "d5": 10, "c6": 5, "d6": 5, "e6": 5, "f6": 5,
                          "c3": 5, "d3": 5, "e3": 5, "f3": 5, "c4": 5, "c5": 5, "f4": 5, "f5": 5}
        
        for move in legal_moves:
            if self.board.turn == player:
                if move[2:4] in center_weights:
                    mobility_res += center_weights[move[2:4]]
            else:
                if move[2:4] in center_weights:
                    mobility_res -= center_weights[move[2:4]]
        #endregion

        #region pawn structure #TODO
        # idea from github.com/lamesjim/Chess-AI
        
        #endregion

        #region checkmate #TODO

        #endregion

        #region outcome
        outcome = 0
        out = self.board.outcome()
        if out is not None:
            if out.winner is None:
                outcome += DRAW_WEIGHT
            if out.winner == WHITE:
                outcome += WIN_WEIGHT * (+1 if player == WHITE else -1)
            else:
                outcome += WIN_WEIGHT * (-1 if player == BLACK else +1)

        #endregion

        return outcome + MOBILITY_WEIGHT*mobility_res + MATERIAL_WEIGHT*material_res
    

class AI(object):
    def __init__(self):
        self.main_player = BLACK
        self.MAX_DEPTH = 2
        self.MIN = -1e9
        self.MAX = 1e9
    
    def update_main_player(self, player): 
        self.main_player = player
    
    def get_best_move(self, state):
        # legal_moves = state.get_legal_moves()
        # return random.choice(legal_moves)
        return self.alphabeta_root(state)

    def alphabeta_root(self, state):
        best_score = self.MIN
        best_move = [None]
        legal_moves = state.get_legal_moves()

        for move in legal_moves:
            next_state = state.gen_next_state(move)
            score = self.alphabeta(next_state, self.MAX_DEPTH, 1 - self.main_player, self.MIN, self.MAX)
            if score == best_score:
                best_move.append(move)
            elif score > best_score:
                best_score = score
                best_move = [move]

        return random.choice(best_move)

    def alphabeta(self, state, depth, player, alpha, beta):
        if state.terminal():
            return state.heuristic_result(self.main_player)

        if depth <= 0:
            return state.heuristic_result(self.main_player)

        if(player == self.main_player):     # maximizing player
            best_score = self.MIN
            legal_moves = state.get_legal_moves()

            for move in legal_moves:
                next_state = state.gen_next_state(move)
                score = self.alphabeta(next_state, depth - 1, 1 - player, alpha, beta)

                best_score = max(best_score, score)
                alpha = max(alpha, score)

                if alpha >= beta:
                    break
        else:                               # minimizing player
            best_score = self.MAX
            legal_moves = state.get_legal_moves()

            for move in legal_moves:
                next_state = state.gen_next_state(move)
                score = self.alphabeta(next_state, depth - 1, 1 - player, alpha, beta)

                best_score = min(best_score, score)
                beta = min(beta, score)

                if alpha >= beta:
                    break

        return best_score





class MAIN(object):
    def __init__(self):
        self.game = Chess()
        self.ai = AI()
        self.my_player = BLACK
        self.ai.update_main_player(self.my_player)
        self.say('RDY')

    def reset(self):
        self.game.reset()
        self.my_player = BLACK
        self.ai.update_main_player(self.my_player)
        self.say('RDY')

    def say(self, what):
        sys.stdout.write(what)
        sys.stdout.write('\n')
        sys.stdout.flush()

    def hear(self):
        line = sys.stdin.readline().split()
        return line[0], line[1:]

    def loop(self):
        while True:
            cmd, args = self.hear()
            if cmd == 'HEDID':
                unused_move_timeout, unused_game_timeout = args[:2]
                move = args[2]
                
                self.game.make_move(move)
            elif cmd == 'ONEMORE':
                self.reset()
                continue
            elif cmd == 'BYE':
                break
            else:
                assert cmd == 'UGO'
                #assert not self.game.move_list
                self.my_player = WHITE
                self.ai.update_main_player(self.my_player)

            move = self.ai.get_best_move(self.game)
            self.game.make_move(move)

            self.say('IDO ' + move)


if __name__ == '__main__':
    main = MAIN()
    main.loop()



