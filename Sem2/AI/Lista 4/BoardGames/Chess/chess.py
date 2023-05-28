#!/usr/bin/env python
# -*- coding: UTF-8 -*-

import random
import sys
import chess


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

    def result(self):
        out = self.board.outcome()
        if out is None:
            return None
        if out.winner is None:
            return 0
        if out.winner:
            return -1
        else:
            return +1

class AI(object):
    def __init__(self):
        pass
    
    def get_best_move(self, game):
        moves = game.get_legal_moves()
        return random.choice(moves)
    

class MAIN(object):
    def __init__(self):
        self.game = Chess()
        self.ai = AI()
        self.my_player = 1
        self.say('RDY')

    def reset(self):
        self.game.reset()
        self.my_player = 1
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
                
                self.game.update(move)
            elif cmd == 'ONEMORE':
                self.reset()
                continue
            elif cmd == 'BYE':
                break
            else:
                assert cmd == 'UGO'
                #assert not self.game.move_list
                self.my_player = 0

            move = self.ai.get_best_move(self.game)
            self.game.update(move)

            self.say('IDO ' + move)


if __name__ == '__main__':
    main = MAIN()
    main.loop()



