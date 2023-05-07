#!/usr/bin/env python
# -*- coding: UTF-8 -*-


import copy
import random
import sys

INT_MAX = sys.maxsize
INT_MIN = -sys.maxsize - 1


class Reversi:
    M = 8
    DIRS = [(0, 1), (1, 0),   (-1, 0), (0, -1),
            (1, 1), (-1, -1), (1, -1), (-1, 1)]

    def __init__(self):
        self.board = self.initial_board()
        self.fields = set()
        self.move_list = []
        for i in range(self.M):
            for j in range(self.M):
                if self.board[i][j] is None:
                    self.fields.add((j, i))

    def __hash__(self):
        return hash(tuple([tuple(x) for x in self.board]))

    def initial_board(self):
        B = [[None] * self.M for _ in range(self.M)]
        B[3][3] = 1
        B[4][4] = 1
        B[3][4] = 0
        B[4][3] = 0
        return B

    def draw(self):
        for i in range(self.M):
            res = []
            for j in range(self.M):
                b = self.board[i][j]
                if b is None:
                    res.append('.')
                elif b == 1:
                    res.append('#')
                else:
                    res.append('o')
            print(''.join(res))
        print('')

    def moves(self, player):
        res = []
        for (x, y) in self.fields:
            if any(self.can_beat(x, y, direction, player)
                   for direction in self.DIRS):
                res.append((x, y))
        return res

    def can_beat(self, x, y, dir, player):
        dx, dy = dir
        x += dx
        y += dy
        cnt = 0
        while self.get(x, y) == 1 - player:
            x += dx
            y += dy
            cnt += 1
        return cnt > 0 and self.get(x, y) == player

    def get(self, x, y):
        if 0 <= x < self.M and 0 <= y < self.M:
            return self.board[y][x]
        return None

    def do_move(self, move, player):
        assert player == len(self.move_list) % 2
        self.move_list.append(move)

        if move is None:
            return
        x, y = move
        x0, y0 = move
        self.board[y][x] = player
        self.fields -= set([move])
        for dx, dy in self.DIRS:
            x, y = x0, y0
            to_beat = []
            x += dx
            y += dy
            while self.get(x, y) == 1 - player:
                to_beat.append((x, y))
                x += dx
                y += dy
            if self.get(x, y) == player:
                for (nx, ny) in to_beat:
                    self.board[ny][nx] = player

    def result(self):
        res = 0
        for y in range(self.M):
            for x in range(self.M):
                b = self.board[y][x]
                if b == 0:
                    res -= 1
                elif b == 1:
                    res += 1
        return res

    def terminal(self):
        if not self.fields:
            return True
        if len(self.move_list) < 2:
            return False
        return self.move_list[-1] is None and self.move_list[-2] is None

    def next_state(self, move, player):
        res = copy.deepcopy(self)
        res.do_move(move, player)
        return res


class Minimax(object):
    def __init__(self):
        self.hashmap = dict()

    def cutoff_test(self, state, depth):
        return depth > 2

    def rollout(self, state, player):
        while not state.terminal():
            moves = state.moves(player)
            if len(moves) == 0:
                state = state.next_state(None, player)
            else:
                state = state.next_state(random.choice(moves), player)
            player = 1 - player
        return state.result()

    def heuristic(self, state, player, samples=1):
        res = 0
        for _ in range(samples):
            res += self.rollout(state, player)
        return res / samples

    def min_move(self, state, depth, player, alpha):
        if state.terminal():
            return state.result()
        if self.cutoff_test(state, depth):
            return self.heuristic(state, player)
        
        moves = state.moves(player)
        if len(moves) == 0:
            next_move = state.next_state(None, player)
            return self.max_move(next_move, depth+1, 1-player, INT_MAX)

        min_score = INT_MAX
        for move in moves:
            next_move = state.next_state(move, player)

            score = self.max_move(next_move, depth+1, 1-player, min_score)
            if score < alpha:
                min_score = alpha
                break
            min_score = min(min_score, score)
                
        return min_score

    def max_move(self, state, depth, player, beta):
        if state.terminal():
            return state.result()
        if self.cutoff_test(state, depth):
            return self.heuristic(state, player)
        
        moves = state.moves(player)
        if len(moves) == 0:
            next_move = state.next_state(None, player)
            return self.min_move(next_move, depth+1, 1-player, INT_MIN)
        
        max_score = INT_MIN
        for move in moves:
            next_move = state.next_state(move, player)

            score = self.min_move(next_move, depth+1, 1-player, max_score)
            if score > beta:
                max_score = beta
                break
            max_score = max(max_score, score)

        return max_score

    def minimax(self, state, player):
        moves = state.moves(player)

        if len(moves) == 0:
            return None
        
        max_score = INT_MIN
        best_move = None
        for move in moves:
            next_move = state.next_state(move, player)

            score = self.min_move(next_move, 0, 1-player, max_score)
            if score > max_score:
                max_score = score
                best_move = move

        return best_move


class Player(object):
    def __init__(self):
        self.reset()

    def reset(self):
        self.game = Reversi()
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
        CORNERS = { (0,0), (0,7), (7,0), (7,7) }
        minimax = Minimax()

        while True:
            cmd, args = self.hear()
            if cmd == 'HEDID':
                unused_move_timeout, unused_game_timeout = args[:2]
                move = tuple((int(m) for m in args[2:]))
                if move == (-1, -1):
                    move = None
                self.game.do_move(move, 1 - self.my_player)
            elif cmd == 'ONEMORE':
                self.reset()
                continue
            elif cmd == 'BYE':
                break
            else:
                assert cmd == 'UGO'
                assert not self.game.move_list
                self.my_player = 0

            moves = self.game.moves(self.my_player)
            better_moves = list(set(moves) & CORNERS)

            if better_moves:
                move = random.choice(better_moves)
                self.game.do_move(move, self.my_player)
            elif moves:
                move = minimax.minimax(self.game, self.my_player)
                self.game.do_move(move, self.my_player)
            else:
                self.game.do_move(None, self.my_player)
                move = (-1, -1)
            self.say('IDO %d %d' % move)


if __name__ == '__main__':
    player = Player()
    player.loop()
