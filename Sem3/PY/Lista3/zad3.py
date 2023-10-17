import collections


def pos_type(lab, pos):
    if pos[0] < 0 or pos[0] >= len(lab) or pos[1] < 0 or pos[1] >= len(lab[0]) or lab[pos[0]][pos[1]] == 'X':
        return 'WALL'
    if pos[0] == 0 or pos[0] == len(lab) - 1 or pos[1] == 0 or pos[1] == len(lab[0]) - 1:
        return 'EXIT'
    return 'EMPTY'

def solve(lab, start):      # returns deque of pairs=steps
    pos = start
    steps = collections.deque()

    moves = [(0, 1), (1, 0), (0, -1), (-1, 0)]
    for move in moves:
        if pos_type(lab, (pos[0] + move[0], pos[1] + move[1])) == 'EXIT':
            steps.append((pos[0] + move[0], pos[1] + move[1]))
            return steps
        if pos_type(lab, (pos[0] + move[0], pos[1] + move[1])) == 'EMPTY':
            steps.append((pos[0] + move[0], pos[1] + move[1]))
            pos = (pos[0] + move[0], pos[1] + move[1])
            solve(lab, pos)