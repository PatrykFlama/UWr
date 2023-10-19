import collections


def pos_type(lab, pos):
    if pos[0] < 0 or pos[0] >= len(lab) or pos[1] < 0 or pos[1] >= len(lab[0]) or lab[pos[0]][pos[1]] == 'X':
        return 'WALL'
    if pos[0] == 0 or pos[0] == len(lab) - 1 or pos[1] == 0 or pos[1] == len(lab[0]) - 1:
        return 'EXIT'
    return 'EMPTY'

vis = set()
moves = [(0, 1), (1, 0), (0, -1), (-1, 0)]

def solve(lab, pos):      # returns deque of pairs=steps
    vis.add(pos)

    for move in moves:
        next_pos = (pos[0] + move[0], pos[1] + move[1])
        if pos_type(lab, (next_pos[0], next_pos[1])) == 'WALL' or (next_pos[0], next_pos[1]) in vis:
            continue

        if pos_type(lab, (next_pos[0], next_pos[1])) == 'EXIT':
            return collections.deque([(pos[0], pos[1]), (next_pos[0], next_pos[1])])
        if pos_type(lab, (next_pos[0], next_pos[1])) == 'EMPTY':
            steps = solve(lab, (next_pos[0], next_pos[1]))
            if steps:
                steps.appendleft((pos[0], pos[1]))
                return steps
            
    return None

def print_solution(lab, pos):
    vis.clear()
    steps = solve(lab, pos)

    if steps is None:
        print('No solution')
        return
    
    print(steps)
    for x in range(len(lab)):
        for y in range(len(lab[0])):
            if (x, y) in steps or (x, y) == pos:
                print('o', end='')
            else:
                print(lab[x][y], end='')
        print()

# ---- test -----
lab = [
    'XXXXXXXXXXXX',
    '           X',
    'XXXXXXXX X X',
    'X   X    X X',
    'X XXXXXX X X',
    'X     X  X X',
    'X XX XXX X X',
    'X X      X X',
    'X XXXXXXXXXX',
    'X          X',
    'XXXXXXXXXX X'
]
print_solution(lab, (1, 0))
