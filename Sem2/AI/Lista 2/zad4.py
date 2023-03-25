import random

board_walls = list()
board_start = set()
board_goal  = set()

def read_board():
    row = 0
    with open("zad4_input.txt", "r") as input:
        for line in input:
            line = (line.rstrip('\r')).rstrip('\n')
            if len(line) == 0: continue

            col = 0
            temp_walls = list()
            for i in line:
                if i == '#': temp_walls.append(True)
                else: temp_walls.append(False)

                if i == 'G': board_goal.add((row, col))
                elif i == 'S': board_start.add((row, col))
                elif i == 'B':
                    board_goal.add((row, col))
                    board_start.add((row, col))
                col += 1

            board_walls.append(temp_walls)
            row += 1

def sum(a, b):
    return (a[0]+b[0], a[1]+b[1])

def move_safe(move):
    return not board_walls[move[0]][move[1]]

def make_move(state, dir):
    new_state = set()
    for commando in state:
        new_move = sum(commando, dir)
        if move_safe(new_move):
            new_state.add(new_move)
        else: new_state.add(commando)
    return new_state

dirs = [(1, 0), (-1, 0), (0, 1), (0, -1)]

def make_random_moves(start_state, count = 50, commandos_alive = 5):
    last_random_move = -1
    for i in range(0, count):
        random_move = random.randint(0, 3)
        while((last_random_move == 0 and random_move == 1) or (last_random_move == 2 and random_move == 3)):
            random_move = random.randint(0, 3)

        start_state = make_move(start_state, dirs[random_move])
        last_random_move = random_move

        # if len(start_state) <= commandos_alive: return start_state
    return start_state


def goal_reached(state):
    for commando in state:
        if commando not in board_goal: return False
    return True

def bfs(start_state, max_depth = 15):
    q = []
    vis = []
    q.append((start_state, 0))
    max_len = len(start_state)

    while len(q) != 0:
        current_state = q.pop(0)
        depth = current_state[1]
        current_state = current_state[0]

        vis.append(current_state)
        if goal_reached(current_state): return current_state
        if len(current_state) > max_len: continue
        if depth > max_depth: continue

        # print("\n", current_state, "from", q)
        for d in dirs:
            new_state = make_move(current_state, d)
            # print(new_state, "in?", vis, new_state in vis)
            if new_state not in vis:
                max_len = max(max_len, len(new_state))
                q.append((new_state, depth+1))

        # print_board(current_state)
        # i = input()

    return False

def print_board(state):
    for row in range(0, len(board_walls)):
        for col in range(0, len(board_walls[row])):
            if board_walls[row][col]: print("#", end='')
            elif (row, col) in state and (row, col) in board_goal: print("B", end='')
            elif (row, col) in state: print("S", end='')
            elif (row, col) in board_goal: print("G", end='')
            else: print(" ", end='')

        print()


# --------- MAIN ---------
read_board()
print(board_goal)
board_start = make_random_moves(board_start, 150)
print_board(board_start)

print(bfs(board_start))

# for t in range(50, 131, 5):
#     avg = 0
#     for i in range(0, 10000):
#         avg += len(make_random_moves(board_start, t))
#         avg /= 2

#     print(t, avg)

"""
50 7.504882128705937
55 5.0261831877677725
60 8.71381990697462
65 5.041194659749196
70 3.9756192866807853
75 4.0198719750679714
80 2.7986840969685396
85 4.027713963916065
90 2.9102440109080057
95 4.013103733226822
100 4.093308143976234
105 3.5059002954630083
110 4.091924189420891
115 2.3464528164446543
120 3.22358637019109
125 3.2539674253346913
130 2.5826009632955107
"""
