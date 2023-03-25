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

def make_random_moves(start_state, count):
    last_random_move = -1
    for i in range(0, count):
        random_move = random.randint(0, 3)
        while((last_random_move == 0 and random_move == 1) or (last_random_move == 2 and random_move == 3)):
            random_move = random.randint(0, 3)

        start_state = make_move(start_state, dirs[random_move])
        last_random_move = random_move
    return start_state


def goal_reached(state):
    for commando in state:
        if commando not in board_goal: return False
    return True

def bfs(start_state):
    q = []
    vis = set()
    q.append(tuple(start_state))
    print("Commandos:", len(start_state))

    while len(q) != 0:
        current_state = q.pop(0)
        vis.add(current_state)
        if goal_reached(current_state): return current_state

        for d in dirs:
            new_state = frozenset(make_move(current_state, d))
            if new_state not in vis:        # TODO that does not work # TODO basicly something is something and so something is not workign very well
                q.append(tuple(new_state))
    
    return False


# --------- MAIN ---------
read_board()
print(board_goal)
board_start = make_random_moves(board_start, 150)
print(bfs(board_start))
