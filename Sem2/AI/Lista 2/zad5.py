import random
import queue

board_walls = list()
board_start = set()
board_goal  = set()
# state is set of pairs of commandos positions

def read_board():
    row = 0
    with open("zad_input.txt", "r") as input:
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

dirs = [(1, 0), (-1, 0), (0, 1), (0, -1)]
dirs_names = "DURL"

def sum(a, b):
    return (a[0]+b[0], a[1]+b[1])

def move_safe(move):
    return (not board_walls[move[0]][move[1]]) and  (move[0] < len(board_walls) and move[0] >= 0) and \
                                                    (move[1] < len(board_walls[0]) and move[0] >= 0)

def make_move(state, dir):
    new_state = set()
    for commando in state:
        new_move = sum(commando, dir)
        if move_safe(new_move):
            new_state.add(new_move)
        else: new_state.add(commando)
    return new_state

def goal_reached(state):
    for commando in state:
        if commando not in board_goal: return False
    return True

def make_random_moves(start_state, count = 50, commandos_alive = 2):
    last_random_move = -1
    path = ""
    for i in range(0, count):
        random_move = random.randint(0, 3)
        while((last_random_move == 0 and random_move == 1) or (last_random_move == 2 and random_move == 3)):
            random_move = random.randint(0, 3)

        start_state = make_move(start_state, dirs[random_move])
        last_random_move = random_move
        path += dirs_names[random_move]

        if len(start_state) <= commandos_alive: break

    return (start_state, path)


def hash_state(state):      # we assume that there are max 3 commandos in state
    state = sorted(state)
    hash = 0
    mult = 1
    for i in state:
        hash += mult * ((i[0] * len(board_walls)) + i[1])
        mult *= len(board_walls) * len(board_walls[0])

    return hash

def bfs(state):
    q = []
    vis = [False] * (pow((len(board_walls)*len(board_walls[0])), 3) + 1000)      #diff
    q.append((state, ""))
    vis[hash_state(state)] = True      #diff
    
    while len(q) != 0:
        state, path = q.pop(0)
        if goal_reached(state): return (state, path)

        for d in range(0, 4):
            new_state = make_move(state, dirs[d])
            if not vis[hash_state(new_state)]:
                q.append((new_state, path+dirs_names[d]))
                vis[hash_state(new_state)] = True

    return (False, "")

def bfs_astar(state):
    pass


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

res = False
path1 = ""
path2 = ""    
while res == False:
    state = board_start

    max_comm = 2
    iter = 0
    while len(state) > max_comm:
        state, path1 = make_random_moves(board_start, 80)
        iter += 1
        if iter > 100: max_comm = 3

    # print_board(state)

    res, path2 = bfs(state)

    # print(res, path1 + path2)

output = open("zad_output.txt", "w")
output.write(path1 + path2)
output.close()
