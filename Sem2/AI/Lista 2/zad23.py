from collections import deque
from queue import PriorityQueue
from munkres import Munkres
from os import system
from time import sleep

class Vertex:
    def __init__(self, key, value):
        self.id = key
        self.value = value
        self.connected_to = set()

    def add_neighbor(self, nbr_key):
        self.connected_to.add(nbr_key)

    def set_value(self, value):
        self.value = value

class Graph:
    def __init__(self):
        self.verticles = {}
    
    def add_vertex(self, key, value):
        self.verticles[key] = Vertex(key, value)

    def add_edge(self, v1, v2):
        if v2 not in self.verticles[v1].connected_to:
            self.verticles[v1].add_neighbor(v2)
        if v1 not in self.verticles[v2].connected_to:
            self.verticles[v2].add_neighbor(v1)

    def get_neighbors_of(self, key):
        if key in self.verticles:
            return self.verticles[key].connected_to
        else:
            return None

    def contains(self, key):
        return key in self.verticles

    def get_vertex_value(self, key):
        return self.verticles[key].value

    def print_adjacency_list(self):
        for v in self.verticles:
            print(v, self.verticles[v].connected_to)
    
    def size(self):
        return len(self.verticles)

def state_to_board(current_state, goal_state, graph, rows, cols):
    board = []
    counter = 0
    placed_player = False
    boxes_placed = 0
    goals_placed = 0
    for y in range(rows):
        line = []
        for x in range(cols):
            node = graph.verticles[counter]
            if node.value == 'W':
                line.append(node.value)
            elif not placed_player and counter == current_state[0]:
                if current_state[0] in goal_state[1]:
                    line.append('+')
                    goals_placed += 1
                    placed_player = True
                else:
                    line.append('K')
                    placed_player = True
            elif boxes_placed != len(current_state[1]) and counter in current_state[1]:
                if node.id in goal_state[1]:
                    line.append('*')
                    goals_placed += 1
                else:
                    line.append('B')
                boxes_placed += 1
            elif goals_placed != len(goal_state[1]) and counter in goal_state[1]:
                line.append('G')
                goals_placed += 1
            else:
                line.append('.')
            counter += 1
        board.append(line)
    return board

def complete_adjacency_list(board, graph):
    for i in range(len(board)):
        for j in range(len(board[i])):
            if j - 1 >= 0:
                graph.add_edge(board[i][j], board[i][j-1])
            if j + 1 < len(board[i]):
                graph.add_edge(board[i][j], board[i][j+1])
            if i - 1 >= 0 and len(board[i-1]) > j:
                graph.add_edge(board[i][j], board[i-1][j])
            if i + 1 < len(board) and len(board[i+1]) > j:
                graph.add_edge(board[i][j], board[i+1][j])

def make_state(field, sub, prev_state, neighbors, graph):
    if field + sub in neighbors and graph.get_vertex_value(field + sub) != 'W' and field + sub not in prev_state[1]:
        boxes = set(prev_state[1])
        boxes.remove(field)
        boxes.add(field + sub)
        return (field, frozenset(boxes))
    return None

def generate_state(prev_state, field, graph, cols):
    if graph.get_vertex_value(field) == 'W':
        return None

    if field not in prev_state[1]:
        return (field, prev_state[1])

    neighbors = graph.get_neighbors_of(field)
    if not neighbors:
        return None

    diff = field - prev_state[0]
    if diff == 1:
        return make_state(field, 1, prev_state, neighbors, graph)
    elif diff == -1:
        return make_state(field, -1, prev_state, neighbors, graph) 
    elif diff < 0:
        for v in neighbors:
            if v < field and v%cols == field%cols:
                return make_state(field, v - field, prev_state, neighbors, graph)
    else:
        for v in neighbors:
            if v > field and v%cols == field%cols:
                return make_state(field, v - field, prev_state, neighbors, graph)

    return None

def reconstruct_path_states(came_from, current_state):
    total_path = [current_state]
    while current_state in came_from.keys():
        current_state = came_from[current_state]
        total_path.insert(0, current_state)
    return total_path

def reconstruct_path(came_from, current_state):
    total_path = ''
    while current_state in came_from.keys():
        prev_state = came_from[current_state]
        diff = current_state[0] - prev_state[0]

        if diff == 1:
            total_path = 'R' + total_path
        elif diff == -1:
            total_path = 'L' + total_path
        elif diff < 0:
            total_path = 'U' + total_path
        else:
            total_path = 'D' + total_path
        
        current_state = prev_state
    return total_path

def is_goal(current_state, goal_state):
    return current_state[1] == goal_state[1]

def number_to_cords(number, rows, cols):
    x = number%cols # column number
    y = (number - x) / cols # row number
    return int(x), int(y)

def manhattan(a, b, rows, cols):
    ax, ay = number_to_cords(a, rows, cols)
    bx, by = number_to_cords(b, rows, cols)
    return abs(ax - bx) + abs(ay - by)

def heuristic(state, goal_state, graph, rows, cols):
    # matrix for hungarian algorithm
    matrix = []

    for box in state[1]:
        row = []
        for goal in goal_state[1]:
            row.append(manhattan(box, goal, rows, cols))
        matrix.append(row)
    
    m = Munkres()
    indexes = m.compute(matrix)
    # the cheapest combination of paths
    total = 0
    for row, column in indexes:
        total += matrix[row][column]
    
    if total == 0:
        return total
    # calculate distance to the closest box - 1
    # player cords:
    min_dist = float('Inf')
    xp, yp = number_to_cords(state[0], rows, cols)
    for box in state[1]:
        x, y = number_to_cords(box, rows, cols)
        distance = abs(xp - x) + abs(yp - y)
        if distance < min_dist:
            min_dist = distance
    total += min_dist - 1
    return total

# A* i BFS do zad 2
# A* with heuristic based on hungarian algorithm
def A_star(graph, start_state, goal_state, rows, cols):
    came_from = {} # dict
    if is_goal(start_state, goal_state):
        return reconstruct_path(came_from, start_state)
    closed_set = set()
    g_score = {} # dict
    g_score[start_state] = 0
    h_score = {} # dict
    h_start = heuristic(start_state, goal_state, graph, rows, cols)
    if h_start == None:
        return None
    h_score[start_state] = h_start
    open_set = PriorityQueue()
    open_set.put((h_score[start_state], start_state))

    while not open_set.empty():
        # print('exploring new')
        # print('queue size:', open_set.qsize())
        current_state = open_set.get()[1]
        # print('taken new state')
        if is_goal(current_state, goal_state):
            return reconstruct_path(came_from, current_state)
        closed_set.add(current_state)
        # print('let us check its neighbors')
        for neighbor in graph.get_neighbors_of(current_state[0]):
            neighbor_state = generate_state(current_state, neighbor, graph, cols)
            # print('generated new state')
            if neighbor_state == None:
                continue
            # print('new state was good')
            g = g_score[current_state] + 1
            if neighbor_state not in g_score or g < g_score[neighbor_state]:
                # print('state doest exists or we found better way to it')
                g_score[neighbor_state] = g
                came_from[neighbor_state] = current_state
                if neighbor_state not in h_score:
                    h = heuristic(neighbor_state, goal_state, graph, rows, cols)
                    if h == None:
                        continue
                    h_score[neighbor_state] = h
                if neighbor_state not in closed_set:
                    open_set.put((g + h_score[neighbor_state], neighbor_state))
                    # print('Put new state on queue')
    return None

def BFS(graph, start_state, goal_state, cols):
    queue = deque()
    closed_set = set()
    came_from = {}
    queue.append(start_state)

    while queue:
        print('exploring new')
        current_state = queue.popleft()
        print('Taken new state')
        if is_goal(current_state, goal_state):
            return reconstruct_path(came_from, current_state)
        closed_set.add(current_state)
        print('let us check its neighbors')
        for neighbor in graph.get_neighbors_of(current_state[0]):
            neighbor_state = generate_state(current_state, neighbor, graph, cols)
            print('generated new state')
            if neighbor_state == None or neighbor_state in closed_set:
                continue
            print('new state was good')
            came_from[neighbor_state] = current_state
            queue.append(neighbor_state)
            print('Put new state on queue')
    return None


# Best first search do zad 3
def player_can_move_to_position(start_state, goal_position, graph, rows, cols):
    closed_set = set()
    open_set = PriorityQueue()
    open_set.put((manhattan(start_state[0], goal_position, rows, cols), start_state[0]))

    while not open_set.empty():
        current_position = open_set.get()[1]
        if current_position == goal_position:
            return True
        closed_set.add(current_position)
        for neighbor in graph.get_neighbors_of(current_position):
            if neighbor not in closed_set and neighbor not in start_state[1] and graph.get_vertex_value(neighbor) != 'W':
                open_set.put((manhattan(neighbor, goal_position, rows, cols), neighbor))
    return False

def make_state_none_player(field, sub, prev_state, box, neighbors, graph, rows, cols, go_to):
    if box + sub in neighbors and graph.get_vertex_value(box + sub) != 'W' and box + sub not in prev_state[1]:
        if player_can_move_to_position(prev_state, box + sub, graph, rows, cols):
            boxes = set(prev_state[1])
            boxes.remove(box)
            boxes.add(field)
            go_to[(box, frozenset(boxes))] = (box + sub, prev_state[1])
            return (box, frozenset(boxes))
    return None

def generate_state_none_player(prev_state, box, field, graph, rows, cols, go_to):
    if graph.get_vertex_value(field) == 'W':
        return None

    neighbors = graph.get_neighbors_of(field)
    if not neighbors:
        return None

    if field in prev_state[1]:
        return None

    neighbors = graph.get_neighbors_of(box)
    if not neighbors:
        return None
    diff = field - box
    if diff == 1:
        return make_state_none_player(field, -1, prev_state, box, neighbors, graph, rows, cols, go_to)
    elif diff == -1:
        return make_state_none_player(field, 1, prev_state, box, neighbors, graph, rows, cols, go_to) 
    elif diff < 0:
        for v in neighbors:
            if v > box and v%cols == box%cols:
                return make_state_none_player(field, v - box, prev_state, box, neighbors, graph, rows, cols, go_to)
    else:
        for v in neighbors:
            if v < box and v%cols == box%cols:
                return make_state_none_player(field, v - box, prev_state, box, neighbors, graph, rows, cols, go_to)

    return None

def path_finder(start_state, end_position, graph, rows, cols):
    closed_set = set()
    came_from = {}
    open_set = PriorityQueue()
    open_set.put((manhattan(start_state[0], end_position, rows, cols), start_state[0]))

    while not open_set.empty():
        current_position = open_set.get()[1]
        if current_position == end_position:
            return reconstruct_path(came_from, (current_position, start_state[1]))
        closed_set.add(current_position)
        for neighbor in graph.get_neighbors_of(current_position):
            if neighbor not in closed_set and neighbor not in start_state[1] and graph.get_vertex_value(neighbor) != 'W':
                came_from[(neighbor, start_state[1])] = (current_position, start_state[1])
                open_set.put((manhattan(neighbor, end_position, rows, cols), neighbor))
    return None

# def fill_the_gaps(go_to, came_from, start_state, current_state, rows, cols):
#     total_path = [current_state]
#     while current_state in came_from.keys():
#         prev_state = current_state
#         current_state = came_from[current_state]
#         # let's find all states between those two
#         path = path_finder(current_state, go_to[prev_state][0], graph, rows, cols)
#         if path == None:
#             return None
#         total_path = path + total_path
#     return total_path

def fill_the_gaps(go_to, came_from, start_state, current_state, rows, cols):
    total_path = reconstruct_path(go_to, current_state)
    while current_state in came_from.keys():
        prev_state = came_from[current_state]
        # let's find all states between those two
        path = path_finder(prev_state, go_to[current_state][0], graph, rows, cols)
        if path == None:
            return None
        total_path = path + total_path
        current_state = prev_state
    return total_path

def best_first_search(graph, start_state, goal_state, rows, cols):
    came_from = {} # dict
    go_to = {} # dict
    if is_goal(start_state, goal_state):
        return reconstruct_path(came_from, start_state)
    closed_set = set()
    h_score = {} # dict
    h_start = heuristic(start_state, goal_state, graph, rows, cols)
    if h_start == None:
        return None
    h_score[start_state] = h_start
    open_set = PriorityQueue()
    open_set.put((h_score[start_state], start_state))

    while not open_set.empty():
        # print('exploring new')
        # print('queue size:', open_set.qsize())
        current_state = open_set.get()[1]
        # print('taken new state')
        if is_goal(current_state, goal_state):
            print('Found goal state')
            return fill_the_gaps(go_to, came_from, start_state, current_state, rows, cols)
        closed_set.add(current_state)
        # print('let us check its neighbors')
        for box in current_state[1]:
            for neighbor in graph.get_neighbors_of(box):
                neighbor_state = generate_state_none_player(current_state, box, neighbor, graph, rows, cols, go_to)
                # print('generated new state')
                if neighbor_state == None:
                    continue
                # print('new state was good')
                if neighbor_state not in h_score:
                    h = heuristic(neighbor_state, goal_state, graph, rows, cols)
                    if h == None:
                        continue
                    h_score[neighbor_state] = h
                    came_from[neighbor_state] = current_state
                if neighbor_state not in closed_set and neighbor_state not in open_set.queue:
                    open_set.put((h_score[neighbor_state], neighbor_state))
                    # print('Put new state on queue')
    return None


Board = []
graph = Graph()
counter = 0
player = None
start = set()
end = set()

with open("zad_input.txt") as file: 
    rows = file.readlines()
    for row in rows:
        line = []
        for field in row:
            if field != '\n':
                # make start and goal states
                if field == 'K':
                    player = counter
                elif field == 'B':
                    start.add(counter)
                elif field == 'G':
                    end.add(counter)
                elif field == '*':
                    end.add(counter)
                    start.add(counter)
                elif field == '+':
                    end.add(counter)
                    player = counter
                
                if field == 'W':
                    graph.add_vertex(counter, field)
                else:
                    graph.add_vertex(counter, '.')
                line.append(counter)
                counter += 1
        Board.append(line)
start_state = (player, frozenset(start))
goal_state = (None, frozenset(end))

rows = len(Board)
cols = len(Board[0])

complete_adjacency_list(Board, graph)
solution = A_star(graph, start_state, goal_state, rows, cols)
# solution = BFS(graph, start_state, goal_state, cols)
# solution = best_first_search(graph, start_state, goal_state, rows, cols)
# system('clear')
with open('zad_output.txt', 'w') as file:
    if solution == None:
        file.write('Cannot find solution!')
        print('Cannot find solution!')
    else:
        # print('----------------------------------')
        # print('Solution found! Look into output.txt file.')
        # print('----------------------------------')
        file.write(solution)
        # i = 0
        # for state in solution:
        #     step = state_to_board(state, goal_state, graph, rows, cols)
        #     for line in step:
        #         l = ''
        #         for field in line:
        #             file.write(field)
        #             l += field
        #         file.write('\n')
        #         print(l)
        #     file.write('----------------------------------\n')
        #     sleep(1)
        #     system('clear')
        #     i += 1
        # file.write(f'Needed {i} steps to do solve.')