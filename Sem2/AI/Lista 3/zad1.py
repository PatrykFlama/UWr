# (that code is quite ugly and could be easily simplified by folding the code into functions, probably even with picture transposition)
import numpy as np
from itertools import combinations

R = 7      # picture row size
C = 7      # picture col size
row_val = [[]]    # how many how long blocks per row
col_val = [[]]    # how many how long blocks per col
row_correct = [False]*R
col_correct = [False]*C
iterations_per_draw = 5000
picture = np.array([[False]*C]*R)

def reset():
    row_correct = [False]*R
    col_correct = [False]*C
    picture = random_picture()
    return row_correct, col_correct, picture

def print_picture(picture = picture):
    with open("zad_output.txt", "w") as output:
        for i in range(R):
            for j in range(C):
                if picture[i][j]: output.write('#')
                else: output.write('.')
            output.write("\n")

def random_picture():
    return np.random.choice([True, False], size = (R, C))

def get_picture_col(col):
    res = []
    for row in range(0, R):
        res.append(picture[row][col])
    return res

def is_correct(domain_row, domain_col):   # if picture is correct <=> its rows and cols are in domain
    i = 0
    for row in picture:
        if tuple(row) not in domain_row[i]:
            return False
        i += 1
    for col in range(0, C):
        if tuple(get_picture_col(col)) not in domain_col[col]:
            return False
        
    return True


def get_possible_domains():    # generate all possibilities/domains for each row and column
    res = [[],[]]
    for row in row_val:
        act_res = []
        maxx = C - row[-1]
        for combination in combinations(range(maxx+1), len(row)):   # combination of block starts
            blocks_dont_overlap = True
            for cell in range(1, len(combination)):
                # generate all possible combinations (block placements)
                # and choose only those, which are correct
                if combination[cell-1] + row[cell-1] >= combination[cell]:
                    blocks_dont_overlap = False
                    break
            if blocks_dont_overlap: # color blocks in row
                new = [0] * C
                row_ptr = 0
                for cell in combination:
                    for j in range(cell, cell + row[row_ptr]):
                        new[j] = 1
                    row_ptr += 1
                act_res.append(new)
        res[0].append({tuple(i) for i in act_res})

    for col in col_val:
        act_res = []
        maxx = R - col[-1]
        for combination in combinations(range(maxx+1), len(col)):
            blocks_dont_overlap = True
            for cell in range(1, len(combination)):
                # generate all possible combinations (block placements)
                # and choose only those, which are correct
                if combination[cell-1] + col[cell-1] >= combination[cell]:
                    blocks_dont_overlap = False
                    break;

            if blocks_dont_overlap: # color blocks in col
                new = [0] * R
                col_ptr = 0
                for cell in combination:
                    for j in range(cell, cell + col[col_ptr]):
                        new[j] = 1
                    col_ptr += 1
                act_res.append(new)
        res[1].append({tuple(i) for i in act_res})
    return (res[0], res[1])

def domain_intersection(domain):    # calculate intersection of given domain for row/col
    intersetion1 = list(domain)[0]
    intersetion0 = list(domain)[0]

    for poss in domain:
        intersetion1 = [1 if intersetion1[i] == poss[i] and poss[i] == 1 
                          else 0 
                            for i in range(0, len(poss))]
        intersetion0 = [0 if intersetion0[i] == poss[i] and poss[i] == 0 
                          else 1 
                            for i in range(0, len(poss))]
            
    return (intersetion1, intersetion0)

def clear_domain(cells, domain, color, is_row):   # remove not fitting possibilities from domain
    if is_row:
        for r, c in cells:
            to_clear = []
            for poss in domain[c]:
                if poss[r] != color:
                    to_clear.append(poss)
            for rm in to_clear:
                domain[c] -= {rm}
    else:
        for r, c in cells:
            to_clear = []
            for poss in domain[r]:
                if poss[c] != color:
                    to_clear.append(poss)
            for rm in to_clear:
                domain[r] -= {rm}
    return domain


def solve_ac3():
    domain_row, domain_col = get_possible_domains()

    while not is_correct(domain_row, domain_col):
        # --- row ---
        colored_cells = set()
        blank_cells = set()

        r = 0
        for row in domain_row:  # select row
            c = 0
            
            intersection = domain_intersection(row)
            for cell in range(0, len(intersection[0])):   # intersection of all possibilities in that row
                if intersection[0][cell] == 1:
                    picture[r][c] = True
                    colored_cells.add((r, c))
                    
                if intersection[1][cell] == 0:
                    picture[r][c] = False
                    blank_cells.add((r, c))
                c += 1

            r += 1

        domain_col = clear_domain(colored_cells, domain_col, 1, True)
        domain_col = clear_domain(blank_cells, domain_col, 0, True)

        # --- col ---
        colored_cells = set()
        blank_cells = set()

        c = 0
        for col in domain_col:  # select col
            r = 0

            intersection = domain_intersection(col)
            for cell in range(0, len(intersection[0])):   # intersection of all possibilities in that col
                if intersection[0][cell] == 1:
                    picture[r][c] = True
                    colored_cells.add((r, c))

                if intersection[1][cell] == 0:
                    picture[r][c] = False
                    blank_cells.add((r, c))
                r += 1

            c += 1

        domain_row = clear_domain(colored_cells, domain_row, 1, False)
        domain_row = clear_domain(blank_cells, domain_row, 0, False)


def read_input():
    with open("zad_input.txt", "r") as input:
        s = input.readline().split()
        R = (int)(s[0])
        C = (int)(s[1])

        row_val = []
        col_val = []
        for i in range(R): row_val.append(list(map(int, input.readline().strip('\n').split())))
        for i in range(C): col_val.append(list(map(int, input.readline().strip('\n').split())))
        return R, C, row_val, col_val

R, C, row_val, col_val = read_input()
row_correct = [False]*R
col_correct = [False]*C
picture = np.array([[False]*C]*R)

solve_ac3()

print_picture(picture)

