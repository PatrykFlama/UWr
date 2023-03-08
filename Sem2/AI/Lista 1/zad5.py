import numpy as np
import random

R = 7      # picture row size
C = 7      # picture col size
picture = np.array([[False]*R]*C)
row_val = [0]*R
col_val = [0]*C
row_correct = [False]*R
col_correct = [False]*C
iterations_per_draw = 5000

def reset():
    row_correct = [False]*R
    col_correct = [False]*C
    picture = random_picture()
    return row_correct, col_correct, picture

def print_picture(picture = picture):
    output = open("zad5_output.txt", "w")
    for i in range(R):
        for j in range(C):
            if picture[i][j]: output.write('#')
            else: output.write('.')
        output.write('\n')
    output.close()

def random_picture():
    return np.random.choice([True, False], size = (R, C))

def check_row(row):     # true if row is correct
    col = 0
    sum = 0
    while col < C and not picture[row][col]: col += 1
    while col < C and picture[row][col]:
        col += 1
        sum += 1
    while col < C and not picture[row][col]: col += 1

    if col < C: return False        # if there are multiple blocks
    if sum != row_val[row]: return False       # inequality of sum
    return True

def check_col(col):
    row = 0
    sum = 0
    while row < C and not picture[row][col]: row += 1
    while row < C and picture[row][col]:
        row += 1
        sum += 1
    while row < C and not picture[row][col]: row += 1

    if row < C: return False        # if there are multiple blocks
    if sum != col_val[col]: return False       # inequality of sum
    return True

def check_rows():
    for i in range(R):
        if not check_row(i): return False
    return True

def check_cols():
    for i in range(C):
        if not check_col(i): return False
    return True

def draw_incorrect_row():
    rows = []
    for row in range(R):
        if not check_row(row):
            rows.append(row)
    if len(rows) > 0: return np.random.choice(rows)
    return -1

def draw_incorrect_col():
    cols = []
    for col in range(C):
        if not check_col(col):
            cols.append(col)
    if len(cols) > 0: return np.random.choice(cols)
    return -1

def main():
    print(R)
    print(row_val)
    print(C)
    print(col_val)

def fix_row(row):
    prefix = [0]*R
    len = row_val[row]
    maxx = 0
    ptr = 0

    if picture[row][0]: 
        prefix[0] = 1
    for i in range(1, len):
        prefix[i] = prefix[i-1]
        if picture[row][i]: prefix[i] += 1
    maxx = prefix[len-1]
    ptr = len-1

    for i in range(len, R):
        if i > 0: prefix[i] = prefix[i-1]
        if picture[row][i-len]: prefix[i] -= 1
        if picture[row][i]: prefix[i] += 1

        if prefix[i] > maxx:
            maxx = prefix[i]
            ptr = i
    
    for i in range(0, ptr-len+1): picture[row][i] = False
    for i in range(ptr-len+1, ptr+1): picture[row][i] = True
    for i in range(ptr+1, R): picture[row][i] = False

def fix_col(col):
    prefix = [0]*C
    len = col_val[col]
    maxx = 0
    ptr = 0

    if picture[0][col]: 
        prefix[0] = 1
    for i in range(1, len):
        prefix[i] = prefix[i-1]
        if picture[i][col]: prefix[i] += 1
    maxx = prefix[len-1]
    ptr = len-1

    for i in range(len, C):
        if i > 0: prefix[i] = prefix[i-1]
        if picture[i-len][col]: prefix[i] -= 1
        if picture[i][col]: prefix[i] += 1

        if prefix[i] > maxx:
            maxx = prefix[i]
            ptr = i
    
    for i in range(0, ptr-len+1): picture[i][col] = False
    for i in range(ptr-len+1, ptr+1): picture[i][col] = True
    for i in range(ptr+1, C): picture[i][col] = False

def try_to_solve():
    for t in range(iterations_per_draw):
        row = draw_incorrect_row()
        if row != -1: fix_row(row)

        col = draw_incorrect_col()
        if col != -1: fix_col(col)

        if check_cols() and check_rows(): return True, picture
    return False, picture

def read_input():
    with open("zad5_input.txt", "r") as input:
        s = input.readline().split()
        R = (int)(s[0])
        C = (int)(s[1])
        for i in range(R): row_val[i] = (int)(input.readline())
        for i in range(C): col_val[i] = (int)(input.readline())
        return R, C

R, C = read_input()

solved = False
while not solved:
    row_correct, col_correct, picture = reset()
    solved, picture = try_to_solve()

print_picture(picture)
