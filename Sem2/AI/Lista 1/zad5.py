import numpy as np
import random

R = 7      # picture row size
C = 7      # picture col size
picture = np.array([[False]*R]*C)
row_val = [0]*R
col_val = [0]*C
row_correct = [False]*R
col_correct = [False]*C
iterations_per_draw = 1000

def print_picture():
    for i in range(R):
        for j in range(C):
            if picture[i][j]: print('#', end='')
            else: print('.', end='')
        print()

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
    while row < C and not picture[col][row]: row += 1
    while row < C and picture[col][row]:
        row += 1
        sum += 1
    while row < C and not picture[col][row]: row += 1

    if row < C: return False        # if there are multiple blocks
    if sum != row_val[col]: return False       # inequality of sum
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

def main():
    print_picture()
    print(draw_incorrect_row())
    print(R)
    print(row_val)
    print(C)
    print(col_val)

def fix_pixel_in_row(row):  # TODO find good way to determine which cell should be repaired
    pass


def try_to_solve():
    picture = random_picture()
    for t in range(iterations_per_draw):
        row = draw_incorrect_row
        if row == -1:
            if check_cols(): return True, picture
            fix_pixel_in_row(random()%R)
        else: fix_pixel_in_row(row)
    return False, picture
        

def solve():
    solved = False
    while not solved:
        solved, picture = try_to_solve()


def read_input():
    with open("zad5_input.txt", "r") as input:
        s = input.readline().split()
        R = (int)(s[0])
        C = (int)(s[1])
        for i in range(R): row_val[i] = (int)(input.readline())
        for i in range(C): col_val[i] = (int)(input.readline())
        return R, C

R, C = read_input()
main()