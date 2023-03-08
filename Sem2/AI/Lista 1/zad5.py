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

def check_col():
    pass

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
    print(row_val)
    print(col_val)

def fix_pixel_in_row(row):
    pass


def try_to_solve():
    pass

def solve():
    pass

def read_input():
    with open("zad5_input.txt", "r") as input:
        s = input.readline().split()
        R = (int)(s[0])
        C = (int)(s[1])
        for i in range(R): row_val[i] = (int)(input.readline())
        for i in range(C): col_val[i] = (int)(input.readline())

read_input()
main()