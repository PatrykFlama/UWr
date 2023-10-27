import numpy as np


# region    EXAMPLES
# example sudoku board: (0/_ - empty)
# 5 3 4 6 7 8 9 _ 2
# 6 7 2 1 9 5 3 4 8
# 1 9 _ 3 4 2 _ 6 7
# 8 5 9 _ 6 1 4 _ _
# 4 2 _ 8 _ 3 7 9 _
# 7 1 3 _ _ _ 8 5 6
# 9 _ 1 5 3 _ 2 8 4
# 2 8 _ 4 1 9 6 3 5
# 3 4 5 2 8 6 1 7 9

# sudoku board with two solutions:
# 9 _ 6 _ 7 _ 4 _ 3
# _ _ _ 4 _ _ 2 _ _
# _ 7 _ _ 2 3 _ 1 _
# 5 _ _ _ _ _ 1 _ _
# _ 4 _ 2 _ 8 _ 6 _
# _ _ 3 _ _ _ _ _ 5
# _ 3 _ 7 _ _ _ 5 _
# _ _ 7 _ _ 5 _ _ _
# 4 _ 5 _ 1 _ 7 _ 8

# _ _ _ _ _ _ _ _ _
# _ _ _ _ _ _ _ _ _
# _ _ _ _ 2 3 _ 1 _
# 5 _ _ _ _ _ 1 _ _
# _ 4 _ _ _ 8 _ 6 _
# _ _ 3 _ _ _ _ _ _
# _ 3 _ 7 _ _ _ 5 _
# _ _ 7 _ _ 5 _ _ _
# 4 _ 5 _ 1 _ 7 _ _

# sudoku board with no solution (pseudoexample, as we should not give to the program sudoku that contains errors):
# 5 3 4 6 7 8 9 1 2
# 6 7 2 1 9 5 3 4 8
# 1 9 8 3 4 2 5 6 7
# 8 5 9 7 6 1 4 2 3
# 4 2 6 8 5 3 7 9 1
# 7 1 3 9 2 4 8 5 6
# 9 6 1 5 3 7 2 8 4
# 2 8 7 4 1 9 6 3 5
# 3 4 5 2 8 6 1 9 _

# endregion

# region    INPUT/OUTPUT
def read_sudoku():
    board = []

    for i in range(9):
        data = input()
        row = []

        for c in data:
            if c == ' ' or c == '\n':
                continue
            if c == '_':
                row.append(0)
            else:
                row.append(int(c))
        board.append(row)

        # board.append(list(map(int, input().split())))
    return board


def print_sudoku(board):
    for row in board:
        for c in row:
            print(c, end=' ')
        print()

# endregion

# region    CHECKS
def check_row(board, row, num):
    for i in range(9):
        if board[row][i] == num:
            return False
    return True


def check_col(board, col, num):
    for i in range(9):
        if board[i][col] == num:
            return False
    return True


def check_square(board, row, col, num):
    for i in range(3):
        for j in range(3):
            if board[i+row][j+col] == num:
                return False
    return True


def check(board, row, col, num):
    return check_row(board, row, num) and check_col(board, col, num) and check_square(board, row - row % 3, col - col % 3, num)


def empty_exists(board):
    for row in range(9):
        for col in range(9):
            if board[row][col] == 0:
                return True
    return False

# endregion


def _solutions_iterator(board):
    for row in range(9):
        for col in range(9):
            if board[row][col] == 0:
                for num in range(1, 10):
                    if check(board, row, col, num):
                        board[row][col] = num
                        yield from _solutions_iterator(board)
                        board[row][col] = 0
                return          # return, to not display unsolved sudoku
    yield board         # no empty slots => solution found => return it

def solutions_iterator(board):
    if all(False for _ in _solutions_iterator(board)):
        yield None
    else:
        return _solutions_iterator(board)



# -------- TEST --------
for solution in solutions_iterator(read_sudoku()):
    if solution is None:
        print("No solution")
        break
    print()
    print_sudoku(solution)


# microtests
# def unfun(t):
#     if all(False for _ in fun(t)):
#         yield 10
#     else:
#         yield from fun(t)


# def fun2():
#     yield 6
#     yield 7
#     yield 8


# def fun(t):
#     if t == 1:
#         return
#     yield 1
#     yield 2
#     yield from fun2()
#     yield 3

# for i in unfun(1):
#     print(i)