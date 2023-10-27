import numpy as np

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

# sudoku board with no solution:
# 5 3 4 6 7 8 9 1 2
# 6 7 2 1 9 5 3 4 8
# 1 9 8 3 4 2 5 6 7
# 8 5 9 7 6 1 4 2 3
# 4 2 6 8 5 3 7 9 1
# 7 1 3 9 2 4 8 5 6
# 9 6 1 5 3 7 2 8 4
# 2 8 7 4 1 9 6 3 5
# 3 4 5 2 8 6 1 7 1


def read_sudoku():
    board = []
    
    for i in range(9):
        data = input()
        row = []

        for c in data:
            if c == ' ' or c == '\n': continue
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
    return check_row(board, row, num) and check_col(board, col, num) and check_square(board, row - row%3, col - col%3, num)

def empty_exists(board):
    for row in range(9):
        for col in range(9):
            if board[row][col] == 0:
                return True
    return False


def solutions_iterator(board):
    solution_exists = False # flag variable to keep track of whether a solution exists
    for row in range(9): 
        for col in range(9):
            if board[row][col] == 0:
                for num in range(1, 10):
                    if check(board, row, col, num):
                        board[row][col] = num
                        temp = solutions_iterator(board)
                        if temp is not None:
                            yield from temp
                            solution_exists = True # set flag variable to True if a solution is found
                        board[row][col] = 0
                return None
    if not solution_exists: # return None if no solution exists
        return None
    yield board


# -------- TEST --------
for solution in solutions_iterator(read_sudoku()):
    if solution is None:
        print("No solution found")
        break
    print()
    print_sudoku(solution)
