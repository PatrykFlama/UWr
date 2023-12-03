import matplotlib.pyplot as plt
import matplotlib.animation as animation
import numpy as np

h, w = 10, 20
board = [[1 for _ in range(h)] for _ in range(w)]  # board of 1's
colors = {0 : 'black', 1 : 'white'}

directions = [(0, 1), (1, 0), (0, -1), (-1, 0)]  # UP, RIGHT, DOWN, LEFT
direction = 0
ant_x, ant_y = w//2, h//2       # ant starting position
initial_moves = 1000

fig, ax = plt.subplots()
ax.set_xlim(0, w)
ax.set_ylim(0, h)
ax.set_aspect('equal', adjustable='box')

def draw_square(x, y, color):
    ax.add_patch(plt.Rectangle((x, y), 1, 1, fc=color))


for t in range(initial_moves):
    if board[ant_x][ant_y] == 1:
        direction = (direction + 1) % 4
        board[ant_x][ant_y] = 0
    else:
        direction = (direction - 1) % 4
        board[ant_x][ant_y] = 1

    ant_x += directions[direction][0]
    ant_y += directions[direction][1]
    ant_x = ant_x % w
    ant_y = ant_y % h

for x in range(w):
    for y in range(h):
        draw_square(x, y, colors[board[x][y]])
draw_square(ant_x, ant_y, 'green')

def update(t):
    global ant_x, ant_y, w, h, board, direction, directions

    if board[ant_x][ant_y] == 1:
        direction = (direction + 1) % 4
        board[ant_x][ant_y] = 0
    else:
        direction = (direction - 1) % 4
        board[ant_x][ant_y] = 1
    draw_square(ant_x, ant_y, colors[board[ant_x][ant_y]])
    
    ant_x += directions[direction][0]
    ant_y += directions[direction][1]
    ant_x = ant_x % w
    ant_y = ant_y % h
    draw_square(ant_x, ant_y, 'green')


ani = animation.FuncAnimation(fig, update, frames=5, interval=200, repeat=True)
plt.show()
