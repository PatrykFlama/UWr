import random
import math

class DartThrower:
    def __init__(self, x, y):
        self.x = x
        self.y = y

    def __str__(self):
        return f'({self.x}, {self.y})'
    
    def randomize(self):
        self.x = random.uniform(-1, 1)
        self.y = random.uniform(-1, 1)

    def in_circle(self):
        return self.x**2 + self.y**2 <= 1
    
    def throw(self):
        self.randomize()
        return self.in_circle()
    

epsilon = 0.000001
max_throws = 1000000
print_rate = 1

dart_thrower = DartThrower(0, 0)
total_throws = 0
in_circle = 0

while True:
    total_throws += 1
    if dart_thrower.throw():
        in_circle += 1

    PI = 4 * in_circle / total_throws
    if total_throws%print_rate == 0:
        print(f'pi = {PI} after {total_throws} throws')

    if(abs(PI - math.pi) <= epsilon):
        print('calculated pi is precise enough')
        print(f'(pi = {math.pi})')
        break

    if total_throws >= max_throws:
        print('max throws reached')
        break
