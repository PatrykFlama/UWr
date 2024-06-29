import random
import os
import subprocess
from tqdm import tqdm

# MAX = int(1e7)
# MIN = int(-1e7)
# MAX_POINTS = int(3.5e6)
MAXX = int(1e4)
MAX = int(MAXX)
MIN = int(-MAXX)
MAX_POINTS = int(3.5e3)
TESTS = 1000
MEAN = MAX_POINTS / 2
STD_DEV = MAX_POINTS / 4  # Adjust this value to change the spread of the distribution

# Ensure the directory exists
os.makedirs('tests/in', exist_ok=True)
os.makedirs('tests/out', exist_ok=True)

# Create a progress bar
progress_bar = tqdm(total=TESTS, desc='Generating tests')

for i in range(TESTS):
    num_points = max(3, min(MAX_POINTS, int(random.gauss(MEAN, STD_DEV))))
    points = set()
    while len(points) < num_points:
        x = random.randint(MIN, MAX)
        y = random.randint(MIN, MAX)
        points.add((x, y))  # Add the point to the set

    with open(f'tests/in/test{i}.txt', 'w') as f:
        f.write(f'{num_points}\n')
        for point in points:
            f.write(f'{point[0]} {point[1]}\n')

    # Run brute.exe and capture its output
    with open(f'tests/in/test{i}.txt', 'r') as in_file, open(f'tests/out/test{i}.txt', 'w') as out_file:
        result = subprocess.run(['brute.exe'], stdin=in_file, stdout=subprocess.PIPE)
        out_file.write(result.stdout.decode())

    # Update the progress bar
    progress_bar.update(1)
