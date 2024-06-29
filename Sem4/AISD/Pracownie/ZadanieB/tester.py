import os
import subprocess
from tqdm import tqdm
from math import sqrt

PROGRAM_NAME = "./main.exe"

def pdist(x,y):
    # tqdm.write(" ".join(str(i) for i in [x, y, "|", float(x[0]), float(y[0]), float(x[1]), float(y[1])]))
    return int(sqrt((float(x[0])-float(y[0]))**2 + (float(x[1])-float(y[1]))**2))

def compare_perimeter(triangle1, triangle2):    # triangle = [x1, y1, x2, y2, x3, y3]
    # tqdm.write(" ".join(str(i) for i in [triangle1, triangle2]))
    perimeter1 = pdist(triangle1[0:2], triangle1[2:4]) + pdist(triangle1[2:4], triangle1[4:6]) + pdist(triangle1[4:6], triangle1[0:2])
    perimeter2 = pdist(triangle2[0:2], triangle2[2:4]) + pdist(triangle2[2:4], triangle2[4:6]) + pdist(triangle2[4:6], triangle2[0:2])
    # tqdm.write(" ".join(str(i) for i in [perimeter1, perimeter2]))
    return perimeter1 - perimeter2, perimeter1, perimeter2

# Get the list of input files
input_files = os.listdir('tests/in')

for input_file in tqdm(input_files, desc="Running tests", unit="test"):
    try:
        # Generate the corresponding output and expected output filenames
        base_name = os.path.splitext(input_file)[0]
        output_file = f'{base_name}.txt'
        
        # Run the program and capture its output
        with open(f'tests/in/{input_file}', 'r') as in_file:
            result = subprocess.run([PROGRAM_NAME], stdin=in_file, stdout=subprocess.PIPE)
            output = result.stdout.decode()#.strip()

        # Compare the output with the expected output
        with open(f'tests/out/{output_file}', 'r') as expected_file:
            expected = expected_file.read()#.strip()

            output = output.split()
            expected = expected.split()

            res, rout, rex = compare_perimeter(output, expected)
            if res != 0:
                tqdm.write(f'Test {base_name} failed: {res} ({rout} != {rex})')
                # tqdm.write(str(output))
                # tqdm.write(str(expected))
    except Exception as e:
        tqdm.write(f'Test {base_name} failed: {e}')
        continue