import subprocess
import sys
from math import sqrt

def pdist(x,y):
    # tqdm.write(" ".join(str(i) for i in [x, y, "|", float(x[0]), float(y[0]), float(x[1]), float(y[1])]))
    return sqrt(((float(x[0])-float(y[0]))**2 + (float(x[1])-float(y[1]))**2))

def calc(triangle):    # triangle = [x1, y1, x2, y2, x3, y3]
    perimeter = pdist(triangle[0:2], triangle[2:4]) + pdist(triangle[2:4], triangle[4:6]) + pdist(triangle[4:6], triangle[0:2])
    return int(perimeter)


def run_program(program_file, input_file, clac_perimeter=False):
    with open(input_file, 'r') as file:
        input_data = file.read()
    print(f"Input size (in lines):\n{len(input_data.splitlines())}")

    process = subprocess.Popen([program_file], stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
    output, error = process.communicate(input=input_data)

    if error:
        print(f"Error: {error}")
    else:
        print(f"Output:\n{output}")
        if calc_perimeter: 
            peri = calc(output.split()) if calc_perimeter else 0
            print(f"Perimeter: {peri}")

if __name__ == '__main__':

    if len(sys.argv) < 3:
        print("Usage: python run.py <program_file> <input_file>")
        sys.exit(1)

    program_file = sys.argv[1]
    input_file = sys.argv[2]
    calc_perimeter = False
    if len(sys.argv) > 3: calc_perimeter = True

    run_program(program_file, input_file, calc_perimeter)