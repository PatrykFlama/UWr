from matplotlib import pyplot as plt
import subprocess
import os

def get_values(filename, M):
    x = []
    y = []
    x0 = []
    y0 = []

    p = subprocess.Popen(["zad7.exe"], stdin=subprocess.PIPE, stdout=subprocess.PIPE)

    with open(filename, "r") as file:
        x0, y0 = file.read().translate({ord(' ') : None, ord('\n') : None, ord('[') : None})[:-1].split(']')
        x0 = [float(i) for i in x0.split(',')]
        y0 = [float(i) for i in y0.split(',')]

    p.stdin.write(f"{M}\n".encode("utf-8"))
    p.stdin.write(f"{len(x0)}\n".encode("utf-8"))
    for tx, ty in zip(x0, y0):
        p.stdin.write(f"{tx} {ty}\n".encode("utf-8"))
        p.stdin.flush()

    while True:
        line = p.stdout.readline()
        if not line:
            break
        if "err" in line.rstrip().decode("utf-8"): 
            print(line.rstrip().decode("utf-8"))
            continue

        a, b = line.rstrip().decode("utf-8").split()
        if(abs(float(a)) < 1000 and abs(float(b)) < 1000):
            x.append(float(a))
            y.append(float(b))
        else:
            print("ERROR at", a, b)
            # p.kill()
            # return get_values(filename, _from, _to, _step)

    return x, y, x0, y0

M = input("M = ")
while M.isdigit():
    x, y, x0, y0 = get_values("zad7.in", M)

    plt.cla()
    plt.plot(x, y)

    # plt.scatter(x0, y0, color = "red")
    ax = plt.gca()
    ax.set_aspect('equal', adjustable='box')
    plt.show(block=False)
    M = input("M = ")

if M == "play":
    for m in range(1, 150):
        x, y, x0, y0 = get_values("zad7.in", m)

        plt.cla()
        plt.plot(x, y)

        # plt.scatter(x0, y0, color = "red")
        ax = plt.gca()
        ax.set_aspect('equal', adjustable='box')
        plt.title(f"M = {m}")
        plt.show(block=False)
        plt.pause(0.01)