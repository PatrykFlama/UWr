import subprocess
import random
from tqdm import tqdm

TESTS = 1000
MAXN = 100
MAXV = 1000000

def generate_test_case(n_range=(1, MAXN), q_range=(1, MAXN//2)):
    n = random.randint(*n_range)
    q = random.randint(*q_range)
    A = [random.randint(1, MAXV) for _ in range(n)]
    ops = []

    for _ in range(q):
        type_ = random.randint(1, 3)
        l = random.randint(1, n)
        r = random.randint(l, n)
        if type_ in [1, 2]:
            v = random.randint(1, MAXV)
            ops.append((type_, l, r, v))
        else:
            ops.append((type_, l, r))
    return n, A, len(ops), ops

def run_cpp_solution(n, A, q, ops):
    input_str = f"{n} {q}\n" + " ".join(map(str, A)) + "\n"
    for op in ops:
        input_str += " ".join(map(str, [op[0], op[1], op[2]] + list(op[3:]))).strip() + "\n"
    proc = subprocess.run(["./main"], input=input_str.encode(), stdout=subprocess.PIPE)
    return proc.stdout.decode().split()

def brute_force_solution(n, A, q, ops):
    a = A[:]
    res = []
    for op in ops:
        t, l, r = op[0], op[1]-1, op[2]-1
        if t == 1:
            v = op[3]
            for i in range(l, r+1):
                a[i] += v
        elif t == 2:
            v = op[3]
            for i in range(l, r+1):
                a[i] = v
        elif t == 3:
            s = sum(a[l:r+1])
            res.append(str(s))
    return res

def check(iterations=TESTS):
    for i in tqdm(range(1, iterations+1), desc="Testing segment tree"):
        n, A, q, ops = generate_test_case()
        try:
            expected = brute_force_solution(n, A, q, ops)
            actual = run_cpp_solution(n, A, q, ops)

            if expected != actual:
                print(f"\nMismatch on test #{i}")
                print(f"n = {n}, q = {q}")
                print("Initial array:", A)
                print("Ops:")
                for op in ops:
                    print(op)
                print("Expected:", expected)
                print("Actual:  ", actual)
                return
        except Exception as e:
            print(f"\nError on test #{i}: {e}")
            return
    print("All tests passed!")

if __name__ == "__main__":
    check()
