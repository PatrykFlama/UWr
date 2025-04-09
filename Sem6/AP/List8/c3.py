import subprocess
import random
from tqdm import tqdm

MAXX = 6000

def generate_test_case(n_range=(1, MAXX), m_range=(1, MAXX), q_range=(1, MAXX)):
    n = random.randint(*n_range)
    m = random.randint(*m_range)
    q = random.randint(*q_range)
    ops = []
    for _ in range(q):
        l = random.randint(0, n - 1)
        r = random.randint(l, n - 1)
        seats = int(random.random() ** 2 * m) + 1
        ops.append((l, r, seats))
    return n, m, q, ops

def run_cpp_solution(n, m, q, ops):
    input_str = f"{n} {m} {q}\n"
    for l, r, s in ops:
        input_str += f"{l} {r} {s}\n"
    proc = subprocess.run(["./main"], input=input_str.encode(), stdout=subprocess.PIPE)
    output = proc.stdout.decode().split()
    return output

def brute_force_solution(n, m, q, ops):
    bookings = [0] * (n + 1)
    result = []
    for l, r, s in ops:
        max_now = max(bookings[l:r+1])
        if max_now + s <= m:
            for i in range(l, r+1):
                bookings[i] += s
            result.append("T")
        else:
            result.append("N")
    return result

def check(iterations=100):
    for i in tqdm(range(1, iterations + 1), desc="Running tests"):
        n, m, q, ops = generate_test_case()
        try:
            expected = brute_force_solution(n, m, q, ops)
            actual = run_cpp_solution(n, m, q, ops)

            if actual != expected:
                print(f"\n❌ Mismatch on test #{i}")
                print(f"n = {n}, m = {m}, q = {q}")
                print("Operations:")
                for op in ops:
                    print(op)
                print("Expected:", expected)
                print("Actual:  ", actual)
                return
        except Exception as e:
            print(f"\n💥 Error on test #{i}: {e}")
            return
    print("✅ All tests passed!")

if __name__ == "__main__":
    check()
