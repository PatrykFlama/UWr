import subprocess
import random
from collections import defaultdict, deque
import sys

def generate_test(n=4, m=5, seed=None):
    if seed is not None:
        random.seed(seed)
    edges = set()
    while len(edges) < m:
        u = random.randint(1, n)
        v = random.randint(1, n)
        if u != v:
            w = random.randint(-10, 10)
            edges.add((u, v, w))
    return n, m, list(edges)

def brute_force(n, edges):
    graph = defaultdict(list)
    for u, v, w in edges:
        graph[u].append((v, w))

    best = None
    visited = [0] * (n + 1)
    def dfs(u, total):
        nonlocal best
        if u == n:
            best = max(best, total) if best is not None else total
        if visited[u] > n:
            best = -1
            return
        visited[u] += 1
        for v, w in graph[u]:
            dfs(v, total + w)
        visited[u] -= 1

    dfs(1, 0)
    return best if best != -1 else -1

def run_main_exe(input_str):
    proc = subprocess.run(
        ["./main.exe"],
        input=input_str.encode(),
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        timeout=2
    )
    output = proc.stdout.decode().strip()
    return int(output)

def test_case(seed=None):
    n, m, edges = generate_test(seed=seed)
    input_str = f"{n} {m}\n" + "\n".join(f"{u} {v} {w}" for u, v, w in edges)
    expected = brute_force(n, edges)
    try:
        result = run_main_exe(input_str)
    except subprocess.TimeoutExpired:
        print("main.exe timed out")
        return False

    if result != expected:
        print("❌ Mismatch:")
        print("Input:")
        print(input_str)
        print(f"Expected: {expected}")
        print(f"Got     : {result}")
        return False
    return True

if __name__ == "__main__":
    passed = 0
    total = 50
    for i in range(total):
        print(f"Test #{i+1}: ", end="")
        if test_case(seed=i):
            print("✅")
            passed += 1
        else:
            print("❌ FAIL")
            break
    print(f"\nPassed {passed}/{total}")
