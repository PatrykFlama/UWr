import subprocess
import random
from collections import defaultdict, deque
from tqdm import tqdm

EXEC_PATH = "./main"
NUM_TESTS = 100
N_RANGE = (5, 2500)
M_RANGE = (5, 5000)
W_RANGE = (-1000000000, 1000000000)

def generate_connected_graph(n, m):
    """Generuje spójny graf skierowany z 1 do N."""
    edges = []
    connected = set([1])
    available = set(range(2, n + 1))
    
    # Łańcuch 1 → ... → N (by zapewnić połączenie)
    path = list(range(1, n + 1))
    for i in range(n - 1):
        u, v = path[i], path[i+1]
        w = random.randint(*W_RANGE)
        edges.append((u, v, w))

    edge_set = set((u, v) for u, v, _ in edges)

    while len(edges) < m:
        u = random.randint(1, n)
        v = random.randint(1, n)
        if u == v or (u, v) in edge_set:
            continue
        w = random.randint(*W_RANGE)
        edges.append((u, v, w))
        edge_set.add((u, v))

    return edges

def naive_longest_path(n, edges):
    """Naiwna metoda przez Bellman-Forda + detekcja cyklu na drodze do N."""
    dist = [-float('inf')] * (n + 1)
    dist[1] = 0

    for _ in range(n - 1):
        for u, v, w in edges:
            if dist[u] > -float('inf'):
                dist[v] = max(dist[v], dist[u] + w)

    changed = [False] * (n + 1)
    for u, v, w in edges:
        if dist[u] > -float('inf') and dist[u] + w > dist[v]:
            changed[v] = True

    graph = defaultdict(list)
    for u, v, _ in edges:
        graph[u].append(v)

    reachable = [False] * (n + 1)
    q = deque(v for v, ch in enumerate(changed) if ch)
    for v in q:
        reachable[v] = True

    while q:
        node = q.popleft()
        for nei in graph[node]:
            if not reachable[nei]:
                reachable[nei] = True
                q.append(nei)

    if reachable[n]:
        return -1
    return dist[n] if dist[n] > -float('inf') else -1

def run_main(input_str):
    try:
        result = subprocess.run(
            [EXEC_PATH],
            input=input_str.encode(),
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            timeout=2
        )
        return result.stdout.decode().strip(), result.stderr.decode()
    except subprocess.TimeoutExpired:
        return "TIMEOUT", ""

def main():
    failed = 0
    for _ in tqdm(range(NUM_TESTS), desc="Testing"):
        n = random.randint(*N_RANGE)
        m = random.randint(n - 1, min(n * 4, M_RANGE[1]))  # >= n-1 to ensure connectivity
        edges = generate_connected_graph(n, m)

        input_str = f"{n} {m}\n" + '\n'.join(f"{u} {v} {w}" for u, v, w in edges) + '\n'
        expected = str(naive_longest_path(n, edges))
        output, stderr = run_main(input_str)

        if output != expected:
            failed += 1
            print("\n❌ Test failed")
            print("Input:")
            print(input_str.strip())
            print("Expected:", expected)
            print("Output  :", output)
            if stderr:
                print("Stderr  :", stderr.strip())

    print(f"\n✅ All tests passed!" if failed == 0 else f"\n❌ {failed}/{NUM_TESTS} tests failed.")

if __name__ == "__main__":
    main()
