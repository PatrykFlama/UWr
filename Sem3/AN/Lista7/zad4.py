def factorial(n):
    if n == 0:
        return 1
    return n * factorial(n-1)

def fa(n):
    return 1/factorial(n+1)

def ba(n):
    return 1/(factorial(n+1) * 2**(2*n-1))


def find_n(f, eps):
    n = 0
    while True:
        if f(n) < eps:
            return n
        n += 1

eps = 10e-12
print(find_n(fa, eps))
print(find_n(ba, eps))