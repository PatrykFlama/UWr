import math
title = "perfect numbers"
def test_perfect(n):
    sum = 0
    for i in range(1, math.ceil(n*0.5)):
        if(n % i == 0): sum += i
    return sum == n

def imperative(n):
    res = []
    for i in range(0, n+1):
        if(test_perfect(i)): res.append(i)
    return res

def folding_list(n):
    return [i for i in range(0, n+1) if test_perfect(i)]

def functional(n):
    return list(filter(test_perfect, range(0, n+1)))


# -------- TEST --------
import timeit
import matplotlib.pyplot as plt

def test_plot():
    plt.title(title + ' functions time comparison')
    iterations = 10
    max_prime = 1000
    step = 25

    times = []
    for i in range(0, max_prime, step):
        times.append((timeit.timeit('imperative({})'.format(i), globals=globals(), number=iterations)) / iterations)
    plt.plot(times, label='imperative')

    times = []
    for i in range(0, max_prime, step):
        times.append((timeit.timeit('folding_list({})'.format(i), globals=globals(), number=iterations)) / iterations)
    plt.plot(times, label='folding_list')

    times = []
    for i in range(0, max_prime, step):
        times.append((timeit.timeit('functional({})'.format(i), globals=globals(), number=iterations)) / iterations)
    plt.plot(times, label='functional')

    plt.legend()
    plt.show()

def test_table():
    iterations = 10
    max_prime = 1000
    step = 100

    imperative_res = []
    for i in range(0, max_prime, step):
        imperative_res.append((timeit.timeit('imperative({})'.format(i), globals=globals(), number=iterations)) / iterations)
    
    folding_list_res = []
    for i in range(0, max_prime, step):
        folding_list_res.append((timeit.timeit('folding_list({})'.format(i), globals=globals(), number=iterations)) / iterations)

    functional_res = []
    for i in range(0, max_prime, step):
        functional_res.append((timeit.timeit('functional({})'.format(i), globals=globals(), number=iterations)) / iterations)

    print(title + ' functions time comparison:')
    print('n\timperative\tfolding_list\tfunctional')
    for i in range(0, len(imperative_res)):
        print('{}\t{:.8f}\t{:.8f}\t{:.8f}'.format(i*step, imperative_res[i], folding_list_res[i], functional_res[i]))


test_table()
test_plot()
