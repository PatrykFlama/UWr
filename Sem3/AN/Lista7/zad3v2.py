import matplotlib.pyplot as plt
import numpy as np


def calc_polynomial(x, args):
    res = 1
    for arg in args:
        res *= (x - arg)    
    return res

def equidistant_nodes(n):
    return np.linspace(-1, 1, n+1)

def chebyshev_nodes(n):
    res = []
    for k in range(n+1):
        res.append(np.cos((2*k+1)/(2*n + 2) * np.pi))
    return np.array(res)


x_precision = np.linspace(-1, 1, 1000)
for n in range(5, 15):
    y = [calc_polynomial(x, equidistant_nodes(n)) for x in x_precision]
    plt.plot(x_precision, y, label = f"{n}")

# for n in range(0, 20):
#     y = [calc_polynomial(x, chebyshev_nodes(n)) for x in x_precision]
#     plt.plot(x_precision, y, label = f"{n}")

# plt.legend()
plt.show()
