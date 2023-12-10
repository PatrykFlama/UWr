import matplotlib.pyplot as plt

def factorial(n):
    if n == 0:
        return 1
    return n*factorial(n-1)

def calc_binom(n, k):
    return factorial(n) / (factorial(k) * factorial(n-k))

def Pslow(n, t, W):
    res = 0
    for k in range(n+1):
        res += calc_binom(n, k) * t**k * (1-t)**(n-k) * W[k]
    return res

def P(n, t, W):
    # return Pslow(n, t, W)
    binom = 1
    mult = 1
    res = binom*W[n]
    for k in range(1, n+1):
        binom = binom * (n-k+1) / k
        mult = mult * (1-t)

        res = res*t + binom * mult * W[n-k]
    return res

def R(n, t, W, w):
    return P(n, t, [w[i]*W[i] for i in range(n+1)]) / P(n, t, w)


coordinates = [(0, 0), (3.5, 36), (25, 25), (25, 1.5), (-5, 3), (-5, 33), (15, 11), (-0.5, 35), (19.5, 15.5), (7, 0), (1.5, 10.5)]
x_values = [coord[0] for coord in coordinates]
y_values = [coord[1] for coord in coordinates]


weights_input = [1, 6, 4, 2, 3, 4, 2, 1, 5, 4, 1]
all_weights = [
                weights_input,
                [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
                [0.001, 1, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001],
                [1, 1000, 1, 1, 1, 1, 1, 1, 1, 1, 1],
              ]


fig, ax = plt.subplots()
ax.set_aspect('equal', adjustable='box')

plt.scatter(x_values, y_values)
for i, txt in enumerate(x_values):
    ax.annotate(i, (x_values[i], y_values[i]))

for weights in all_weights:
    x_res = []
    y_res = []
    t = 0
    prec = 0.01
    while t <= 1:
        x_res.append(R(len(coordinates)-1, t, x_values, weights))
        y_res.append(R(len(coordinates)-1, t, y_values, weights))
        t += prec

    ax.plot(x_res, y_res)


plt.show()
