import matplotlib.pyplot as plt
import numpy as np
import math

# Define the functions
def g(x):
    return x**4

def h(x):
    if x + 4 > 0:
        return math.log(x + 4)
    else:
        return float('nan')

# Generate x values
x = np.linspace(-4, 2, 100)
plt.ylim(-5, 10)

# Apply math.log() to each element of x
y = []
for i in x:
    y.append(h(i))

# Plot the functions
plt.plot(x, g(x), label='g(x)')
plt.plot(x, y,    label='h(x)')

# Add title and legend
plt.title('Plot of g(x) = x^4 and h(x) = log(x + 4)')
plt.legend()

# Show the plot
plt.show()