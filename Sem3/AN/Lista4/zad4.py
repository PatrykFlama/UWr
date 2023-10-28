import matplotlib.pyplot as plt
import numpy as np

# Define the functions
def g(x):
    return x**4

def h(x):
    return x + 4

# Generate x values
x = np.linspace(-10, 10, 100)

# Plot the functions
plt.plot(x, g(x), label='g(x)')
plt.plot(x, h(x), label='h(x)')

# Add title and legend
plt.title('Plot of g(x) = x^4 and h(x) = x + 4')
plt.legend()

# Show the plot
plt.show()
sync