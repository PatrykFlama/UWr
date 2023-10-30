import matplotlib.pyplot as plt

# Read data from file
with open('zad6.txt', 'r') as f:
    data = f.readlines()

# Extract x and y values
x = []
y = []
for line in data:
    values = line.split()
    x.append(float(values[0]))
    y.append(float(values[1]))

# Plot data
plt.plot(x, y)
plt.xlabel('x')
plt.ylabel('y')
plt.title('zad6.txt')
plt.show()
