import matplotlib.pyplot as plt
import numpy as np

def convert_to_float(frac_str):
    try:
        return float(frac_str)
    except ValueError:
        num, denom = frac_str.split('/')
        try:
            leading, num = num.split(' ')
            whole = float(leading)
        except ValueError:
            whole = 0
        frac = float(num) / float(denom)
        return whole - frac if whole < 0 else whole + frac


# load data from file
data = []
with open('zad3.txt', 'r') as f:
    for line in f:
        data.append(convert_to_float(line.strip()))

# create plot
plt.plot(data, np.zeros_like(data) + 0.1, 'x')
plt.show()
