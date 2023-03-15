import math

figures = 9
colors = 4

res = [0]*9

res[0] = colors * (figures-5 + 1)
res[1] = figures * ((figures-1) * colors)
res[2] = (math.comb(4, 3) * figures) * (math.comb(4, 2) * (figures-1))
res[3] = colors * math.comb(figures, 5) - res[0]
res[4] = (figures-5 + 1) * pow(4, 5) - res[0]
res[5] = (math.comb(4, 3) * figures) * (math.comb(figures-1, 2) * 4*4)
res[6] = math.comb(figures, 3) * math.comb(4, 2) * math.comb(4, 2) * 4 * 3
res[7] = math.comb(figures, 4) * pow(4, 3) * math.comb(4, 3) *3 *2
res[8] = math.comb(9*4,5) - sum(res)

print(res)
math.comb(9*4,5)
