# read 1d pref_sums from input
data = input().split()

if(data == []):
    print("Empty input")
    exit()

tab = [int(i) for i in data]

#? as long as range that we want to take is not negative, we want to take it
i, j, act_sum = 0, 0, 0
best_i, best_j, best_sum = 0, 0, 0
while(i < len(tab) and j < len(tab)):
    if(act_sum < 0):
        i = j
        act_sum = 0

    act_sum += tab[j]
    if(act_sum > best_sum):
        best_i, best_j, best_sum = i, j, act_sum
    j += 1

print(best_i, best_j, best_sum)
