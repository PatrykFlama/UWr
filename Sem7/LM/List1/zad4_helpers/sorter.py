import random

sample_size = 0.1

with open('../task4_answers.txt', 'r') as infile:
    lines = infile.readlines()
    random.shuffle(lines)
    lines = lines[:int(len(lines) * sample_size)]
lines.sort()
with open('sorted.txt', 'w') as outfile:
    outfile.writelines(lines)
