import random

sample_size = 0.1

with open('../task4_answers.txt', 'r') as ansfile, open('../task4_questions.txt', 'r') as qfile:
    lines = [(q, a) for q, a in zip(qfile.readlines(), ansfile.readlines())]
    random.shuffle(lines)
    lines = lines[:int(len(lines) * sample_size)]
lines.sort()
with open('sorted.txt', 'w') as outfile:
    outfile.writelines(f"{a.strip()}\t\t{q.strip()}\n" for q, a in lines)
