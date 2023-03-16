import gzip
from random import randint

rand_max = 1000

words = set()
max_word_len = 0


def find_best_partition(line):      # word ranges: [from, to)
    dp = [0]*(len(line)+1)
    father = [-1]*(len(line)+1)
    father[0] = 0

    for r in range(1, min(max_word_len, len(line)+1)):
        if line[0:r] in words:
            if  (dp[r] < ((r-0)*(r-0))):
                dp[r] = ((r-0)*(r-0))
                father[r] = 0
    for l in range(1, len(line)):
        if(father[l] == -1): continue
        for r in range(l+1, min(l+max_word_len, len(line)+1)):
            if line[l:r] in words:
                if (dp[r] < (dp[l] + (r-l)*(r-l))):
                    dp[r] = (dp[l] + (r-l)*(r-l))
                    father[r] = l

    ptr = len(line)
    str = ""
    while ptr != 0:
        str = line[father[ptr]:ptr] + ' ' + str
        ptr = father[ptr]
    return str

def find_random_partition(line):      # word ranges: [from, to)
    dp = [0]*(len(line)+1)
    father = [-1]*(len(line)+1)
    father[0] = 0

    for r in range(1, min(max_word_len, len(line)+1)):
        if line[0:r] in words:
            rand = randint(0, 1000)
            if  (dp[r] < rand):
                dp[r] = rand
                father[r] = 0
    for l in range(1, len(line)):
        if(father[l] == -1): continue
        for r in range(l+1, min(l+max_word_len, len(line)+1)):
            if line[l:r] in words:
                if (dp[r] < rand):
                    dp[r] = rand
                    father[r] = l

    ptr = len(line)
    str = ""
    cntr = 1000
    while ptr != 0:
        if(cntr < 0): return "#error"
        cntr -= 1
        str = line[father[ptr]:ptr] + ' ' + str
        ptr = father[ptr]
    return str


#? save the dictionary, which is in lex order
with gzip.open('../Lista 1/zad2_words.txt.gz', 'rb') as words_file:
        for line in words_file:
            line = line.decode('utf-8')
            line = line.rstrip('\n')
            line = line.rstrip('\r')
            max_word_len = max(max_word_len, len(line))
            if line: words.add(line)

max_word_len += 5

lines = 0
lines_correct_longest = 0
lines_correct_random = 0


with open("zad3_input.txt", "rb") as tadeusz, open("PT_stripped.txt", "rb") as oryginal:
    for line, line_oryg in zip(tadeusz, oryginal):
        lines += 1
        # if(lines == 171): break        #! max line limiter

        line = line.decode("utf-8")
        line = line.strip()
        line = line.replace('\n', '')
        line = line.replace('\r', '')

        line_oryg = line_oryg.decode("utf-8")
        line_oryg = line_oryg.strip()
        line_oryg = line_oryg.replace('\n', '')
        line_oryg = line_oryg.replace('\r', '')

        rand = "#error"
        while(rand == "#error"): rand = find_random_partition(line)
        rand = rand.rstrip(' ')

        if(line_oryg == find_best_partition(line).rstrip(' ')): lines_correct_longest += 1
        if(line_oryg == rand): lines_correct_random += 1

        print("Line:", lines)

print("Longest partition: ", lines_correct_longest/lines)
print("Random partition: ", lines_correct_random/lines)
