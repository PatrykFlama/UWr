import gzip

words = set()
max_word_len = 0
output = open("zad2_output.txt", "wb")


def find_best_partition(line):      # word ranges: [from, to)
    dp = [0]*(len(line)+1)
    father = [-1]*(len(line)+1)
    father[0] = 0

    for r in range(1, min(max_word_len, len(line)+1)):
        # print("Searching in range ", 0, r, line[0:r])
        if line[0:r] in words:
            # print("binsearched", line[0:r])
            if  (dp[r] < ((r-0)*(r-0))):
                dp[r] = ((r-0)*(r-0))
                father[r] = 0
    for l in range(1, len(line)):
        if(father[l] == -1): continue
        for r in range(l+1, min(l+max_word_len, len(line)+1)):
            # print("Searching in range ", l, r, line[l:r])
            if line[l:r] in words:
                # print("binsearched", line[l:r])
                if (dp[r] < (dp[l] + (r-l)*(r-l))):
                    dp[r] = (dp[l] + (r-l)*(r-l))
                    father[r] = l

    ptr = len(line)
    str = ""
    # for i in range (len(line)-1):
    #     if father[i] != -1: print(line[i], father[i], i, dp[i])
    
    while ptr != 0:
        str = line[father[ptr]:ptr] + ' ' + str
        ptr = father[ptr]
    output.write(str.encode("utf-8"))
    output.write('\n'.encode("utf-8"))


#? save the dictionary, which is in lex order
with gzip.open('zad2_words.txt.gz', 'rb') as words_file:
        for line in words_file:
            line = line.decode('utf-8')
            line = line.rstrip('\n')
            line = line.rstrip('\r')
            max_word_len = max(max_word_len, len(line))
            if line: words.add(line)

max_word_len += 5

# lines = 0
# with open("pan_tadeusz_bez_spacji.txt", "rb") as tadeusz:
with open("zad2_input.txt", "rb") as tadeusz:
    for line in tadeusz:
        # if(lines == 20): break
        # lines += 1

        line = line.decode("utf-8")
        line = line.rstrip('\n')
        line = line.rstrip('\r')
        find_best_partition(line)

output.close()