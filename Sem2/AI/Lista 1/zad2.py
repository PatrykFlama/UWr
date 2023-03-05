words = {"a"}
max_word_len = 0


def find_best_partition(line):
    dp = [0]*len(line)
    father = [-1]*len(line)
    father[0] = 0

    for l in range(0, len(line)):
        if(l > 0 and father[l-1] == -1): continue
        for r in range(l, min(l+max_word_len, len(line))):
            # print("Searching in range ", l, r, line[l:(r+1)])
            if line[l:(r+1)] in words:
                # print("binsearched", line[l:(r+1)])
                if  (dp[r] < (dp[l-1] + (r-l)*(r-l))):
                    dp[r] = (dp[l-1] + (r-l+1)*(r-l+1))
                    father[r] = l

    ptr = len(line)-1
    str = ""
    while ptr > 0:
        str = line[father[ptr]:ptr+1] + ' ' + str
        ptr = father[ptr]-1
    print(str)


#? save the dictionary, which is in lex order
with open("words.txt", "rb") as words_file:
    for line in words_file:
        line = line.decode("utf-8")
        line = line.rstrip('\n')
        line = line.rstrip('\r')
        max_word_len = max(max_word_len, len(line))
        words.add(line)

temp = 0
with open("pan_tadeusz_bez_spacji.txt", "rb") as tadeusz:
    for line in tadeusz:
        if(temp == 20): break
        temp += 1

        line = line.decode("utf-8")
        line = line.rstrip('\n')
        line = line.rstrip('\r')
        find_best_partition(line)