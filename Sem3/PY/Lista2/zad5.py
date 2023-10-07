def compress(str):
    if(len(str) == 0): return []

    res = []
    last = str[0]
    cnt = 0
    for c in str:
        if c == last:
            cnt+=1
        else:
            res.append((cnt, last))
            last = c
            cnt = 1

    if cnt != 0: res.append((cnt, last))

    return res

def decompress(tab):
    res = ''
    for cnt, c in tab:
        res += cnt*c
    return res

print(decompress(compress("suuuuper")))
