from string import ascii_lowercase

def CP(strs, depth=0):  
    max_pref = ""
    for c in ascii_lowercase:       # could be optimized with queue
        new_strs = [s for s in strs if len(s) > depth and s[depth] == c]
        if len(new_strs) < 3:
            continue

        pref = CP(new_strs, depth+1)

        if len(pref)+1 > len(max_pref):
            max_pref = c+pref
        
    return max_pref

def common_prefix(strs):
    return CP([s.lower() for s in strs])


print(common_prefix(["Cyprian", "cyberotoman", "cynik", "ceniąc", "czule"]))
print(common_prefix(["Cyprian", "cyberotoman", "cynik", "ceniąc", "czule", "cyba", "cybb"]))
print(common_prefix(["aaa", "aba", "aca", "ada", ""]))
print(common_prefix(["a", "b", "c", "d", "e"]))
