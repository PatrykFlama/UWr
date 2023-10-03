def is_palindrom(str):
    str = (''.join(c for c in str if c.isalnum())).lower()
    return str == str[::-1]

to_test = ['kajak', 'Kobyła ma mały bok.', 'kajaki.', 'k', 'Eine güldne, gute Tugend: Lüge nie!', 'Míč omočím.']

for test in to_test:
    print(f'{test} -> {is_palindrom(test)}')