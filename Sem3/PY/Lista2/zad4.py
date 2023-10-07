import random
import urllib.request

def simplify(text, word_len, max_words):
    text = text.split()
    text = [x for x in text if len(x) < word_len]

    while len(text) > max_words:
        text.pop(random.randint(0, len(text) - 1))

    return ' '.join(text)

def get_text():
    max_read = 100000
    url = 'https://wolnelektury.pl/media/book/txt/pan-tadeusz.txt'
    return urllib.request.urlopen(url).read().decode('utf-8')[:max_read]

# print(get_text())
print(simplify(get_text(), 10, 5))

