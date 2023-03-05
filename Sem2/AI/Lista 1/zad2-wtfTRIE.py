"""
generowanie drzewa TRIE
klasa stanów z: {pozycja w słowie, aktualna suma kwadratów}

funkcja rekurencyjna która przyjmuje aktualny stan:
od pozycji zapisanej w stanie przechodzi po słowie i za każdym razem gdy trafia na słowo:
    modyfikuje sumę kwadratów i wywołuje się z tym nowym stanem

jeżeli kończy się zdanie i istnieje słowo na końcu to:
    poprawia wynik na podstawie globalnych zmiennych
"""

polish_letters = ['ą', 'ę', 'ć', 'ł', 'ń', 'ó', 'ś', 'ź', 'ż']

def charnum(c):
    if('a' <= c and c <= 'z'):
        return int(ord(c)-ord('a'))
    # polish letters: ą ę ć ł ń ó ś ź ż
    for pol in range(0, len(polish_letters)):
        if c == polish_letters[pol]:
            return int(ord('z')-ord('a')+pol+1)

class Node():
    next_node = [-1]*35
    is_end = False
    is_leaf = False

class Trie():
    nodes = []
    nodes_cnt = 1
    words_cnt = 0
    leaves_cnt = 0

    def __init__(self):
        self.nodes.append(Node())

    def add_word(self, str):
        v = 0
        for c in str:
            c_num = charnum(c)
            print(v, c,':', self.nodes[v].next_node[c_num], end='\n')
            if (self.nodes[v].next_node[c_num] == -1):
                self.nodes[v].next_node[c_num] = self.nodes_cnt
                self.nodes_cnt += 1
                self.nodes.append(Node())
                self.nodes[self.nodes_cnt-1].is_leaf = True
                self.leaves_cnt += 1

                if self.nodes[v].is_leaf:
                    self.leaves_cnt -= 1
                    self.nodes[v].is_leaf = False

            v = self.nodes[v].next_node[c_num]
        
        if not(self.nodes[v].is_end):
            self.nodes[v].is_end = True
            self.words_cnt += 1
            print("New word ", str, " words cnt: ", self.words_cnt)

trie = Trie()

trie.add_word("ugabuga")
trie.add_word("bugauga")
trie.add_word("bugaug")

print(trie.nodes_cnt)
print(trie.words_cnt)
print(trie.leaves_cnt)


for i in trie.nodes:
    print("\nNode:")
    for j in range(0, 35):
        if(i.next_node[j] != -1):
            print(i.next_node[j], chr(j+ord('a')))

trie.nodes[0].next_node[charnum('u')] = 0
print("NEW")

for i in trie.nodes:
    print("\nNode:")
    for j in range(0, 35):
        if(i.next_node[j] != -1):
            print(i.next_node[j], chr(j+ord('a')))
