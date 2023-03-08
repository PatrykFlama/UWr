import numpy as np
import itertools
import random

# --------VAR--------
#* karty: 2-10 + WQKA -> 11-14
#* kolory: {0, 1, 2, 3}

# ---------FUNC---------
def czy_ten_sam_kolor(karty):
    if len(karty) == 0: return True
    kolor = karty[0][1]
    for karta in karty:
        if karta[1] != kolor: return False
    return True

def czy_rosnace(karty):
    if len(karty) == 0: return True
    ostatnia = karty[0][0]
    for karta in range(1, len(karty)):
        if ostatnia+1 != karty[karta][0]: return False
        ostatnia += 1
    return True

def zlicz_te_same(karty):
    tab = [0]*16
    for karta in karty:
        tab[karta[0]] += 1
    return tab


def sprawdz_poker(karty):
    return (czy_ten_sam_kolor(karty) and czy_rosnace(karty))

def sprawdz_kareta(karty):
    max_sum = 0
    for element in zlicz_te_same(karty):
        max_sum = max(max_sum, element)
    if max_sum == 4: return True
    return False

def sprawdz_ful(karty):
    three = two = False
    for element in zlicz_te_same(karty):
        if element == 3: three = True
        elif element == 2: two = True
    return three and two

def sprawdz_kolor(karty):
    return czy_ten_sam_kolor(karty)

def sprawdz_strit(karty):
    return czy_rosnace(karty)

def sprawdz_trojka(karty):
    maxx = 0
    for i in zlicz_te_same(karty): maxx = max(maxx, i)
    return maxx == 3

def sprawdz_dwie_pary(karty):
    cnt = 0
    for i in zlicz_te_same(karty): 
        if i == 2: cnt += 1
    return cnt == 2

def sprawdz_para(karty):
    cnt = 0
    for i in zlicz_te_same(karty): 
        if i == 2: return True
    return False

def sprawdz_wysoka(karty):
    wysoka = 0
    for karta in karty: wysoka = max(wysoka, karta[0])
    return wysoka


def sila(karty):
    if sprawdz_poker(karty):    return 9+16
    if sprawdz_kareta(karty):   return 8+16
    if sprawdz_ful(karty):      return 7+16
    if sprawdz_kolor(karty):    return 6+16
    if sprawdz_strit(karty):    return 5+16
    if sprawdz_trojka(karty):  return 4+16
    if sprawdz_dwie_pary(karty):         return 3+16
    if sprawdz_para(karty):         return 2+16
    return sprawdz_wysoka(karty)

def porownaj_karty(karty1, karty2):
    if sila(karty1) > sila(karty2): return 1
    return 0

def losuj_karty(zestaw):
    return sorted(list(random.sample(list(zestaw), 5)))

def generuj_zestaw(figury):
    res = []
    for figura in figury:
        for kolor in range(4):
            res.append([figura, kolor])
    return res

def graj(blotkarz, figurant):
    liczba_gier = 1000
    blotkarz = generuj_zestaw(blotkarz)
    figurant = generuj_zestaw(figurant)

    wygrane = 0
    for i in range(liczba_gier):
        wygrane += porownaj_karty(losuj_karty(blotkarz), losuj_karty(figurant))
    return wygrane/liczba_gier

pula_figur_figuranta = [11, 12, 13, 14]
pula_figur_blotkarza = oryginalna_pula_figur_blotkarza = [2, 3, 4, 5, 6, 7, 8, 9, 10]
print(graj(pula_figur_blotkarza, pula_figur_figuranta))
