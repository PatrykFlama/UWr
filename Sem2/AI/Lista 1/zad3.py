import numpy as np
import itertools
import random

# --------VAR--------
liczba_gier = 1000

# ---------FUNC---------
def sprawdz_ten_sam_kolor(karty):
    kolor = karty[0] // 12
    for i in karty:
        if (i // 12) != kolor: return False
    return True

def sprawdz_kofiguracja_rosnaca(karty):
    vis = [False]*12
    minn = 20
    for i in karty:
        minn = min(minn, i % 12)
        if(vis[i%12]): return False
        vis[i % 12] = True

    for i in range(minn, minn+5):
        if vis[i] != True: return False
    return True

def sprawdz_ile_figur(karty):
    cnt = [0]*12
    maxx = 0
    for i in karty:
        cnt[i % 12] += 1
        maxx = max(maxx, cnt[i % 12])
    return maxx

def czy_full(karty):
    cnt = [0]*12
    maxx = 0
    for i in karty:
        cnt[i % 12] += 1
        maxx = max(maxx, cnt[i % 12])
    if(maxx != 3): return False

    for i in cnt:
        if i == 2:
            return True
    return False

def wylosuj(karty):
    return list(random.sample(list(karty), 5))

# sila od 1 (wysoka karta) do 9 (poker)
def sila(karty):
    konf_rosnaca = sprawdz_kofiguracja_rosnaca(karty)
    ile_figur = sprawdz_ile_figur(karty)
    ten_sam_kolor = sprawdz_ten_sam_kolor(karty)

    if konf_rosnaca and ten_sam_kolor: return 9     # poker
    if ile_figur == 4: return 8                     # kareta
    if czy_full(karty): return 7                    # full
    if ten_sam_kolor: return 6                      # kolor
    if konf_rosnaca: return 5                       # strit
    return ile_figur

def losowa_gra(karty_blotkarza, karty_figuranta):
    if (sila(wylosuj(karty_blotkarza)) > sila(wylosuj(karty_figuranta))):
        return True
    return False

def prawdopodobienstwo_wygranej(karty_blotkarza, karty_figuranta):
    wygrane_blotkarz = 0
    for i in range(liczba_gier):
        if losowa_gra(karty_blotkarza, karty_figuranta):
            wygrane_blotkarz += 1
    return (wygrane_blotkarz/liczba_gier)

def generuj_karty_blotkarza(figury):
    nowe_karty_blotkarza = np.array([])
    for kol in range(0, 4):
        for fig in figury:
            nowe_karty_blotkarza = np.append(nowe_karty_blotkarza, fig + (12*kol)).astype(int)
    return nowe_karty_blotkarza

def generuj_karty_figuranta(figury):
    nowe_karty_figuranta = np.array([])
    for kol in range (0, 4):
        for fig in figury:
            nowe_karty_figuranta = np.append(nowe_karty_figuranta, fig + (12*kol)).astype(int)
    return nowe_karty_figuranta

# ---------MAIN---------
karty_figuranta = generuj_karty_figuranta([9, 10, 11, 12])


# figury = generuj_karty_blotkarza(np.array([0, 1, 2, 3, 4, 5, 6, 7, 8]))
figury = np.array([0, 1, 2, 3, 4, 5, 6, 7, 8])
for amt_of_cards in range(0, 9-4):
    for combination in list(itertools.combinations(figury, 9-amt_of_cards)):
        # print(combination, prawdopodobienstwo_wygranej(combination, karty_figuranta))
        if prawdopodobienstwo_wygranej(generuj_karty_blotkarza(combination), karty_figuranta) > 0.5:
            print(combination)
            exit()

figury = generuj_karty_blotkarza(np.array([0, 1, 2]))
print(figury, prawdopodobienstwo_wygranej(figury, karty_figuranta))
