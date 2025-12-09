# List 3.5
| 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 |
|---|---|---|---|---|---|---|---|---|----|----|
| x |   |   |   |   |   |   |   |   |    |    |


## Zad 1
tonie:  
- jako tonąć, topić się
- jako toń, głębina
- jako tona, waga
- jako ton, np głosu

Tonie skrywają skarby, tonie kapitan statku, który w tonie spokoju spekuluje o tonie złota.

## Zad 2
Rzadkie reprezentacje to wektory kontekstów o wielu zerach - dobrze pokazują współwystępowania, lecz rozdzielają formy leksykalne i mieszają znaczenia. Na przykład "piękna żaglówka" i "śliczny żaglowiec" mogą być sensownie bliskie, a w TF‑IDF traktowane oddzielnie.

Krótka procedura:
- Lematyzacja; budowa wektorów kontekstów (okno +=N), oblicz PPMI/TF‑IDF
- Truncated SVD / PCA -> gęste wektory (100-300 wymiarów)
- Dla każdego typu słowa sklastruj jego wektory kontekstów (spherical k‑means lub HDBSCAN) -> centroidy = wektory sensów
- Reprezentuj słowo jako zbiór centroidów lub ważoną sumę; opcjonalna dalsza redukcja do 50-150 wymiarów
- Indeksuj sensy (Faiss/Annoy) i wybieraj najbliższy centroid dla danego wystąpienia (WSD, similarity)

Takie podejście łączy synonimiczne formy przez podobne konteksty i rozdziela różne znaczenia, jednocześnie dając gęstsze, niższe wymiarowo reprezentacje.

