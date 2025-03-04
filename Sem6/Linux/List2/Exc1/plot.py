import matplotlib.pyplot as plt
import csv

CSV_FILE = "linux_statistics.csv"

versions = []
files_count = []
lines_of_code = []
person_years = []
cost_dollars = []

with open(CSV_FILE, newline="") as f:
    reader = csv.reader(f)
    next(reader)
    for row in reader:
        versions.append(row[0])
        files_count.append(int(row[1]))
        lines_of_code.append(int(row[2]))
        person_years.append(float(row[3]))
        cost_dollars.append(int(row[4]))



plt.figure(figsize=(12, 6))

# liczba linii kodu
plt.subplot(2, 2, 1)
plt.plot(versions, lines_of_code, marker="o", linestyle="-", color="b")
plt.xticks(rotation=45, ha="right")
plt.xlabel("Wersja")
plt.ylabel("Linie kodu")
plt.title("Liczba linii kodu w kolejnych wersjach")
plt.grid()

# liczba plików
plt.subplot(2, 2, 2)
plt.plot(versions, files_count, marker="s", linestyle="--", color="g")
plt.xticks(rotation=45, ha="right")
plt.xlabel("Wersja")
plt.ylabel("Liczba plików")
plt.title("Liczba plików w kolejnych wersjach")
plt.grid()

# koszt w dolarach
plt.subplot(2, 2, 3)
plt.plot(versions, cost_dollars, marker="^", linestyle="-.", color="r")
plt.xticks(rotation=45, ha="right")
plt.xlabel("Wersja")
plt.ylabel("Koszt ($)")
plt.title("Szacowany koszt odtworzeniowy")
plt.grid()

# osobolata
plt.subplot(2, 2, 4)
plt.plot(versions, person_years, marker="d", linestyle=":", color="purple")
plt.xticks(rotation=45, ha="right")
plt.xlabel("Wersja")
plt.ylabel("Osobolata")
plt.title("Szacowany czas pracy (osobolata)")
plt.grid()

plt.tight_layout()
plt.show()
