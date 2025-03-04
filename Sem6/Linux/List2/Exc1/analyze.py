import os
import subprocess
import csv

OUTPUT_DIR = "linux_kernels_unpacked"
CSV_FILE = "linux_statistics.csv"

data = []
for version in sorted(os.listdir(OUTPUT_DIR)):
    version_path = os.path.join(OUTPUT_DIR, version)
        
    if os.path.isdir(version_path):
        print(f"Analizuję: {version}")
        
        # liczenie linii kodu
        cloc_result = subprocess.run(["cloc", version_path, "--csv"], capture_output=True, text=True)
        lines_of_code = 0
        files_count = 0
        for line in cloc_result.stdout.split("\n"):
            # files,language,blank,comment,code
            if "SUM" in line:
                parts = line.split(",")
                files_count = int(parts[0])
                lines_of_code = int(parts[4])

        print(f"Linie kodu: {lines_of_code}, pliki: {files_count}")
        
        # koszt odtworzeniowy
        sloccount_result = subprocess.run(["sloccount", version_path], capture_output=True, text=True)
        cost_dollars = 0
        person_years = 0
        for line in sloccount_result.stdout.split("\n"):
            if "Total Estimated Cost to Develop" in line:
                cost_dollars = int(line.split("=")[-1].replace("$", "").split()[0].replace(",", ""))
            if "Development Effort Estimate" in line:
                person_years = float(line.split("=")[-1].split()[0].replace(",", ""))

        print(f"Osobolata: {person_years}, koszt: {cost_dollars}")
        print()

        data.append([version, files_count, lines_of_code, person_years, cost_dollars])

with open(CSV_FILE, "w", newline="") as f:
    writer = csv.writer(f)
    writer.writerow(["Wersja", "Liczba plików", "Liczba linii kodu", "Osobolata", "Koszt ($)"])
    writer.writerows(data)

print(f"Wyniki zapisane do {CSV_FILE}")
