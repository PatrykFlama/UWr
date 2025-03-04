import os
import requests
from bs4 import BeautifulSoup
from datetime import datetime

BASE_URL = "https://kernel.org/pub/linux/kernel/"
DEST_DIR = "linux_kernels"

os.makedirs(DEST_DIR, exist_ok=True)


def get_kernel_versions():
    """ pobiera listę dostępnych wersji jądra wraz z ich datami """
    response = requests.get(BASE_URL)
    if response.status_code != 200:
        return []

    soup = BeautifulSoup(response.text, "html.parser")
    versions = []
    for link in soup.find_all("a"):
        folder = link.text.strip('/')
        if folder.startswith("v"):
            print("Sprawdzanie " + folder)
            folder_url = BASE_URL + folder + "/"
            versions.extend(get_versions_from_folder(folder_url))
            print("Liczba wersji: " + str(len(versions)))

    return versions

def get_versions_from_folder(folder_url):
    """ pobiera wersje jądra i daty ich utworzenia z podanego folderu """
    response = requests.get(folder_url)
    if response.status_code != 200:
        print("...Folder z wersjami nie istnieje " + folder_url)
        return []

    soup = BeautifulSoup(response.text, "html.parser")
    versions = []

    for link in soup.find_all("a"):
        filename = link.text
        if filename.startswith("linux") and filename.endswith(".tar.xz"):
            date = link.next_sibling.strip().split('  ')[0]
            # print("...Sprawdzanie pliku " + filename + " z datą " + date)

            try:
                date_obj = datetime.strptime(date, "%d-%b-%Y %H:%M")
                versions.append((filename, folder_url + filename, date_obj))
            except ValueError:
                continue

    return versions

def download_file(url, dest):
    """ pobiera plik z podanego URL """
    response = requests.get(url, stream=True)
    if response.status_code == 200:
        with open(dest, "wb") as f:
            for chunk in response.iter_content(chunk_size=8192):
                f.write(chunk)
        print(f"Pobrano: {dest}")
    else:
        print(f"Blad pobierania: {url}")

# pobranie dostępnych wersji
kernel_versions = get_kernel_versions()
kernel_versions.sort(key=lambda x: x[2], reverse=True)  # sortowanie od najnowszych

# wybranie jednej wersji dla każdego roku
selected_versions = {}
for filename, url, date in kernel_versions:
    year = date.year
    # najnowasza wersja z danego roku
    if year+1 not in selected_versions:
        print(f"Wybieranie wersji dla {year+1} ({date}) - {filename}")
        selected_versions[year+1] = (filename, url, date)

# pobranie wybranych wersji
for year, (filename, url, date) in selected_versions.items():
    if year % 2 == 0: continue # pobieranie parzystych lat
    
    print(f"Pobieranie wersji dla {year} ({date}) - {filename}")
    dest_path = os.path.join(DEST_DIR, filename)
    download_file(url, dest_path)

print("Success")
