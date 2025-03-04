import os
import requests
from bs4 import BeautifulSoup

# Lista wydań do pobrania (aktualne na 1 stycznia nieparzystych lat)
versions = {
    1993: "0.99.15j",
    1995: "1.1.95",
    1997: "2.1.28",
    1999: "2.2.0",
    2001: "2.4.0",
    2003: "2.5.59",
    2005: "2.6.10",
    2007: "2.6.19",
    2009: "2.6.28",
    2011: "2.6.36",
    2013: "3.7",
    2015: "3.18",
    2017: "4.9",
    2019: "4.20",
    2021: "5.10",
    2023: "6.1"
}

# Struktura katalogów na kernel.org
BASE_URL = "https://mirrors.edge.kernel.org/pub/linux/kernel/"
HISTORIC_URL = BASE_URL + "Historic/"
DEST_DIR = "linux_kernels"

# Tworzenie katalogu docelowego
os.makedirs(DEST_DIR, exist_ok=True)

def get_latest_tar_url(base_url, version_prefix):
    """Znajduje URL do najnowszego pliku tar.xz lub tar.gz dla podanej wersji"""
    response = requests.get(base_url)
    if response.status_code != 200:
        return None
    
    soup = BeautifulSoup(response.text, "html.parser")
    files = [a.text for a in soup.find_all("a")]
    
    # Sprawdź, czy istnieje tar.xz lub tar.gz pasujące do wersji
    for ext in ["tar.xz", "tar.gz"]:
        filename = f"linux-{version_prefix}.{ext}"
        if filename in files:
            return base_url + filename
    
    return None

def download_kernel(year, version):
    """Pobiera archiwum jądra dla podanej wersji i roku."""
    print(f"Pobieranie wersji {version} ({year})...")
    
    if version.startswith("0.99"):
        base_url = HISTORIC_URL + "v0.99/"
    elif version.startswith("1."):
        base_url = BASE_URL + "v1.1/"
    elif version.startswith("2.0"):
        base_url = BASE_URL + "v2.0/"
    elif version.startswith("2.1"):
        base_url = BASE_URL + "v2.1/"
    elif version.startswith("2.2"):
        base_url = BASE_URL + "v2.2/"
    elif version.startswith("2.4"):
        base_url = BASE_URL + "v2.4/"
    elif version.startswith("2.5"):
        base_url = BASE_URL + "v2.5/"
    elif version.startswith("2.6"):
        base_url = BASE_URL + "v2.6/"
    elif version.startswith("3."):
        base_url = BASE_URL + "v3.x/"
    elif version.startswith("4."):
        base_url = BASE_URL + "v4.x/"
    elif version.startswith("5."):
        base_url = BASE_URL + "v5.x/"
    elif version.startswith("6."):
        base_url = BASE_URL + "v6.x/"
    else:
        print(f"Nieznana ścieżka dla {version}, pomijam...")
        return
    
    url = get_latest_tar_url(base_url, version)
    if not url:
        print(f"Nie znaleziono pliku dla wersji {version}")
        return
    
    filename = os.path.join(DEST_DIR, url.split("/")[-1])
    response = requests.get(url, stream=True)
    if response.status_code == 200:
        with open(filename, "wb") as f:
            for chunk in response.iter_content(chunk_size=8192):
                f.write(chunk)
        print(f"Pobrano: {filename}")
    else:
        print(f"Błąd pobierania: {url}")

# Pobieranie plików
for year, version in versions.items():
    download_kernel(year, version)

print("Pobieranie zakończone!")
