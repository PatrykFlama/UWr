import requests
import time

urls = ['https://www.example.com', 'https://www.google.com']
website_content = {}        # dictionary

# pobieramy zawartość każdej strony i zapisujemy ją w słowniku
for url in urls:
    response = requests.get(url)
    content_dict[url] = response.text

# ustawiamy timer na 1 minutę
interval = 60

while True:
    # czekamy na upływ czasu
    time.sleep(interval)
    
    # pobieramy ponownie zawartość każdej strony i porównujemy ją z zawartością zapisaną w słowniku
    for url in urls:
        response = requests.get(url)
        if response.text != content_dict[url]:
            # jeśli zawartość się zmieniła, zwracamy to, co się zmieniło
            print(f'Zmiana na stronie {url}:')
            print(response.text)
            content_dict[url] = response.text
