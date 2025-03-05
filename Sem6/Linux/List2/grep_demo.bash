#!/bin/bash

echo -e "Error: Something went wrong\nWarning: Low disk space\nINFO: System is running" > log.txt

echo "Plik testowy log.txt:"
cat log.txt

echo -e "\nPrzykłady użycia grep:\n"

echo "1. Wyszukiwanie 'error' (ignorując wielkość liter):"
grep -i "error" log.txt

echo -e "\n2. Zliczanie linii zawierających 'warning':"
grep -c "warning" log.txt

echo -e "\n3. Wyszukiwanie linii zaczynających się od 'INFO':"
grep "^INFO" log.txt

echo -e "\n4. Odwrócone wyszukiwanie (linie NIE zawierające 'INFO'):"
grep -v "INFO" log.txt

rm log.txt
echo -e "\nPlik log.txt usunięty."
