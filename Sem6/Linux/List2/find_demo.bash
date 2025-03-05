#!/bin/bash

echo -e "Wyszukiwanie plików większych niż 100MB w katalogu domowym użytkownika:"
find ~ -type f -size +100M
