W zadaniach zmieniamy ciała funkcji. Interfejsów nie ruszamy. Testy powinny przechodzić po wykonaniu zadań.

Kompilacja:

mkdir build
cd build
cmake ..
make

Po skompilowaniu powstają 3 pliki binarne odpalające testy GTest: pow2, cheap, unique.

Odpowiedzi można zebrać np tak:
head -n -0 *.cpp | gzip > /tmp/output.gz

[Link do prezentacji](https://nokia.v92.pl/stl/)

___
Od C++20 można używać _spaceship operator_ `<=>` do porównywania obiektów. 
