## I2C - Inter-Integrated Circuit
master wystawia sygnał zegarowy  
slave może ściągnąć zegar w gół, aby spowolnić transmisję danych (master nie może wysyłać danych)  

eeprom - Electrically Erasable Programmable Read-Only Memory, pamięć bajtowa nieulotna  

> fajne - FRAM, ferroelectric RAM - pamięć nieulotna, szybka, duża liczba cykli zapisu (10^9), ale droga  
> niektóre modele msp430 mają wbudowaną FRAM (bardzo energooszczędny)

format intel hex jest cały czas wykorzystywany (gdy chcemy mieć sumy kontrolne, etc) 

> opowiesć o alternatywnych źródłach synchronizacji czasi:
> - 50Hz - sieć energetyczna
> - fale radiowe - DCF77 (Niemcy)

gdy w komputerze wyładuje się bateria w zegarze, to po starcie będzie jakieś T0 z przeszłości - więc system żeby nie cofać się w czasie, ani aby nie losować daty z przyszłości, ustawia datę na czas odmontowania rootfs (wtedy ciągłość linii czasu zostanie zchowana)

> zlaeta freebsd - mamy wszystkie pliki źródłowe systemu

kiedyś nawet był koncept robienia arytmetyki w procesorze, bazujących na liczbach BCD ("E BCD IC")  


po do _bit-banging_? np jak mamy dhc11 (czujnik wilgotności), który się komunikuje po 2 drutach, to nie ma dla niego wsparcia sprzętowego (nie mamy z kim gadać) - więc musimy go ogarnąć software'owo

