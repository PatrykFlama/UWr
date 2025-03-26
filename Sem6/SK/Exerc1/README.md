# Ćwiczenia 1
## Zadanie 1
które: adres sieci, adres rozgłoszeniowy czy też adres komputera  
wyznacz odpowiadający mu adres sieci, rozgłoszeniowy i adres innego komputera w tej samej sieci  
- 10.1.2.3/8  
jest to adres komputera  
> 255.0.0.0  

adres sieci: 10.0.0.0  
adres rozgłoszeniowy: 10.255.255.255  
adres innego komputera: 10.1.2.4  

- 156.17.0.0/16  
jest to adres sieci  
> 255.255.0.0  

adres sieci: 156.17.0.0  
adres rozgłoszeniowy: 156.17.255.255    
adres innego komputera: 156.17.0.1  

- 99.99.99.99/27    
> 255.255.255.11100000  
>    99.99.99.01100011   

jest to adres komputera  
adres sieci: 99.99.99.96  
adres rozgłoszeniowy: 99.99.99.127  
adres innego komputera: 99.99.99.100  

- 156.17.64.4/30  
jest to adres sieci  
> 255.255.255.11111100  
>   156.17.64.00000100

adres sieci: 156.17.64.4  
adres rozgłoszeniowy: 156.17.64.7  
adres innego komputera: 156.17.64.5

- 123.123.123.123/32  
jest to adres komputera  
> 255.255.255.11111111  
> 123.123.123.01111011



## Zadanie 2
> 10.10.0.0/16  
> 00001010.00001010.00000000.00000000  
> 11111111.11111111.00000000.00000000  

> podmaska /19:   
> 11111111.11111111.11100000.00000000  

> podmaska /17:   
> 11111111.11111111.10000000.00000000  

podsieci:  
- 10.10.0.0/19
- 10.10.32.0/19
- 10.10.64.0/19
- 10.10.96.0/19
- 10.10.128.0/17

stara liczba komputerów w sieci: $2^{16}-2 = 65534$  
nowa liczba komputerów w sieci: $4*{2^{13}-2} + 2^{15}-2 = 65526$  
straciliśmy 8 adresów komputerów, ale zyskaliśmy 4 podsieci  


spróbujmy uzyskać jak najmniejszą podsieć:  
- 10.10.0.0/17 <=> 10.10.0.0 - 10.10.127.255  
- 10.10.128.0/18 <=> 10.10.128.0 - 10.10.191.255
- 10.10.192.0/19 <=> 10.10.192.0 - 10.10.223.255 
- 10.10.224.0/20 <=> 10.10.224.0 - 10.10.239.255
- 10.10.240.0/20 <=> 10.10.240.0 - 10.10.255.255

