# Lista 1
## Podsumowanie/notatki

* aktywowanie kart:
```bash
sudo ip link set up dev enp0s3
```

* sprawdzanie stanu kart:
```bash
ethtool enp0s3
```

* przypisanie adresu IP:
```bash
sudo ip addr add 192.168.0.1/24 dev enp0s3
```

* pingowanie
```bash
ping 192.168.0.1
```

* testowanie przepustowości łącza
```bash
VM1 $> iperf3 -s
VM2 $> iperf3 -c 192.168.0.1
```

* łączenie się z serwerem echa innej maszyny
```bash
telnet 192.168.0.2 7
```

* dekonfiugracja karty
```bash
sudo ip link set down dev enp0s3
sudo ip addr flush dev enp0s3
```


## Tutorial 1
![img](image.png)
![img](image-1.png)

![img](image-2.png)

![alt text](image-4.png)
![alt text](image-3.png)

![alt text](image-5.png)
![alt text](image-6.png)
![alt text](image-7.png)
![alt text](image-8.png)
![alt text](image-9.png)

![alt text](image-10.png)
![alt text](image-11.png)

## Tutorial 2
![alt text](image-12.png)
![alt text](image-13.png)
![alt text](image-14.png)

![alt text](image-15.png)

![alt text](image-16.png)

## Wyzwanie 1
![alt text](image-17.png)
![alt text](image-18.png)
![alt text](image-19.png)
![alt text](image-20.png)
![alt text](image-21.png)
![alt text](image-22.png)