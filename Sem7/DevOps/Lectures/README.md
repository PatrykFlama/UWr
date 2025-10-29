# Lecures

## Lecture 1: IDK IDC

## Lecture 2: Docker
> (those notes are based (on presentation))

- reprodukowalność nie jest taka oczywista: np gcc zaznacza w kompilacie czas kompilacji, co sprawdia że nawet przy tym samym kodzie źródłowym i tych samych flagach kompilacji, ale w różnym czasie, binarka będzie inna (trzeba to wyłączyć specjalną flagą)  
- bezpieczeństwo w kontekście cyberbezpieczeństwa oraz w kontekście izolacji (aby zabezpieczyć się przez błędami w aplikacji) - Docker nie jest bezpipieczny, więc będziemy oczekiwać izolacji
- wydajność - konteneryzacja wprowadza pewne narzuty, które mogą wpływać na wydajność aplikacji
- łatwość konfiguracji (np konfiguracja samego linuxa jest skomplikowana i zbyt wolna)  


### wirtualizacja
domyślnie na maszynie różna aplikacje nie są (od siebie) izolowane - chcemy jedną maszynę podzielić na różne części dla tych aplikacji. 
pierwszym podejściem było tworzenie maszyn wirtualnych  

> domyślnie w systemie pamięć wirtualna aplikacji jest mapowana (stronami) na pamięć fizyczną - jest to dodatkowy narzut, więc mamy do tego akceleratory w CPU  
> przez to w VM mamy podwójne tłumaczenie pamięci (chociaż współczesnych CPU są mechanizmy przyspieszające to, które nie zawsze działają) - co jest wolne  

w tym wypadku aby zaatakować system musimy najpierw zaatakować system w VM, potem zaatakować hypervisora, i potem zaatakować OS

### konteneryzacja
w tym wypadku, aby wyjść z kontenera musimy zaatakować hypervisora, potem OS (mniej warstw do przejścia)

docker bazuje na: 
- namespaces
- cgroups
- capabilities

> fun fact: client docker hosta może pracować na nim zdalnie

#### demo
przez to że docker pracuje na namespaceach, to wymaga dostępu do jądra (root) - albo korzystamy z sudo, albo dodajemy użytkownika do grupy docker (ale to jest niebezpieczne, bo daje dostęp do roota)  

komendy z wykładu:
- `docker`
- `groups` - sprawdzamy że jesteśmy w docker
- `systemctl status docker.service` - sprawdzenie czy działa 
- `apt list | grep 'containerd'` - może być tak że mamy jakieś niezainstalowane biblioteki (tak się zdarzało w debian)
- `docker run debian:latest` - uruchamiamy kontener najnowszego debiana (o tyle niebezbieczne, że możemy zrobić literówkę i ściągnąć złośliwy obraz) (złośliwy bo kradnie kursor myszy i klawiatury)
  - po `:` jest tag 
  - ale nic się nie odpaliło - dlaczego? docker domyślnie uruchamia w tle, debian odpalił basha, który wykrył EOF (brak terminala) i się zamknął
- `docker run -it ubuntu:24.04` - uruchamiamy kontener  w trybie interaktywnym (z terminalem)
  - `-i` - interactive
  - `-t` - terminal
- `ps -eF` - pokazuje procesy
- `docker run -d ubuntu:24.04` - uruchamiamy kontener w tle (detached)
- `docker stop <container_id>` - zatrzymujemy kontener
- `docker ps --all` - wypisuje wszystkie kontenery, nawet te z przeszłości i przyszłości i innej linii czasuoraz innego uniwersum
- `docker run -idt --rm ubuntu:24.04` - uruchamiamy kontener w tle, z terminalem, interaktywnie, który po wyjściu się usunie




> dodatkowa bardzo użyteczna wiedza (nie z wykładu):  
> - `apt update && apt install -y vim` - instalujemy vim w kontenerze
> - `exit` - wychodzimy z kontenera (kontener się zatrzymuje)
> - `docker ps` - pokazuje działające kontenery
> - `docker ps -a` - pokazuje wszystkie kontenery (także zatrzymane)
> - `docker start <container_id>` - uruchamiamy zatrzymany kontener
> - `docker attach <container_id>` - dołączamy się do działającego kontenera
> - `docker exec -it <container_id> bash` - uruchamiamy nowy                          bash w działającym kontenerze
> - `docker rm <container_id>` - usuwamy zatrzymany kontener
> - `docker rmi <image_id>` - usuwamy obraz
> - `docker images` - pokazuje obrazy
> - `docker run -it --name moj_debian debian:latest` - uruchamiamy kontener i nadajemy mu nazwę
> - `docker logs <container_id>` - pokazuje logi kontenera
> - `docker run -it --rm debian:latest` - uruchamiamy kontener, który po wyjściu się usunie
> - `docker run -it --network host debian:latest` - uruchamiamy kontener z siecią hosta (niezalecane, bo kontener ma wtedy dostęp do wszystkiego co ma host)
> - `docker run -it -v /home/michal:/mnt debian:latest` - montujemy katalog z hosta do kontenera (uwaga na prawa dostępu)
> - `docker run -it --cpus="1.5" debian:latest` - ograniczamy liczbę CPU dla kontenera
> - `docker run -it --memory="512m" debian:latest` - ograniczamy pamięć dla kontenera
> - `docker run -it --memory="512m" --memory-swap="1g" debian:latest` - ograniczamy pamięć RAM i SWAP dla kontenera
> - `docker run -it --memory="512m" --memory-swap="1g" --memory-swappiness="0" debian:latest` - ograniczamy pamięć RAM i SWAP dla kontenera           oraz ustawiamy swappiness (0-100, domyślnie 60) - im wyższa wartość, tym chętniej system będzie korzystał ze SWAPu
> - `docker run -it --cap-drop ALL debian:latest` - usuwamy wszystkie capabilities (uwaga, może to spowodować że aplikacja nie będzie działać)        

### budowanie obrazów
docker desktop - płatna licencja, hayia  
ale my będziemy używać cli - fuiyoh  


valid backendy dockera:  
- namespaces + cgroups + capabilities (linux)
- LXC
- QEMU (niby MacOS)


budowanie obrazów:
- buildx - klient budowania (wbudowany w docker build)
- buildkit - demon budowania (wbudowany w dockerd)
- zamiast buildkit można użyć docker-container, kubernetes, remote


chcemy reprodukowalności, więc używamy Dockerfile (plik tekstowy z instrukcjami budowania obrazu) - Configuration as Code

każde polecenie to osobny obraz (warstwa) - który jest tworzony w dockerze i cachowany - (przy ponownym budowaniu obrazu, jeśli warstwa się nie zmieniła, to docker użyje cache'a zamiast budować od nowa)  
> `FROM` - jaki kontener podstawowy chcemy skonfigurować  


warstwy są cachowane, więc uwaga na:
```dockerfile
RUN apt update          # ta warstwa się nie zmieni, pozostanie zcacheowana, więc przy ponownym buildzie nie zaktualizuje listy pakietów
RUN apt install -y vim
```

#### demo


`Dockerfile:`
```dockerfile
FROM ubuntu:24.04

RUN apt update && apt install -y iproute-2 vim ssh curl
COPY id_ed25519.pub /root/.ssh/authorized_keys

WORKDIR /root
CMD bash
```

`docker build . wyklad1:v0`


> przeprowadzona próba cache  
> przeprowadzone budowane obrazu bazującego na obrazie z poprzedniego kroku


```dockerfile
FROM ubuntu:24.04

RUN apt update && apt install -y iproute-2 vim ssh curl
COPY id_ed25519.pub /root/.ssh/authorized_keys

RUN echo "PermitRootLogin yes" >> /etc/ssh/sshd_config

ENTRYPOINT service ssh start && bash
WORKDIR /root
```

CMD vs ENTRYPOINT
- CMD - domyślne polecenie, które można nadpisać przy uruchamianiu kontenera
- ENTRYPOINT - polecenie, które zawsze się wykona (chyba że użyjemy `--entrypoint` przy uruchamianiu kontenera)

RUN - wykonuje polecenie podczas budowania obrazu


## Lecture 3: Docker
yeah


## Lecture 4: Ansible
co jak chcemy konfigurować wiele maszyn naraz (np 1000 komputerów)? co jeżeli chcemy zaktuoalizować aplikację, gdzie proces jest opary na zarządzaniu 3 serwerami?  
chcemy osiągnąć efekt IaC, przykładowe narzędzia: 
- puppet, konfiguracja we własnym języku, wymagają klienta na zarządzanych maszynach (niewygodne - bo żeby automatycznie zarządzać musimy najpierw automatycznie zainstalować klienta/agenta) - konfiguracja pull
- chef, podobne do puppet - konfiguracja pull
- ansible, python i ssh są już domyślnie na większości systemów, więc ansible się łączy po ssh i uruchamia skrypt pythona - konfiguracja push
- terraform, rozwiązanie stricte chmurowe
- open tofu, fork terraforma

konfiguracja:
- pull - konfigurowany klient odpytuje serwer o konfigurację 
- push - serwer łączy się z klientem i konfiguruje go

> historia o OI w białym stoku  

> historia o błędzie w skrypcie czyszczącym edge w google

### demo ansible
- `ansible-doc` - dokumentacja modułów ansible  
- `ansible-doc --list -t inventory` - lista modułów inwentaryzacji
  - domyślnie używamy yaml i ini do konfiguracji inwentarza
- `ansible-doc --list -t inventory ansible.builtin.yaml` - pokazuje dokumentację modułu yaml inwentaryzacji


przykładowy plik inventory.yaml:
```yaml
all:  # nie musimy tego pisać, jest dodawane domyślnie
  mojagrupa:
    hosts:
      docker1:
        ansible_host: 172.17.0.101
        ansible_user: root
    docker2:
        ansible_host: 172.17.0.102
        ansible_user: root
    docker3:
        ansible_host: 172.17.0.103
        ansible_user: root

  mojagrupa2:
    hosts:
      docker1:
      docker3:
```

- `ansible-inventory -i inventory.yaml --list` - pokazuje inwentarz z pliku inventory.yaml
- `ansible-inventory -i inventory.yaml --graph` - pokazuje inwentarz z pliku inventory.yaml w formie grafu


inventory2.yaml:
```yaml
grupa_a:
  hosts:
    docker1:
      var_foo: "Smok"

grupa_b:
  hosts:
    docker1:
      var_foo: "Wąż"
```

grupy są spłaszczane, więc "smok" zostanie nadpisany przez "wąż" (bo jest później)  

zaktualizujmy inventory.yaml - inventory3.yaml:
```yaml
mojagrupa:
  hosts:
    ansible_host:
      172.17.0.[2:4]:    # lista prawostronnie domknięta
        ansible_user: root
```

inventory4.yaml:
```yaml
plugin: ansible.builtin.generator   # pozwala korzystać z jinja2 
hosts:
  name;: "zawodnik{{ id+10 }}"
  parents:
    - name: "mojagrupa"
      vars:
        ansible_host: "172.17.0.{{id}}"
        ansible_user: root

layers:
  id:
    - 2
    - 3
    - 4
```

inventory5.yaml:
```yaml
plugin: ansible.builtin.generator   # pozwala korzystać z jinja2 
hosts: "{{ operation }}_{{ application }}_{{ env }}_runner"

layer:
  operation: 
    - build
    - launch
  application:
    - appa
    - appb
  env:
    - linux
    - windows
```

ansible ogólnie jest deklaratywny, ale możemy np w ramach debugowania użyć imperatywności:
```bash
export ANSIBLE_HOST_KEY_CHECKING=False   # wyłącza sprawdzanie kluczy hosta
ansible all -i inventory5.yaml -m ping  # pingujemy wszystkie hosty z inwentarza
ansible docker1 -i inventory5.yaml -m gather_facts # zbieramy fakty o hoście docker1
```

### ansible.builtin
- `copy` - z kontrolera na hosta
- `fetch` - z hosta na kontroler
- `git` - klonowanie repozytoriów git
- `ping` - sprawdzanie czy host jest osiągalny
- `shell` - uruchamianie poleceń shellowych
- `gather_facts` - zbieranie faktów o hoście


```bash
ansible all -m shell -a "ls" -i inventory.yaml # docker nie wie co zrobi ta zmiana, więc przy użyciu shell uzna że został zmieniony (CHANGED)
ansible all -i inventory.yaml -m apt -a "name=sl state=present" -vvv # flaga verbose ma 3 poziomy
```

### jinja2
plik musi być poprawnym yaml'em aby zastosować jinja

- `{%...%}` - instrukcje (for, if, set, call)
- `{{...}}` - wypisywanie wyrażenia (np string, [1,2,3]*4)
- `{#...#}` - komentarze











