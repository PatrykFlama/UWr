[(back)](../)

# **Language** **M**odels

<details>
    <summary>CUDA w pracowni 7 (RTX 3060)</summary>

Pracownia 7 jest wyposażona w komputery z kartami RTX 3060 12GB. Są już na nich zainstalowane biblioteki **transformers** i **pytorch**, a także modele **polka-1.1b** i **papuGaPT**.

- Do pracowni można logować się zdalnie (najlepiej parą kluczy, jak na pwi). Aby zalogować się z zewnątrz instytutu potrzebny jest vpn, konta zakładamy wg instrukcji 
- aby użyć lokalnych modeli należy odwołać się do katalogu **/models-lib** czyli np. tak (podajemy ścieżkę, a nie nazwę pliku!):

```py
generator = pipeline('text-generation', model='/models-lib/polka-1.1b',
                     max_new_tokens=100, device=0)  # zamiast `eryk-mazus/polka-1.1b`

generator = pipeline('text-generation', model='/models-lib/papuGaPT2', device=0)
```


Na kontach studenckich nie ma odpowiedniej ilości miejsca aby pobierać większe modele. W tym celu można wykorzystać katalog **/tmp** i wydać w nim polecenie **git clone**. np.: `git clone https://huggingface.co/eryk-mazus/polka-1.1b`

a następnie w swoim kodzie używać odpowiedniej ścieżki.

```
Nazwy komputerów:

lab007-01.stud.ii

...

lab007-20.stud.ii
```

(Może się oczywiście zdarzyć, że komputery są wyłączone)

(by pwi)

</details>
