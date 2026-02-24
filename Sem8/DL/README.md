# Projekt: Deep learning
## Wstępne info
- warto skonfigurować VPN uczelniany
- [strona internetowa przedmiotu]
- idea:
  - zaznajomić się z tematyką jakiegoś problemu DL
  - projekt jako repo GH, wraz z kompleksowym github actions
    - lepiej mieć `src` w projekcie
  - musi zawierać DevOps/MLOps
  - dane rzeczywiste
  - projekt bardziej się skupia na procesie/strukturze niż rozwiązaniu problemu - na dobrej praktyce inżynierskiej
  - polecane pytorch, lightning, kedro (choicaż to już ciężkie)
- wymagania:
  - zalecane co najmniej 3 osoby (2 też ujdzie, ale ciężej)
  - rozwiązanie **łatwego** problemu ML - to nie podlega ocenie
  - korzystanie z Astral uv
- warto zabezpieczyć main przed pushowaniem
- warto mieć linter i tool do formatowania kodu
- polecane torchvision, ew huggingface (potężne, ale tam jest syf)
- CI w githubie
  - testy (w pytest)
  - linter
- eksperymetny na modelu śledzone (porządnie) w W&B


> - lepiej wziąć dataset bez licencji, bo inaczej robi się straszny problem z przechowywaniem danych/publikacją (np **DVC** - Data Version Control)
> - [Astral UV](https://docs.astral.sh/uv/) - środowisko wirtualne, bardzo szybkie
> - WandB można postawić lolaknie - wtedy jest bez limitów  


