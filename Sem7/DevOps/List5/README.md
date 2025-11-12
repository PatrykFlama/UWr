# List 5
| 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 |
|---|---|---|---|---|---|---|---|
|   |   |   |   |   |   |   |   |



## Zad 1
Zainstalować pakiet (Debian/Ubuntu) - za pomocą modułu apt:
```bash
ansible all -i inventory.yaml -m ansible.builtin.apt -a "name=nginx state=present update_cache=yes" -b
```

(-b = become, jeśli potrzeba uprawnień root)


Aktywować i uruchomić service - moduł systemd:
```bash
ansible all -i inventory.yaml -m ansible.builtin.systemd -a "name=nginx enabled=yes state=started" -b
```

Ten moduł obsługuje enabled (enable on boot) i state (started/stopped).

Sprawdzić fakty o hoście:

szybkie zebranie pełnych faktów:
```bash
ansible all -i inventory.yaml -m ansible.builtin.setup -a "gather_subset=all"
```

albo zebrać tylko wybrane fakty (szybciej):
```bash
ansible all -i inventory.yaml -m ansible.builtin.setup -a "gather_subset=!all,hardware,network,virtual"
```

przykłady poleceń ad-hoc, by wypisać konkretne fakty (np. liczba rdzeni, jądro, dyski):
```bash
ansible host1 -i inventory.yaml -m ansible.builtin.setup -a "filter=ansible_processor_count,ansible_kernel,ansible_devices,ansible_mounts"
```

ansible_processor_count - liczba rdzeni; ansible_devices - wykryte bloki/dyski; ansible_kernel - wersja jądra.

