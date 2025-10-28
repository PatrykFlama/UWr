[(back)](../)

# Lista 3
| 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 |
|---|---|---|---|---|---|---|---|
| x | x |   | x |   | x |   | x |


## Zad 1 i 8
Lista 2

## Zad 2
Użytkownik w grupie `docker` ma uprawnienia `root` względem hosta (docker działa jako root). Możemy więc utworzyć kontener z dostępem do całego systemu plików i przeczytać falgę.

```bash
docker run -it --rm -v ./:/mnt debian bash
```

## Zad 4
- sprawdzanie wersji cgroups:
```bash
stat -fc %T /sys/fs/cgroup
mount | grep cgroup
```
(jeśli `cgroup2fs`, to v2, jeśli `tmpfs`, to v1)

- sprawdzanie cgroup procesu:
```bash
cat /proc/<proces>/cgroup
cat /proc/$$/cgroup
```

- sprawdzanie ograniczeń
```bash
cat /sys/fs/cgroup/<cgroupa>/memory.max
cat /sys/fs/cgroup/<cgroupa>/cpu.max
```


- tworzenie własnej cgroupy
```bash
sudo mkdir /sys/fs/cgroup/demo
echo $$ | sudo tee /sys/fs/cgroup/demo/cgroup.procs
echo 100 | sudo tee /sys/fs/cgroup/demo/cpu.max
```

- cgroup.stat
  - nr_descendants 0 — liczba potomków tej cgroup (bez bieżącej)
  - nr_subsys_cpuset 0 — cpuset controller przydzielony (0 = brak, 1 = obecny)
  - nr_subsys_cpu 1 — cpu controller przydzielony (0 = brak, 1 = obecny)
  - nr_subsys_io 1 — io controller przydzielony (0 = brak, 1 = obecny)
  - nr_subsys_memory 1 — memory controller przydzielony (0 = brak, 1 = obecny)
  - nr_subsys_perf_event 1 — perf_event controller przydzielony (0 = brak, 1 = obecny)
  - nr_subsys_hugetlb 0 — hugetlb controller przydzielony (0 = brak, 1 = obecny)
  - nr_subsys_pids 1 — pids controller przydzielony (0 = brak, 1 = obecny)
  - nr_subsys_rdma 0 — rdma controller przydzielony (0 = brak, 1 = obecny)
  - nr_subsys_misc 0 — pozostałe/misc controller-y (0 = brak, 1 = obecny)
  - nr_dying_descendants 0 — liczba potomków w stanie "terminacji"
  - nr_dying_subsys_cpuset 0 — liczba umierających cpuset subsys
  - nr_dying_subsys_cpu 0 — liczba umierających cpu subsys
  - nr_dying_subsys_io 0 — liczba umierających io subsys
  - nr_dying_subsys_memory 0 — liczba umierających memory subsys
  - nr_dying_subsys_perf_event 0 — liczba umierających perf_event subsys
  - nr_dying_subsys_hugetlb 0 — liczba umierających hugetlb subsys
  - nr_dying_subsys_pids 0 — liczba umierających pids subsys
  - nr_dying_subsys_rdma 0 — liczba umierających rdma subsys
  - nr_dying_subsys_misc 0 — liczba umierających misc subsys


## Zad 6
slice to logiczna grupa procesów zarządzana przez systemd, odpowiadająca cgroup  

- tworzenie slice:
```bash
sudo systemd-run --unit=myservice1.service --slice=myslice.slice sleep 600
sudo systemd-run --unit=myservice2.service --slice=myslice.slice yes > /dev/null
```

- nakładanie ograniczeń:
```bash
sudo systemctl set-property myslice.slice MemoryMax=100M CPUQuota=20%
```

- sprawdzanie stanu slice:
```bash
systemd-cgtop # aktualne zużycie zasobów
systemd-cgls  # rekurencyjnie pokazuje zawartość cgroup
```


