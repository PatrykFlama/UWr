


## Zad 1
oczekiwane wartości (samych operacji, bez narzutu przenoszenia danych):
- add/sub:
    - int8_t : 1 cycle
    - int16_t : 2 cycles
    - int32_t : 4 cycles
    - int64_t : 8 cycles
- multiply (8×8 MUL = 2 cykle):
    - int8_t : = 2 cycles
    - int16_t : = 10-15 cycles (MUL + ADD; rough = 12)
    - int32_t : = 40-60 cycles
    - int64_t : = 150-250 cycles
- float (nie ma FPU, więc operacje emulowane software'owo)


mierzone wartości:
```
int8_t   add    : ticks=218 cycles/op=6.97
int8_t   mul    : ticks=312 cycles/op=9.98
int8_t   div    : ticks=7125 cycles/op=228.00
int16_t  add    : ticks=312 cycles/op=9.98
int16_t  mul    : ticks=563 cycles/op=18.01
int16_t  div    : ticks=7000 cycles/op=224.00
int32_t  add    : ticks=500 cycles/op=16.00
int32_t  mul    : ticks=2656 cycles/op=84.99
int32_t  div    : ticks=18937 cycles/op=605.98
int64_t  add    : ticks=2125 cycles/op=68.00
int64_t  mul    : ticks=10282 cycles/op=329.02
int64_t  div    : ticks=15907 cycles/op=509.02
float    add    : ticks=3250 cycles/op=104.00
float    mul    : ticks=4750 cycles/op=152.00
float    div    : ticks=14313 cycles/op=458.01
```


