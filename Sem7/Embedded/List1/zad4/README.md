# List 1

**max obciążenie pinu to 40mA**  

więc gdy mamy diodę oraz napięcie 5V - jakiego rezystora potrzebujemy? potrzebujemy spadku 3V, więc $R = \frac{3V}{0.04A} = 75 \Omega$ (ale my będziemy używać $220 \Omega$)  

na naszej płytce mamy porty B,C,D (ale nie wszystkie piny są wyprowadzone)  
`DDRB` oraz `PORTB` to adresy (zmienne w C, ale w zasadzie to to są pseudozmienne)  
`_BV(i)` przesuwa 1 w odpowiednie miejsce (chociaż mamy też `PB5`)  

gdy `DDRB` jest ustawione na 1 to mamy port wejściowy, a gdy 0 to mamy port wejściowy  


> **w rozwiązaniach chcemy optymalizować pamięć**

> warto mieć dedykowanego użytkownika dla prezentacji (z jadnymi colorshemami i dużymi literami)


## Zad 4
zamiast float możemy użyć fixed-point number (ustalona precyzja)  


| | 1 bajt | 2 bajty | 4 bajty | float |
|--|---|---|---|---|
| sum | 1 | 1  | 1 | |
|prod| 2  | 5  | | |

### int8_t
```assembler
000002ea <.Loc.96>:
        int8_t sum8 = a8 + b8;                                       // int8_t: calculate sum
     2ea:	10 0f       	add	r17, r16

000002da <.LVL20>:
        int8_t prod8 = a8 * b8;                                      // int8_t: calculate product
     2da:	10 9f       	mul	r17, r16
     2dc:	80 2d       	mov	r24, r0
     2de:	11 24       	eor	r1, r1

00000306 <.LVL25>:
        int8_t div8 = a8 / b8;                                       // int8_t: calculate division
     306:	c6 01       	movw	r24, r12
     308:	b7 01       	movw	r22, r14
     30a:	0e 94 b9 0c 	call	0x1972	; 0x1972 <__divmodhi4>
```


### int16_t
```assembler
000005c4 <.L24>:
        int16_t sum16 = a16 + b16;                                       // int16_t: calculate sum
     5c4:	eb 8c       	ldd	r14, Y+27	; 0x1b
     5c6:	fc 8c       	ldd	r15, Y+28	; 0x1c
     5c8:	09 8d       	ldd	r16, Y+25	; 0x19
     5ca:	1a 8d       	ldd	r17, Y+26	; 0x1a


000005e0 <.LVL57>:
        int16_t prod16 = a16 * b16;                                      // int16_t: calculate product
     5e0:	e0 9e       	mul	r14, r16
     5e2:	c0 01       	movw	r24, r0
     5e4:	e1 9e       	mul	r14, r17
     5e6:	90 0d       	add	r25, r0
     5e8:	f0 9e       	mul	r15, r16
     5ea:	90 0d       	add	r25, r0
     5ec:	11 24       	eor	r1, r1


00000596 <.LVL59>:
        int16_t div16 = a16 / b16;                                       // int16_t: calculate division
     596:	c7 01       	movw	r24, r14
     598:	b8 01       	movw	r22, r16
     59a:	0e 94 b9 0c 	call	0x1972	; 0x1972 <__divmodhi4>
```

### int32_t
```assembler

000004fe <.Loc.149>:
        int32_t sum32 = a32 + b32;                                       // int32_t: calculate sum
     4fe:	8f 8d       	ldd	r24, Y+31	; 0x1f
     500:	98 a1       	ldd	r25, Y+32	; 0x20
     502:	a9 a1       	ldd	r26, Y+33	; 0x21
     504:	ba a1       	ldd	r27, Y+34	; 0x22
     506:	8c 0d       	add	r24, r12
     508:	9d 1d       	adc	r25, r13
     50a:	ae 1d       	adc	r26, r14
     50c:	bf 1d       	adc	r27, r15

000004e6 <.LVL49>:
        int32_t prod32 = a32 * b32;                                      // int32_t: calculate product
     4e6:	2f 8d       	ldd	r18, Y+31	; 0x1f
     4e8:	38 a1       	ldd	r19, Y+32	; 0x20
     4ea:	49 a1       	ldd	r20, Y+33	; 0x21
     4ec:	5a a1       	ldd	r21, Y+34	; 0x22
     4ee:	b6 01       	movw	r22, r12
     4f0:	c7 01       	movw	r24, r14
     4f2:	0e 94 a9 0c 	call	0x1952	; 0x1952 <__mulsi3>

00001952 <__mulsi3>:
    1952:	db 01       	movw	r26, r22

00000522 <.LVL50>:
        int32_t div32 = a32 / b32;                                       // int32_t: calculate division
     522:	6f 8d       	ldd	r22, Y+31	; 0x1f
     524:	78 a1       	ldd	r23, Y+32	; 0x20
     526:	89 a1       	ldd	r24, Y+33	; 0x21
     528:	9a a1       	ldd	r25, Y+34	; 0x22
     52a:	96 01       	movw	r18, r12
     52c:	a7 01       	movw	r20, r14
     52e:	0e 94 cd 0c 	call	0x199a	; 0x199a <__divmodsi4>

0000199a <__divmodsi4>:
    199a:	05 2e       	mov	r0, r21
```

### int64_t
```assembler
0000041e <.LVL42>:
        int64_t prod64 = a64 * b64;                                                        // int64_t: calculate product
     41e:	6f 8d       	ldd	r22, Y+31	; 0x1f
     420:	78 a1       	ldd	r23, Y+32	; 0x20
     422:	89 a1       	ldd	r24, Y+33	; 0x21
     424:	9a a1       	ldd	r25, Y+34	; 0x22
     426:	97 01       	movw	r18, r14
     428:	a8 01       	movw	r20, r16
     42a:	0e 94 f7 0c 	call	0x19ee	; 0x19ee <__mulsidi3>

00000472 <.Loc.137>:
        int64_t div64 = a64 / b64;                                                         // int64_t: calculate division
     472:	57 01       	movw	r10, r14
     474:	68 01       	movw	r12, r16
     476:	e8 2e       	mov	r14, r24
```

### float
```assembler
00000330 <.L20>:
        float sumf = af + bf;                        // float: calculate sum
     330:	3d 85       	ldd	r19, Y+13	; 0x0d
     332:	3f 8f       	std	Y+31, r19	; 0x1f
     334:	8e 85       	ldd	r24, Y+14	; 0x0e
     336:	8b a3       	std	Y+35, r24	; 0x23
     338:	9f 85       	ldd	r25, Y+15	; 0x0f
     33a:	9f a3       	std	Y+39, r25	; 0x27
     33c:	a8 88       	ldd	r10, Y+16	; 0x10
     33e:	f9 84       	ldd	r15, Y+9	; 0x09
     340:	0a 85       	ldd	r16, Y+10	; 0x0a
     342:	1b 85       	ldd	r17, Y+11	; 0x0b
     344:	ac 85       	ldd	r26, Y+12	; 0x0c
     346:	a8 a7       	std	Y+40, r26	; 0x28

0000038e <.Loc.117>:
        float sumf = af + bf;                        // float: calculate sum
     38e:	2b 2d       	mov	r18, r11
     390:	3c 2d       	mov	r19, r12
     392:	4d 2d       	mov	r20, r13
     394:	5e 2d       	mov	r21, r14
     396:	6f 2d       	mov	r22, r15
     398:	70 2f       	mov	r23, r16
     39a:	81 2f       	mov	r24, r17
     39c:	98 a5       	ldd	r25, Y+40	; 0x28
     39e:	0e 94 d0 0a 	call	0x15a0	; 0x15a0 <__addsf3>

000015a0 <__addsf3>:
    15a0:	bb 27       	eor	r27, r27
    15a2:	aa 27       	eor	r26, r26
    15a4:	0e 94 e7 0a 	call	0x15ce	; 0x15ce <__addsf3x>
    15a8:	0c 94 02 0c 	jmp	0x1804	; 0x1804 <__fp_round>

000015ce <__addsf3x>:
    15ce:	e9 2f       	mov	r30, r25
    15d0:	0e 94 13 0c 	call	0x1826	; 0x1826 <__fp_split3>
    15d4:	58 f3       	brcs	.-42     	; 0x15ac <.L0^B1>
    15d6:	ba 17       	cp	r27, r26
    15d8:	62 07       	cpc	r22, r18
    15da:	73 07       	cpc	r23, r19
    15dc:	84 07       	cpc	r24, r20
    15de:	95 07       	cpc	r25, r21
    15e0:	20 f0       	brcs	.+8      	; 0x15ea <.L2^B1>
    15e2:	79 f4       	brne	.+30     	; 0x1602 <.L4^B1>
    15e4:	a6 f5       	brtc	.+104    	; 0x164e <.L_add>
    15e6:	0c 94 35 0c 	jmp	0x186a	; 0x186a <__fp_zero>

00000364 <.LVL30>:
        float prodf = af * bf;                       // float: calculate product
     364:	bf 2c       	mov	r11, r15
     366:	68 01       	movw	r12, r16
     368:	e8 a4       	ldd	r14, Y+40	; 0x28
     36a:	2f 2d       	mov	r18, r15
     36c:	3c 2d       	mov	r19, r12
     36e:	4d 2d       	mov	r20, r13
     370:	5e 2d       	mov	r21, r14
     372:	ff 8c       	ldd	r15, Y+31	; 0x1f


000003b6 <.LVL36>:
        float divf = af / bf;                        // float: calculate division
     3b6:	2b 2d       	mov	r18, r11
     3b8:	3c 2d       	mov	r19, r12
     3ba:	4d 2d       	mov	r20, r13
     3bc:	5e 2d       	mov	r21, r14
     3be:	6f 2d       	mov	r22, r15
     3c0:	70 2f       	mov	r23, r16
     3c2:	81 2f       	mov	r24, r17
     3c4:	98 a5       	ldd	r25, Y+40	; 0x28
     3c6:	0e 94 3c 0b 	call	0x1678	; 0x1678 <__divsf3>

00001678 <__divsf3>:
    1678:	0e 94 50 0b 	call	0x16a0	; 0x16a0 <__divsf3x>
    167c:	0c 94 02 0c 	jmp	0x1804	; 0x1804 <__fp_round>

000016a0 <__divsf3x>:
    16a0:	0e 94 13 0c 	call	0x1826	; 0x1826 <__fp_split3>
    16a4:	68 f3       	brcs	.-38     	; 0x1680 <.L0^B1>

00001826 <__fp_split3>:
    1826:	57 fd       	sbrc	r21, 7
    1828:	90 58       	subi	r25, 0x80	; 128
    182a:	44 0f       	add	r20, r20
    182c:	55 1f       	adc	r21, r21
    182e:	59 f0       	breq	.+22     	; 0x1846 <.L4^B1>
    1830:	5f 3f       	cpi	r21, 0xFF	; 255
    1832:	71 f0       	breq	.+28     	; 0x1850 <.L5^B1>

00001826 <__fp_split3>:
    1826:	57 fd       	sbrc	r21, 7
    1828:	90 58       	subi	r25, 0x80	; 128
    182a:	44 0f       	add	r20, r20
    182c:	55 1f       	adc	r21, r21
    182e:	59 f0       	breq	.+22     	; 0x1846 <.L4^B1>
    1830:	5f 3f       	cpi	r21, 0xFF	; 255
    1832:	71 f0       	breq	.+28     	; 0x1850 <.L5^B1>

00001804 <__fp_round>:
    1804:	09 2e       	mov	r0, r25
    1806:	03 94       	inc	r0
    1808:	00 0c       	add	r0, r0
    180a:	11 f4       	brne	.+4      	; 0x1810 <.L1^B1>
    180c:	88 23       	and	r24, r24
    180e:	52 f0       	brmi	.+20     	; 0x1824 <.L3^B1>
```
