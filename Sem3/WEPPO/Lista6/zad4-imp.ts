// domyślny vs nazwany:
// domyślny - do eksportu używamy dowolnej nazwy naszego obieku,
// a przy imporcie również możemy użyć dowolnej nazwy
// nazwany - do eksportu używamy nazwy naszego obiektu,
// a przy imporcie musimy użyć tej samej nazwy


import myObj from './zad4-eks.ts';
const ins1 = new myObj();


import { Foo2 } from './zad4-eks.ts';
const ins2 = new Foo2();

import { Foo2 as Foo3 } from './zad4-eks.ts';
const ins3 = new Foo3();

// import { Foo } from './zad4-eks.ts'; // "./zad4-eks.ts" has no exported member named 'Foo'
