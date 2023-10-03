function nthFibRec(n){
    if(n == 0 || n == 1)
        return n;
    return nthFibRec(n-1) + nthFibRec(n-2);
}

function nthFibIt(n){
    let fib1 = 0, fib2 = 1;
    for(let i = 0; i < n; i++){
        let temp = fib1;
        fib1 = fib2;
        fib2 += temp;
    }
    return fib1;
}

function measureTime(func, name, n){
    console.time(name);
    func(n);
    console.timeEnd(name);
}


for(let i = 10; i <= 40; i++) measureTime(nthFibRec, "nthFibRec", i);
for(let i = 10; i <= 40; i++) measureTime(nthFibIt, "nthFibIt", i);

