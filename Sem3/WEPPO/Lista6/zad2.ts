let fib: Function = (n: number): number => {
    if (n < 2) {
        return n;
    }
    return fib(n - 1) + fib(n - 2);
}

function memoize(fn: Function): Function {
    var cache: any = {};
    return function (n: number) {
        if(n in cache)
            return cache[n];
        else {
            let result = fn(n);
            cache[n] = result;
            return result;
        }
    }
}


fib = memoize(fib);
console.log("------");
console.log(fib(40));
console.log(fib(40));


// ------- V2 ---------

function fib2(n: number): number {
    if (n < 2) {
        return n;
    }
    return fib(n - 1) + fib(n - 2);
}

function _memoize2(fn: Function): Function {
    var cache: any = {};
    return function (n: number) {
        if(n in cache)
            return cache[n];
        else {
            let result = fn(n);
            cache[n] = result;
            return result;
        }
    }
}

function memoize2(fn: Function): Function {
    var res = fn;
    res = _memoize2(res);
    return res;
}


let fib2_f = memoize2(fib2);
console.log("------");
console.log(fib2_f(40));