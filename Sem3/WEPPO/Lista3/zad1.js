function fib(n) {
    if (n < 2) {
        return n;
    }
    return fib(n - 1) + fib(n - 2);
}

var cache = {};
function fib_memo(n){
    if (n in cache) {
        return cache[n];
    } else {
        if (n < 2) {
            return n;
        }
        cache[n] = fib_memo(n - 1) + fib_memo(n - 2);
        return cache[n];
    }
}

function measure_time(fun, n){
    console.time();
    fun(n);
    console.timeEnd();
}


iter = 40
console.log("Fib normal recursion: ");
measure_time(fib, iter);
console.log("Fib memoization recursion: ");
measure_time(fib_memo, iter);

