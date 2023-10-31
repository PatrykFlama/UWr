function fib(n) {
    if (n < 2) {
        return n;
    }
    return fib(n - 1) + fib(n - 2);
}

var cache_glob = {};
function fib_memo(n) {
    if (n in cache_glob) {
        return cache_glob[n];
    } else {
        if (n < 2) {
            return n;
        }
        cache_glob[n] = fib_memo(n - 1) + fib_memo(n - 2);
        return cache_glob[n];
    }
}

function memoize(fn) {
    var cache = {};
    return function (n) {
        if (n in cache) {
            return cache[n]
        } else {
            var result = fn(n);
            cache[n] = result;
            return result;
        }
    }
}


function measure_time(fun, n) {
    console.time();
    fun(n);
    console.timeEnd();
}


iter = 40
console.log("Fib normal recursion: ");
measure_time(fib, iter);
console.log("Fib memoization with global cache recursion: ");
measure_time(fib_memo, iter);
fib = memoize(fib);
console.log("Fib memoization with local cache recursion: ");
measure_time(fib, iter);
console.log("Fib memoization with local cache recursion: ");
measure_time(fib, iter);

