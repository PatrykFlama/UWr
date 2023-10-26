function* fib() {
    let a = 0, b = 1;
    while (true) {
        yield a;
        [a, b] = [b, a + b];
    }
}

function* take(it, top) {
    for (let i = 0; i < top; i++) {
        yield it.next().value;
    }
}

// zwróć dokładnie 10 wartości z potencjalnie
// "nieskończonego" iteratora/generatora
for (let num of take(fib(), 10)) {
    console.log(num);
}
