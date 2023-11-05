module.exports = { fun_a };
let b = require('./zad1b.js');

function fun_a(n) {
    if ( n > 0 ) {
        console.log(`a: ${n}`);
        b.fun_b(n-1);
    }
}
