function createFs(n) { // tworzy tablicę n funkcji
    var fs = []; // i-ta funkcja z tablicy ma zwrócić i
    
    var helper = function helper(i) {     // tworzymy funkcję pomocniczą, która dzięki temu że przyjmuje wartość jako argument, zrywa wiązanie z oryginalną zmienną
        fs[i] = function () {
            return i;
        };
    };

    for (var i = 0; i < n; i++) {
        helper(i);
    }
    
    return fs;
}
var myfs = createFs(10);
console.log(myfs[0]()); // zerowa funkcja miała zwrócić 0
console.log(myfs[2]()); // druga miała zwrócić 2
console.log(myfs[7]());

// skorygować można za pomocą let, ponieważ let tworzy zmienną w bloku, natomiast var jest usuwany wtedy, kiedy nie ma już do niego referencji