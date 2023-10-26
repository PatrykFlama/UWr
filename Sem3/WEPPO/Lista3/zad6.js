function fibIT() {
    var a = 0, b = 1;
    return {
        next: function () {
            [a, b] = [b, a + b];
            return {
                value: a,
                done: false,
            }
        }
    }
}

function* fibGEN() {
    let a = 0, b = 1;
    while (true) {
        yield a;
        [a, b] = [b, a + b];
    }
}

var option = 0;

if(option == 0){
    var _it = fibIT();
    for (var _result; _result = _it.next(), !_result.done;) {
        console.log(_result.value);
    }
} else if(option == 1){
    _it = fibGEN();
    for (var _result; _result = _it.next(), !_result.done;) {
        console.log(_result.value);
    }
} else if(option == 2){
    for (var i of fibIT()) {
        console.log(i);
    }
} else{
    for (var i of fibGEN()) {
        console.log(i);
    }
}
