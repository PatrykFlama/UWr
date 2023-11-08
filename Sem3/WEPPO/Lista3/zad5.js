function createGenerator() {
    var _state = 0;
    return {
        next: function () {
            return {
                value: _state,
                done: _state++ >= 10
            }
        }
    }
}

var foo = {
    [Symbol.iterator]: createGenerator
};
for (var f of foo)
    console.log(f);
        
/*
function createGeneratorParam(end) {
    var _state = 0;
    return {
        next: function () {
            return {
                value: _state,
                done: _state++ >= end
            }
        }
    }
}

var foo1 = {
    [Symbol.iterator]: createGeneratorParam.bind(null, 3)
};
for (var f of foo1)
    console.log(f);

var foo2 = {
    [Symbol.iterator]: createGeneratorParam.bind(null, 6)
};
for (var f of foo2)
    console.log(f);
*/

function createGeneratorParam(end) {
    return function() {
        var _state = 0;
        return {
            next: function () {
                return {
                    value: _state,
                    done: _state++ >= end
                }
            }
        }
    }
}

var foo3 = {
    [Symbol.iterator]: createGeneratorParam(6)
};
for (var f of foo3)
    console.log(f);
