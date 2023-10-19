function forEach(a, f) {
    for(var i = 0; i < a.length; i++){
        f(a[i]);
    }
}
function map(a, f) {
    var res = [];
    forEach(a, _ => res.push(f(_)));
    return res;
}
function filter(a, f) {
    var res = []
    forEach(a, _ => { if(f(_)) res.push(_); });
    return res;
}


// ------ examples ------
var a = [1, 2, 3, 4];
console.log(a);
console.log("foreach");
forEach(a, _ => { console.log(_); });
// [1,2,3,4]
console.log("filter");
console.log(filter(a, _ => _ < 3));
// [1,2]
console.log("map");
console.log(map(a, _ => _ * 2));
// [2,4,6,8]

function print_two_times(a) {
    console.log(a);
    console.log(a);
}
function mult_by_2(a) {
    return a * 2;
}
function check_if_even(a) {
    return a % 2 == 0;
}

forEach(a, print_two_times);
console.log(map(a, mult_by_2));
console.log(filter(a, check_if_even));
