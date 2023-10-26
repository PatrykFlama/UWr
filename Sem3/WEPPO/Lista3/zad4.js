function sum(...args) {
    return args.reduce((acc, val) => acc + val);
}

console.log(sum(1, 2, 3));
// 6
console.log(sum(1, 2, 3, 4, 5));
// 15