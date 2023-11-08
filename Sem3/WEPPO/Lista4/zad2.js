var n = {
    name: 'jan'
}
var q = {
    surname: 'kowalski'
}
Object.setPrototypeOf(n, q);
console.log(n.name);
console.log(n.surname);


function owns(obj, prop) {
    return obj.hasOwnProperty(prop); // prop in Object.getPrototypeOf(obj) to check if is in prototype
}

// Example usage
console.log('---------')
console.log(owns(n, 'name')); // true
console.log(owns(n, 'surname')); // false


function getFromObject(obj){
    for (var prop in obj) 
        if (owns(obj, prop)) 
            console.log(prop);
}

// Example usage
console.log('---------')
getFromObject(n); // name: jan, surname: kowalski


function getFieldsFromChain(obj){
    while (obj) {
        for (var prop in obj) 
            // if (owns(obj, prop)) 
                console.log(prop);

        // obj = Object.getPrototypeOf(obj);
    }
}

// Example usage
console.log('---------')
getFieldsFromChain(n); // name: jan, surname: kowalski
