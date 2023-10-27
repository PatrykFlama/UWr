function getLastProto(o) {
    var p = o;
    do {
        o = p;
        p = Object.getPrototypeOf(o);
    } while (p);
    
    return o;
}

// Define some objects
const obj1 = {};
const obj2 = {};
const obj3 = {};

// Set up prototype chain
Object.setPrototypeOf(obj2, obj1);
Object.setPrototypeOf(obj3, obj2);

// Test if prototype chain converges to the same object
console.log(getLastProto(obj1) === getLastProto(obj2) && 
            getLastProto(obj2) === getLastProto(obj3)); // true
