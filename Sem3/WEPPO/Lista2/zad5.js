const obj = {
    x: 'Hello World!',
    fun: function() {
        console.log(this.x);
    },
    get stuff() {
        return this.x;
    },
    set stuff(n) {
        this.x = n;
    }
};

obj.fun();
console.log(obj.stuff);
obj.stuff = 'Hello World Again!';
console.log(obj.stuff);

obj.y = 'Goodbye World!';
obj.misery = function() {
    console.log(this.y);
}
Object.defineProperty(obj, 'things', {
    get: function() {
        return this.y;
    },
    set: function(n) {
        this.y = n;
    }
});

obj.misery();
console.log(obj.things);
obj.things = 'Goodbye World Again!';
console.log(obj.things);

// we must use defineProperty to add/modify getters and setters to an object
// we can use defineProperty to add/modify values and functions
