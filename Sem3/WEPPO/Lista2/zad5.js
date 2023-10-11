var obj = {
    field: 0,
    method: function() {
        return "stuff";
    },
    get: field(){
        return this.field;
    },
    set: field(_field){
        this.field = _field;
    }
}