const N = 1e5;

function check(i){
    let str = i.toString(), sum = 0;
    for(let j = 0; j < str.length; j++){
        let num = parseInt(str[j]);
        if(i % num != 0) return null;
        sum += num;
    }
    if(i % sum != 0) return null;
    return i;
}

for(let i = 1; i <= N; i++){
    let num = check(i);
    if(num != null) console.log(num);
}

