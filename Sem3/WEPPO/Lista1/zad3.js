const N = 1e5;

function isPrime(n){
    if(n < 2)
        return false;
    for(let i = 2; i*i <= n; i++)
        if(n % i == 0)
            return false;
    return true;
}

for(let i = 2; i <= N; i++){
    if(isPrime(i))
        console.log(i);
}
