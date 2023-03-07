using System;


class IntStream {
    int val;

    public IntStream(){
        this.val = 0;
    }

    public int actual(){
        return this.val;
    }

    public int predict_next(int n){
        return n + 1;
    }

    public virtual int next(){
        this.val += 1;
        return this.val;
    }

    public virtual bool eos(){
        if(this.val > Int32.MaxValue) return true;
        return false;
    }

    public void reset(){
        this.val = 0;
    }
};

class PrimeStream : IntStream {
    // const int L = 1e6;  // max prime
    // bool primeTable[L];

    // public PrimeStream(){
    //     for(int i = 0; i < L; i++) primeTable[i] = true;
        
    //     primeTable[0] = primeTable[1] = false;
    //     for(int i = 2; i < L; i++){
    //         if(primeTable[i]){
    //             for(int j = i*i; j < L; j++) primeTable[j] = false;
    //         }
    //     }
    // }

    private bool IsPrime(int n){
        // return primeTable[n];
        if(n == 1) return false;
        for(int i = 2; i*i <= n; i++){
            if(n%i == 0) return false;
        }
        return true;
    }

    public override int next(){
        int next = base.next();
        while(!IsPrime(next)) next = base.next();
        return next;
    }

    public override bool eos(){
        int actual = base.actual();
        while(!IsPrime(actual) && actual < Int32.MaxValue) actual = base.predict_next(actual);
        if(!IsPrime(actual)) return false;
        return true;
    }
};

class RandomStream : IntStream {
    Random rnd;

    public RandomStream(){
        rnd = new Random();
    }

    public override int next(){
        return rnd.Next();
    }

    public override bool eos(){
        return false;
    }
};

class Program{
    public static void Main(string[] args){
        Console.WriteLine("Hello World!");
        PrimeStream ins = new PrimeStream();
        for(int i = 0; i < 10; i ++ ){  /   // TODO: check if primes tream eos is working
             Console.WriteLine(ins.next(), ins.eos());
        }
        
    }
}
