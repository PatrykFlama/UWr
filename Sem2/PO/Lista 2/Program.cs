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
    private bool IsPrime(int n){
        if(n < 2) return false;
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
        int actual = base.predict_next(base.actual());
        while(!IsPrime(actual) && actual < Int32.MaxValue) actual = base.predict_next(actual);
        if(!IsPrime(actual)) return true;
        return false;
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
        PrimeStream ins = new PrimeStream();
        for(int i = 0; i < 10 && !ins.eos(); i++){
             Console.WriteLine(ins.next());
        }
    }
}
