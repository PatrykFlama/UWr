using System;

class BigNum{
    int val;

    public BigNum(){
        this.val = 0;
    }

    void Print(){
        Console.WriteLine(this.val);
    }

    void Add(BigNum a){
        this.val += a;
    }
}

class Program{
    public static void Main(string[] args){
        Console.WriteLine("Hello World!");
    }
}
