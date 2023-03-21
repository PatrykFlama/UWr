/*
Patryk Flama - lista 4 zadanie 2
kompilacja: mcs zad2.cs
*/

using System;
using System.Collections;

class Element{
    int n;
    int act;

    public Element(int _n){
        n = _n;
        reset();
    }
    public Element(int _n, int _act){
        n = _n;
        act = _act;
    }

    public void next(){
        act += 1;       // or return new Element(n, act+1);
    }

    public bool eos(){
        if(act > n) return true;
        return false;
    }

    public String val(){
        String s1 = "b", s2 = "a";
        for(int i = 1; i < act; i++){
            String temp = s1;
            s1 = s2;
            s2 = s2 + temp;
        }
        return s1;
    }

    public void reset(){
        act = 0;
    }
}

class FWEnum : IEnumerator{
    Element el;
    public FWEnum(Element _el){
        el = _el;
    }

    public object Current{
        get{
            return el.val();
        }
    }

    public bool MoveNext(){
        this.el.next();     // or this.el = this.el.next();  
        return !this.el.eos();
    }

    public void Reset(){
        el.reset();
    }
}

class FibonacciWords : IEnumerable{
    Element el;
    public FibonacciWords(int _n){
        el = new Element(_n);
    }

    public IEnumerator GetEnumerator(){
        return new FWEnum(el);
    }
}

class Program{
    public static void Main(){
        foreach(String str in new FibonacciWords(6)){
            Console.WriteLine(str);
        }
    }
}