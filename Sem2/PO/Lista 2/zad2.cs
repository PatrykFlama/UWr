/*
Patryk Flama - lista 2 zadanie 2
kompilacja: mcs .\zad2.cs
*/

using System;

class BigNum {
    string num;
    
    public BigNum(int n){
        num = "";
        int i;
        for(i = 10; i <= n; i *= 10) {}
        for(; i >= 10; i /= 10){
            num += (n%i)/(i/10);
        } 
    }

    public void Print(){
        Console.WriteLine(num);
    }

    private string Reverse(string s){   //  stackoverflow.com/questions/228038
        string res = "";
        for(int i = s.Length-1; i >= 0; i--) res += s[i];
        return res;
    }

    public void Add(BigNum a){
        string num2 = a.num;
        string res = "";
        int overflow = 0;

        int i = 0;
        for(; i < Math.Min(num2.Length, num.Length); i++){
            int temp = num[num.Length-i-1] + num2[num2.Length-i-1] - 2*'0' + overflow;
            res += (char)(temp%10 + '0');
            overflow = temp/10;
        }

        if(num2.Length > num.Length){
            for(; i < num2.Length; i++){
                int temp = num2[num2.Length-i-1] - '0' + overflow;
                res += (char)(temp%10 + '0');
                overflow = temp/10;
            }
        } else{
            for(; i < num.Length; i++){
                int temp = num[num.Length-i-1] - '0' + overflow;
                res += (char)(temp%10 + '0');
                overflow = temp/10;
            }
        }

        if(overflow == 1) res += '1';
        num = Reverse(res);
    }

    public void Subtract(BigNum a){
        string sub = a.num;
        string res = "";
        int overflow = 0;

        int i = 0;
        for(; i < Math.Min(sub.Length, num.Length); i++){
            int temp = num[num.Length-i-1] - sub[sub.Length-i-1] + overflow;
            if(temp < 0){
                temp += 10;
                overflow = -1;
            } else overflow = 0;

            res += (char)(temp + '0');
        }

        if(sub.Length == num.Length && ){

        }

        if(sub.Length > num.Length){
            for(; i < sub.Length; i++){
                int temp = num[num.Length-i-1] - sub[sub.Length-i-1] + overflow;
                if(temp < 0){
                    temp += 10;
                    overflow = -1;
                } else overflow = 0;

                res += (char)(temp + '0');
            }
        } if(sub.Length <= num.Length){
            for(; i < num.Length; i++){
                int temp = num[num.Length-i-1] + overflow;
                if(temp < 0){
                    temp += 10;
                    overflow = -1;
                } else overflow = 0;

                res += (char)(temp + '0');
            }
        }

        num = Reverse(res);
    }
}

class Program {
    public static void Main(string[] args){
        BigNum an = new BigNum(789);
        BigNum bn = new BigNum(253);
        an.Print();
        bn.Print();
        an.Subtract(bn);
        an.Print();
    }
}