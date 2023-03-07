/*
Patryk Flama - lista 2 zadanie 2
kompilacja: mcs .\zad2.cs
*/

using System;

class BigNum {
    string num;
    bool negative;
    
    public BigNum(int n){
        if(n > 0) negative = false;
        else {n = -n; negative = true;}

        num = "";
        int i;
        for(i = 10; i <= n; i *= 10) {}
        for(; i >= 10; i /= 10){
            num += (n%i)/(i/10);
        }
    }

    public void Print(){
        if(negative) Console.Write('-');
        Console.WriteLine(num);
    }

    private string Reverse(string s){   //  stackoverflow.com/questions/228038
        string res = "";
        for(int i = s.Length-1; i >= 0; i--) res += s[i];
        return res;
    }

    public void Add(BigNum a){
        if(!negative && a.negative){
            Subtract(a);
            return;
        } else if(negative && !a.negative){
            negative = false;
            Subtract(a);
            negative = !negative;
            return;
        }

        string num2 = a.num;
        string res = "";
        int overflow = 0;

        int i = 0;
        for(; i < Math.Min(num2.Length, num.Length); i++){
            int add = num[num.Length-i-1] + num2[num2.Length-i-1] - 2*'0' + overflow;
            res += (char)(add%10 + '0');
            overflow = add/10;
        }

        if(num2.Length > num.Length){
            for(; i < num2.Length; i++){
                int add = num2[num2.Length-i-1] - '0' + overflow;
                res += (char)(add%10 + '0');
                overflow = add/10;
            }
        } else{
            for(; i < num.Length; i++){
                int add = num[num.Length-i-1] - '0' + overflow;
                res += (char)(add%10 + '0');
                overflow = add/10;
            }
        }

        if(overflow == 1) res += '1';
        num = Reverse(res);
    }

    private bool stringSmaller(string str){
        int n1 = num.Length, n2 = str.Length;
        
        if (n1 < n2)
            return true;
        if (n2 < n1)
            return false;
        
        for(int i = 0; i < n1; i++)
            if (num[i] < str[i])
                return true;
            else if (num[i] > str[i])
                return false;
        
        return false;
    }

    public void Subtract(BigNum a){
        bool negation = false;
        if(!negative && a.negative) {
            a.negative = false;
            Add(a);
            return;
        } else if(negative && !a.negative){
            a.negative = true;
            Add(a);
            return;
        } else if(negative && !a.negative){
            negation = true;
        }

        string str1 = num;
        string str2 = a.num;

        if (stringSmaller(str2)){
            negation = true;
            string temp = str1;
            str1 = str2;
            str2 = temp;
        }
    
        string res = "";
    
        int len1 = str1.Length;
        int len2 = str2.Length;
    
        str1 = Reverse(str1);
        str2 = Reverse(str2);
    
        int overflow = 0;
        for (int i = 0; i < len2; i++) {
            int sub = ((str1[i] - '0') - (str2[i] - '0') - overflow);

            if (sub < 0) {
                sub = sub + 10;
                overflow = 1;
            } else overflow = 0;

            res += (char)(sub + '0');
        }
    
        for (int i = len2; i < len1; i++) {
            int sub = ((str1[i] - '0') - overflow);
    
            if (sub < 0) {
                sub = sub + 10;
                overflow = 1;
            } else overflow = 0;
    
            res += (char)(sub + '0');
        }

        if(negation) negative = !negative;

        num = Reverse(res);
    }
}

class Program {
    public static void Main(string[] args){
        BigNum an = new BigNum(253);
        BigNum bn = new BigNum(789);

        Console.Write("Enter first number:");
        int temp = Convert.ToInt32(Console.ReadLine());
        an = new BigNum(temp);

        Console.Write("Enter second number:");
        temp = Convert.ToInt32(Console.ReadLine());
        bn = new BigNum(temp);

        Console.Write("Enter operation (+/-):");
        string op = Console.ReadLine();
        if(op[0] == '+'){
            an.Add(bn);
        } else{
            an.Subtract(bn);
        }

        an.Print();
    }
}