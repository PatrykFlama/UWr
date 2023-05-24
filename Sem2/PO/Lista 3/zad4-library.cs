/*
Patryk Flama - lista 3 zadanie 4
mcs -target:library -out:lista.dll zad4-library.cs
*/

using System;


public class Vector{
    public float[] vals;
    public int size;

    public Vector(int _size){
        size = _size;
        vals = new float[size];
        for(int i = 0; i < size; i++) vals[i] = 0;
    }

    public static Vector operator +(Vector v1, Vector v2){
        Vector res = new Vector(v1.size);
        if(v1.size != v2.size) return res;

        for(int i = 0; i < v1.size; i++){
            res.vals[i] = v1.vals[i] + v2.vals[i];
        }
        return res;
    }

    public static float operator *(Vector v1, Vector v2){
        float res = 0;
        if(v1.size != v2.size) return res;

        for(int i = 0; i < v1.size; i++){
            res += v1.vals[i] * v2.vals[i];
        }
        return res;
    }

    public static Vector operator *(Vector v1, float f){
        Vector res = new Vector(v1.size);

        for(int i = 0; i < v1.size; i++){
            res.vals[i] = v1.vals[i] * f;
        }
        return res;
    }

    public float dot_product(Vector v){
        if(size != v.size) return 0;
        
        float res = 0;
        for(int i = 0; i < size; i++){
            res += vals[i] * v.vals[i];
        }
        return res;
    }

    public void mult(float v){
        for(int i = 0; i < size; i++){
            vals[i] *= v;
        }
    }

    float sqrt(float x){
        float l = 0, r = x;
        for(int i = 0; i < 100; i++){
            float mid = (l+r)/2;
            if(mid*mid > x) r = mid;
            else l = mid;
        }

        return (l+r)/2;
    }

    public float norm(){
        return sqrt(this*this);
    }

    public void set_vector(float[] tab){
        for(int i = 0; i < size; i++){
            vals[i] = tab[i];
        }
    }

    public void print(){
        for(int i = 0; i < size; i++){
            Console.Write(vals[i]);
            Console.Write(' ');
        }
        Console.WriteLine();
    }
};
