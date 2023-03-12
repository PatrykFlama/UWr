using System

class Vector
{
    int size;
    float[] coords;
    public Vector(int size)
    {
        this.size = size;
        for(int i = 0; i < size; i++)
        {
            coords[i] = 0;
        }
    }
    public int size()
    {
        return this.size;
    }
    public float[] tab()
    {
        return coords;
    }

    public void set_vector(float[] tab)
    {
        if(tab.Length() != size) return;
        else
        {
            for(int i = 0; i < size; i++)
            {
                coords[i] = tab[i];
            }
        }
    }
    public void add(Vector V)
    {
        if(V.size() == this.size)
        {
            for(int i = 0; i < size; i++)
            {
                coords[i] += V.tab()[i];
            }
        }
    }
    public void mult(float S)
    {
        for(int i = 0; i < size; i++)
        {
            coords[i] *= S;
        }
    }
    public float scalar_mult(Vector V)
    {
        float res = 0.0;
        if(V.size() == this.size)
        {
            for(int i = 0; i < size; i++)
            {
                res += V.tab()[i]*coords[i];
            }
        }
        return res;
    }
    public float distance()
    {
        return MathF.Sqrt(this.scalar_mult(this));
    }
    public void print()
    {
        for(int i = 0; i < size; i++)
        {
            Console.Write(coords[i]);
            Console.Write(' ');
        }
        Console.Write('\n');
    }
}


class Program
{
    public static void Main()
    {
        Vector V = new Vector(3);
        V.set_vector({3, 2, 4});
        V.mult(4);
        V.print();
    }
}