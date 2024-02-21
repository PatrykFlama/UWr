using Libone;
using Libduo;

Prog2 p = new();
p.exec();

class Prog2
{

    public void exec()
    {
        int n = 23493;
        int a = 100;
        int b = 123;

        Console.WriteLine(Textify.Exec(n));
        Console.WriteLine(Textify.Exec(a));
        Console.WriteLine(Textify.Exec(b));

        TheGreatAdder tga = new(a, b);
        int c = tga.add();
        Console.WriteLine(Textify.Exec(c));
    }
}

