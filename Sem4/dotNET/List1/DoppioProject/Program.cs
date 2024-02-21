using Libone;
using Libduo;

int a = 100;
int b = 123;
TheGreatAdder tga = new(a, b);

Console.WriteLine(Textify.Exec("Hello world"));
Console.WriteLine(Textify.Exec(tga.add()));