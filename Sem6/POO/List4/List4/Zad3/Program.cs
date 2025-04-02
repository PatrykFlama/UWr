using System;
using System.Collections.Concurrent;


Console.WriteLine("Creating a BetterReusable instance...");
var reusable = new BetterReusable();
reusable.DoWork();
Console.WriteLine("Releasing the resource...");
reusable.Release();

try
{
    reusable.DoWork();
}
catch(Exception ex)
{
    Console.WriteLine("Exception caught: " + ex.Message);
}

