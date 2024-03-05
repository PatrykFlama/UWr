using System;

public class Person1
{
    private string _name; // pole kopii zapasowej

    public string Name
    {
        get { return _name; }
        set { _name = value; }
    }
}

public class Person2
{
    // Właściwość implementowana automatycznie
    public string Name { get; set; }
}




