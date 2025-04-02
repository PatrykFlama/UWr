using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

class Program
{
    static void Main()
    {
        StringWriter writer = new StringWriter(...);
        TagBuilder tag = new TagBuilder(writer);
        tag.IsIndnted = true;
        tag.Indentation = 4;
        tag.StartTag("parent")
            .AddAttribute("parentproperty1", "true")
            .AddAttribute("parentproperty2", "5")
            .StartTag("child1")
            .AddAttribute("childproperty1", "c")
            .AddContent("childbody")
            .EndTag()
            .StartTag("child2")
            .AddAttribute("childproperty2", "c")
            .AddContent("childbody")
            .EndTag()
            .EndTag()
            .StartTag("script")
            .AddContent("$.scriptbody();")
            .EndTag();
    }
}
