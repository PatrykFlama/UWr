using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;

[TestClass]
public class TagBuilderTests
{
    [TestMethod]
    public void TagBuilder_ShouldIndentTagsCorrectly()
    {
        StringWriter writer = new StringWriter();
        TagBuilder tag = new TagBuilder(writer);
        tag.IsIndented = true;
        tag.Indentation = 4;

        string result = tag.StartTag("parent")
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
            .ToString();

        string expected = "<parent parentproperty1='true' parentproperty2='5'>
    < child1 childproperty1 = 'c' > childbody </ child1 >
    < child2 childproperty2 = 'c' > childbody </ child2 >
</ parent >\n";

        Assert.AreEqual(expected, result);
    }

    [TestMethod]
    public void TagBuilder_ShouldNotIndentIfIsIndentedIsFalse()
    {
        StringWriter writer = new StringWriter();
        TagBuilder tag = new TagBuilder(writer);
        tag.IsIndented = false;

        string result = tag.StartTag("parent")
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
            .ToString();

        string expected = "<parent parentproperty1='true' parentproperty2='5'><child1 childproperty1='c'>childbody</child1><child2 childproperty2='c'>childbody</child2></parent>\n";

        Assert.AreEqual(expected, result);
    }
}
