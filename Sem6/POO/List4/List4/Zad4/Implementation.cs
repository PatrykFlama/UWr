using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

public class TagBuilder
{
    private string tagName;
    private TagBuilder parent;
    private StringBuilder body = new StringBuilder();
    private Dictionary<string, string> _attributes = new Dictionary<string, string>();
    private int currentIndentation = 0;
    private int indentationStep = 4;

    public bool IsIndented { get; set; } = false;
    public int Indentation { get; set; } = 4;

    public TagBuilder() { }

    public TagBuilder(string tagName, TagBuilder parent)
    {
        this.tagName = tagName;
        this.parent = parent;
    }

    public TagBuilder AddContent(string content)
    {
        body.Append(content);
        return this;
    }

    public TagBuilder AddContentFormat(string format, params object[] args)
    {
        body.AppendFormat(format, args);
        return this;
    }

    public TagBuilder StartTag(string tagName)
    {
        TagBuilder tag = new TagBuilder(tagName, this);
        return tag;
    }

    public TagBuilder EndTag()
    {
        parent.AddContent(this.ToString());
        return parent;
    }

    public TagBuilder AddAttribute(string name, string value)
    {
        _attributes.Add(name, value);
        return this;
    }

    public override string ToString()
    {
        StringBuilder tag = new StringBuilder();

        string indent = IsIndented ? new string(' ', currentIndentation * Indentation) : "";

        // Start tag
        if(!string.IsNullOrEmpty(this.tagName))
        {
            tag.AppendFormat("{0}<{1}", indent, tagName);
            if(_attributes.Count > 0)
            {
                tag.Append(" ");
                tag.Append(string.Join(" ",
                    _attributes.Select(kvp => string.Format("{0}='{1}'", kvp.Key, kvp.Value)).ToArray()));
            }
            tag.Append(">" + Environment.NewLine);
            currentIndentation++;
        }

        // Body
        if(body.Length > 0)
        {
            tag.Append(indent + body.ToString() + Environment.NewLine);
        }

        // End tag
        if(!string.IsNullOrEmpty(this.tagName))
        {
            currentIndentation--;
            tag.AppendFormat("{0}</{1}>" + Environment.NewLine, indent, tagName);
        } else
        {
            tag.Append(indent + "/>" + Environment.NewLine);
        }

        return tag.ToString();
    }
}


