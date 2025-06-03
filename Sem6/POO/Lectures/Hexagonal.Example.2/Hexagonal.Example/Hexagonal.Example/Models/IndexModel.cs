using Model;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;

namespace Hexagonal.Example.Models
{
    public class IndexModel
    {
        public IEnumerable<Person> Persons { get; set; }
    }
}