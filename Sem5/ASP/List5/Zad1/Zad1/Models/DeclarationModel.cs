using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.Linq;
using System.Web;

namespace Zad1.Models
{
    public class DeclarationModel
    {
        [Required]
        public string Name { get; set; }
        [Required]
        public string Course { get; set; }
        [Required]
        public int List {  get; set; }
        public int?[] Points { get; set; }

    }
}