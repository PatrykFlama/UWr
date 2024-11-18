using System.ComponentModel.DataAnnotations;

namespace Zad2.Models
{
    public class DeclarationModel
    {
        [Required]
        public string Name { get; set; }
        [Required]
        public string Course { get; set; }
        [Required]
        public int List { get; set; }
        public int?[] Points { get; set; }
    }
}
