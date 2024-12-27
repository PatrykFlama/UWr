using System;
using System.Collections.Generic;
using System.Net.Http;
using System.Text;
using System.Text.Json;
using System.Threading.Tasks;

namespace Client
{
    class Program
    {
        static HttpClient client = new HttpClient { BaseAddress = new Uri("http://localhost:5298/") };

        static async Task Main(string[] args)
        {
            await PostPerson(new Person { Name = "Alice", Surname = "Johanson" });
            await GetPeople();
            Console.ReadLine();
        }

        static async Task GetPeople()
        {
            var response = await client.GetStringAsync("api/person");
            var people = JsonSerializer.Deserialize<List<Person>>(response);
            Console.WriteLine("People:");
            people.ForEach(p => Console.WriteLine($"{p.ID} - {p.Name} {p.Surname} "));
        }

        static async Task PostPerson(Person person)
        {
            var content = new StringContent(JsonSerializer.Serialize(person), Encoding.UTF8, "application/json");
            var response = await client.PostAsync("api/person", content);
            Console.WriteLine($"POST response: {response.StatusCode}");
        }

        public class Person
        {
            public int? ID { get; set; }
            public string Name { get; set; }
            public string Surname { get; set; }
        }
    }
}