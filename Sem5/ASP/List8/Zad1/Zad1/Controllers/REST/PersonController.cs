using DatabaseModels;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Mvc;
using System;
using Zad1.Data;

namespace Zad1.Controllers.REST
{
    [Route("api/[controller]")]
    [ApiController]
    public class PersonController : ControllerBase
    {
        private readonly AppDbContext _context;

        public PersonController(AppDbContext context)
        {
            _context = context;
        }

        [HttpGet]
        public async Task<IActionResult> Get()
        {
            var people = _context.People.ToList();
            return Ok(people);
        }

        [HttpPost]
        public async Task<IActionResult> Post([FromBody] Person person)
        {
            if(person == null)
                return BadRequest("Invalid data.");

            _context.People.Add(person);
            await _context.SaveChangesAsync();
            return CreatedAtAction(nameof(Get), new { id = person.Id }, person);
        }
    }
}
