from transformers import pipeline, set_seed

generator = pipeline('text-generation', model='eryk-mazus/polka-1.1b-chat', device=0)

samples = [
    ("I like to read books in the evening.", "Lubię czytać książki wieczorem."),
    ("She is walking her dog in the park.", "Ona spaceruje z psem w parku."),
    ("He enjoys cooking Italian food.", "On lubi gotować włoską kuchnię."),
]

# prompt = """Oto zdania w języku angielskim wraz z ich tłumaczeniami na język polski:
# I like to read books in the evening. Lubię czytać książki wieczorem.
# She is walking her dog in the park. Ona spaceruje z psem w parku.
# He enjoys cooking Italian food. On lubi gotować włoską kuchnię.
# """
# prompt = "Przetłumacz zdanie poniżej z Angielskiego na Polski:\n" + "\n".join([f"Zdanie po Angielsku: {eng} Zdanie po Polsku: {pol}" for eng, pol in samples])
prompt = "\n".join([f"Zdanie po Angielsku: {eng} Zdanie po Polsku: {pol}" for eng, pol in samples])

# sentence = "They are playing football on the field."
sentence = "The sun is shining brightly in the sky."

prompt += f"\nZdanie po Angielsku: {sentence} Zdanie po Polsku: "

g = generator(prompt, 
    pad_token_id=generator.tokenizer.eos_token_id,
    temperature=0.2)
g = g[0]['generated_text']

print(g)
