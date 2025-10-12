from transformers import pipeline, set_seed
import re

MAX_HISTORY = 5
MAX_TOKENS = 50
NUM_RESPONSES = 3

generator = pipeline('text-generation', model='flax-community/papuGaPT2', device=0)

chat_history = []   # (user question, chatbot answer)

print ('Model loaded')
# user_prefix = "(pierwsza osoba mówi) - "
# bot_prefix = "(druga osoba mówi) - "

# dialog_prompt = 'Oto kawałek dialogu między dwoma osobami. Osoby zawsze mówią na przemian:\n'
# dialog_prompt = 'Oto kawałek dialogu między dwoma osobami. Osoby zawsze mówią na przemian. Druga osoba jest wszechwiedząca i odpowiada na pytania osoby pierwszej zgodnie z prawdą. Oto kawałek dialogu między tymi dwoma osobami:\n'
# dialog_prompt = '''
# Oto kawałek dialogu między dwoma osobami: Patryk (pierwsza osoba) i Chatbot (druga osoba). 
# Rozmowa odbywa się w przyjaznej atmosferze, a Chatbot zawsze odpowiada zgodnie z prawdą, jest uprzejmy, pomocny i stara się wyjaśniać zagadnienia w sposób zrozumiały. 
# Pierwsza osoba zadaje pytania dotyczące różnych tematów, a Chatbot udziela szczegółowych, rzeczowych odpowiedzi. 
# Oto fragment tej rozmowy:
# '''

dialog_prompt = '''
    Poniżej znajduje się rozmowa między Użytkownikiem i Chatbotem.\n
    Użytkownik zadaje pytania, a Chatbot odpowiada krótko, sensownie i na temat.\n
    Rozmowa:\n
    "Użytkownik: Cześć!\n"
    "Chatbot: Cześć Użytkowniku! Jak się masz?\n"
'''
user_prefix = "Użytkownik: "
bot_prefix = "Chatbot: "


# dialog_prompt = '''
#     Poniżej znajduje się rozmowa między Użytkownikiem i Chatbotem.\n
#     Użytkownik zadaje pytania, a Chatbot odpowiada krótko, sensownie i na temat.\n
#     Rozmowa:\n
# '''
# chat_history = [
#     ("Halo. Jest tam kto?", "Tak, jestem tutaj. Jestem Chatbotem i jestem do twoich usług. Jak mogę pomóc?"), 
#     ("Cześć Chatbocie!", "Cześć Użytkowniku!"),
#     ("To może zacznijmy od początku, ok?", "Oczywiście.")
# ]
# user_prefix = "Użytkownik: "
# bot_prefix = "Chatbot: "

def score_response(resp):
    words = len(resp.split())
    length_score = -abs(words - 8)
    end_bonus = 5 if re.search(r"[.!?]\s*$", resp) else 0 # preferuje zdania zakończone kropką lub wykrzyknikiem
    # mniej interpunkcji to lepsza ocena
    less_punctuation_bonus = -len(re.findall(r'[,.!?;:]', resp))
    return length_score + end_bonus + less_punctuation_bonus


while True:
    user_query = input().strip().lstrip(" -\t")
    if not user_query:
        continue

    prompt = dialog_prompt
    for user, bot in chat_history[-MAX_HISTORY:]:
        user = user.lstrip(" -\t")
        bot = bot.lstrip(" -\t")
        prompt += f"{user_prefix} - {user}\n{bot_prefix} - {bot}\n\n"

    prompt += f"{user_prefix} - {user_query}\n{bot_prefix} - "

    # print("----")
    # print(prompt)
    # print("----")

    generations = generator(
        prompt,
        pad_token_id=generator.tokenizer.eos_token_id,
        max_new_tokens=MAX_TOKENS,
        num_return_sequences=NUM_RESPONSES,
        do_sample=True,
        temperature=0.8,
        top_p=0.95,
        top_k=60
    )

    candidates = []
    for g in generations:
        chatbot_answer = g["generated_text"][len(prompt):].strip()
        # chatbot_answer = g["generated_text"][len(prompt):].strip().split("\n")[0].lstrip(" -\t")
        line = chatbot_answer.split("\n")[0].lstrip(" -\t")
        if len(line.split()) > 1:
            candidates.append(line)

    if not candidates:
        chatbot_answer = "yyyyyy"
    else:
        chatbot_answer = max(candidates, key=score_response)

    chat_history.append((user_query, chatbot_answer))

    print (chatbot_answer)
    print ()
    
    

    
