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


shrek_dialogs = '''
Osioł: Cześć mała! \n
Fiona: On gada! \n
Shrek: No! Ale to tylko jedna z jego wad! \n
\n
Osioł: No to gdzie jest ta ognista potwora, co?! \n
Shrek: W zamku... czeka na swojego rycerza. \n
Osioł: Ja smoka miałem na myśli, Shrek. \n
\n
Osioł: Ty wiesz, że ja też? Kurcze, tyle nas łączy. Ale najbardziej jak się ktoś przyczepia, dajesz do zrozumienia, a on nic i zapada taka niezręczna cisza co nie... mogę tu zamieszkać? \n
Shrek: Coo? \n
Osioł: Mogę tu zamieszkać? Proszę... \n
Shrek: Ależ jasne! \n
Osioł: Serio? \n
Shrek: Nie! \n
Osioł: Błagam! Nie chcę wracać! Nie wiesz, jak to jest robić za dziwoląga. No... Może i wiesz. Tym bardziej musimy się trzymać razem! Nie możesz mnie wyrzucić! Proszę, proszę! \n
\n
Shrek: Ogry są... jak cebula! \n
Osioł: Śmierdzą?! \n
Shrek: Tak... nie! \n
Osioł: Bo się od nich płacze? \n
Shrek: Nie! \n
Osioł: Bo jak się je zostawi na słońcu to robią się brązowe i rosną im włoski? \n
Shrek: NIE! Warstwy! Cebula ma warstwy. Dociera? Ogry mają warstwy... \n
Osioł: No więc ogry mają warstwy, wiesz, nie wszyscy lubią cebulę... \n
\n
Osioł: Widziałeś tę ruderę? Kto by chciał mieszkać w czymś takim? \n
Shrek: Wiesz, tak konkretnie to ja. \n
Osioł: Tu jest pięknie! Cudownie wręcz. Ty wiesz, że masz talent? Taki efekt przy tak skromnym budżecie... Podoba mi się ten głaz. To naprawdę ładny głaz. \n
\n
Osioł: Na prawdę mi się podobało \n
Shrek: O, to świetnie. Serio! \n
Osioł: Wolność rządzi! \n
Shrek: Fajnie, to może idź się nią cieszyć w gronie przyjaciół? \n
Osioł: Ale... Ja nie mam przyjaciół. A do zamku już na pewno nie wrócę. Ej! Czekaj, mam bomba pomysł, pójdę z Tobą! Z moim mózgiem i Twoim gabarytem cała droga nasza! Nikt nam nie podskoczy. (Shrek krzyczy) O, rany! No i właśnie o to chodzi. I daruj, że to mówię, ale jak to nie wystarczy, położysz ich samym oddechem. Na prawdę, stary, zainwestuj tic tac-i, bo ci jedzie! Stary, prawie mi przeżarło nos. Tak jak wtedy, jak (Shrek zasłania mu pyszczek) A na deser brukselki, no, kamikadze, boski wiatr nie ma przebacz. \n
Shrek: Ale... czemu za mną leziesz! \n
Osioł: Czemu pytasz? Bo... (śpiewa) Jestem taki sam, jak palec albo coś tam. Problemu wreszcie stos, samotność, ciężki loos. Przyjaciele do dębu... \n
Shrek: Dość, przestań! I ty się dziwisz, że nie masz przyjaciół? \n
Osioł: Tylko przyjaciela stać na taką szczerość. \n
Shrek: Słuchaj, skoncentruj się. Przyjrzyj mi się dobrze. I co widzisz? \n
Osioł: Że... lubisz jeść? \n
Shrek: Nie! Że jestem ogrem! No, wiesz. Piąty głaz za widły to ogr! Nie przeszkadza Ci to? \n
Osioł: (macha głową w boki) Ani trochę. \n
Shrek: Serio? \n
Osioł: Serio, serio. \n
Shrek: O... \n
Osioł: Stary, lubię Cię! Jak Cię zwam? \n
Shrek: Shrek. \n
'''

dialog_prompt = '''
    Przykłady dialogów między Shrekiem a Osłem:\n
'''+ shrek_dialogs[:300] + '''
    Poniżej znajduje się rozmowa między Shrekiem a Osłem.\n
    Osioł zadaje pytania, a Shrek odpowiada krótko, sensownie i na temat.\n
    Rozmowa:\n
'''

dialog_prompt = '''
    Poniżej znajduje się rozmowa między Shrekiem a Osłem.\n
    Osioł zadaje pytania, a Shrek odpowiada krótko, sensownie i na temat.\n
    Rozmowa:\n
''' + shrek_dialogs[:300] + '\n'

user_prefix = "Osioł: "
bot_prefix = "Shrek: "


dialog_prompt = '''
    Poniżej znajduje się rozmowa między Użytkownikiem i Chatbotem.\n
    Użytkownik zadaje pytania, a Chatbot odpowiada krótko, sensownie i na temat.\n
    Rozmowa:\n
    "Użytkownik: Cześć!\n"
    "Chatbot: Cześć Użytkowniku! Jak się masz?\n"
'''
user_prefix = "Użytkownik: "
bot_prefix = "Chatbot: "


def score_response(resp):
    words = len(resp.split())
    length_score = -abs(words - 8)
    end_bonus = 5 if re.search(r"[.!?]\s*$", resp) else 0 # kropka czy wykrzyknik
    # mniej interpunkcji to lepsza ocena
    less_punctuation_bonus = -len(re.findall(r'[,.!?;:]', resp)) * 2
    # characters_diversity_bonus = len(set(resp)) / (len(resp) + 1e-6) * 5
    # words_diversity_bonus = len(set(resp.split())) / (len(resp.split()) + 1e-6) * 20
    return length_score + end_bonus + less_punctuation_bonus


while True:
    print (user_prefix, end=' ')
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
        top_k=60,
        # repetition_penalty=1.1,
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

    print (bot_prefix, chatbot_answer)
    print ()
    
    

    
