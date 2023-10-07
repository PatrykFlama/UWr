import urllib.request
import string


def calc_stats(url, dict={}):
    str = urllib.request.urlopen(url).read().decode('utf-8')        # get text
    str = (c for c in str if c.isalnum())                           # remove non-alphanumeric characters
    str = filter(lambda x: x in set(string.printable), str)         # remove non-ascii characters
    str = (''.join(str)).lower()                                    # convert to string and lowercase

    for c in str:
        if c in dict: dict[c] += 1
        else: dict[c] = 1

    for k, v in dict.items():
        dict[k] = v / len(str)

    return dict

def guess_language(languages, url):
    stats = calc_stats(url)
    min = 1
    lang = ''
    for name, dict, _ in languages:
        if len(dict) == 0: continue

        diff = 0
        for k, v in dict.items():
            if k in stats: diff += abs(v - stats[k])
            else: diff += v
        diff /= len(dict)

        if diff < min:
            min = diff
            lang = name

    return lang

english = {}
english_sources = [
    'https://en.wikisource.org/wiki/A_Venetian_June', 
    'https://en.wikisource.org/wiki/Freedom_of_Information_Act_2000', 
    'https://en.wikisource.org/wiki/The_Cession_of_Louisana_to_the_United_States', 
    'https://en.wikisource.org/wiki/East_European_Quarterly/Volume_15/Number_1/Franti%C5%A1ek_Palack%C3%BD_and_the_Development_of_Modern_Czech_Nationalism', 
    'https://en.wikisource.org/wiki/The_Diary_of_a_Pilgrimage/The_New_Utopia', 
    'https://en.wikisource.org/wiki/The_Mortover_Grange_Affair',
    'https://en.wikisource.org/wiki/The_British_Volunteers'
]

polish = {}
polish_sources = [
    'https://pl.wikisource.org/wiki/Joan._VIII_1-12/I',
    'https://pl.wikisource.org/wiki/Babia_g%C3%B3ra_i_j%C3%A9j_okolice',
    'https://pl.wikisource.org/wiki/Obraz_I',
    'https://pl.wikisource.org/wiki/Ludzie_zb%C4%99dni_w_s%C5%82u%C5%BCbie_przemocy'
]

spanish = {}
spanish_sources = [
    'https://es.wikisource.org/wiki/El_olvido_(Payr%C3%B3)',
    'https://es.wikisource.org/wiki/Las_literatas',
    'https://es.wikisource.org/wiki/Lieders',
    'https://es.wikisource.org/wiki/Flavio',
    'https://es.wikisource.org/wiki/Abel_Mart%C3%ADn',
    'https://es.wikisource.org/wiki/La_Espada_del_mago',
    'https://es.wikisource.org/wiki/Adem%C3%A1s_del_frac/Cap%C3%ADtulo_I',
    'https://es.wikisource.org/wiki/A_la_sombra_de_un_chaparro'
]

german = {}
german_sources = [
    'https://de.wikisource.org/wiki/Lisa%E2%80%99s_Tagebuch', 
    'https://de.wikisource.org/wiki/Chronicon_der_mecklenburgischen_Regenten/Das_Dritte_Buch', 
    'https://de.wikisource.org/wiki/Niederbayern', 
    'https://de.wikisource.org/wiki/Verzeichni%C3%9F_der_vom_14._Juli_1850_an_in_der_K._S._Akademie_der_K%C3%BCnste_zu_Dresden_%C3%B6ffentlich_ausgestellten_Werke_der_bildenden_Kunst', 
    'https://de.wikisource.org/wiki/Friedrich_Friedrich', 
    'https://de.wikisource.org/wiki/Bob_Zellina', 
    'https://de.wikisource.org/wiki/Der_heimliche_Gast',
    'https://de.wikisource.org/wiki/Geschichte_der_Herrschaft_Eisenburg'
]

languages = [
    ('english', english, english_sources),
    ('polish', polish, polish_sources),
    ('spanish', spanish, spanish_sources),
    ('german', german, german_sources)
]

for _, lang, sources in languages:
    for source in sources:
        lang = calc_stats(source, lang)

# ---------------------------- TEST ---------------------------------
guess_list = [
    ('https://en.wikisource.org/wiki/Tixall_Poetry', 'english'),
    ('https://pl.wikisource.org/wiki/Joan._VIII_1-12/II', 'polish'),
    ('https://pl.wikisource.org/wiki/Joan._VIII_1-12/III', 'polish'),
    ('https://pl.wikisource.org/wiki/Joan._VIII_1-12/V', 'polish'),
    ('https://es.wikisource.org/wiki/El_Domingo_de_Ramos', 'spanish'),
    ('https://de.wikisource.org/wiki/Philotas_(Gleim_1767)', 'german'),
    ('https://de.wikisource.org/wiki/Sammlung_alt-_und_mitteldeutscher_W%C3%B6rter_aus_lateinischen_Urkunden', 'german')
]

for url, lang in guess_list:
    print(f'{lang} -> {guess_language(languages, url)} (url: {url})')

