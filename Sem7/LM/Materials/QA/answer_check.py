import editdistance
import sys

split = 'test'

def scaled_editdist(ans, cor):
    ans = ans.lower()
    cor = cor.lower()
    
    return editdistance.eval(ans, cor) / len(cor)
    
def single_match(a, c):
    a = a.lower()
    if c in ['tak','nie']:
        return a == c
    if c.isdecimal():
        return a == c
    
    if a.startswith('w '):
        a = a[2:]
    if c.startswith('w '):
        c = c[2:]
    
    if len(c) <= 3:
        return a == c
        
    return scaled_editdist(a, c) < 0.33
        
def match(ans, cor):
    return any(single_match(ans, c) for c in cor)
        
found_answers = []
correct_answers = []

answers_file_name = split + '_answers.txt'

for x in open(answers_file_name):
    x = x.strip()
    correct_answers.append(x.lower().split('\t'))
    
for x in open('found_answers.txt'):    
    x = x.strip()
    found_answers.append(x.lower())

assert  len(correct_answers) == len(found_anwers)
    
N = len(correct_answers)


score = 0.0

for ans, cor in zip(found_answers, correct_answers):    
    if match(ans, cor):
        score += 1
        
print ('TOTAL SCORE:', score/ N)        
