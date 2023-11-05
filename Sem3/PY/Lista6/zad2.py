import requests
import time

def find_different_range(str1, str2):
    l1 = l2 = 0
    r1, r2 = len(str1)-1, len(str2)-1

    while l1 < len(str1) and l2 < len(str2) and \
          str1[l1] == str2[l2]:
        l1 += 1
        l2 += 1
    
    while r1 >= 0 and r2 >= 0 and \
          str1[r1] == str2[r2]:
        r1 -= 1
        r2 -= 1
    
    return (l1, r1)

#   should update from time to time          should update every hour            maybe updates constantly
urls = ['https://www.bbc.com/news', 'https://www.dota2.com/leaderboards/#europe', 'https://time.is/']
website_content = {}        # dictionary

# initial content
for url in urls:
    response = requests.get(url)
    website_content[url] = response.text

interval = 60
while True:
    time.sleep(interval)
    
    for url in urls:
        response = requests.get(url)
        l, r = find_different_range(website_content[url], response.text)
        if website_content[url] != response.text:
            print(f'---------- Update on {url} ----------')
            print("Old content: ")
            print(website_content[url][l:r])
            print("New content: ")
            print(response.text[l:r])
            print()
            print()
            website_content[url] = response.text
