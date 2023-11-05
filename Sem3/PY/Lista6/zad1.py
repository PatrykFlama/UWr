import requests
from bs4 import BeautifulSoup


def crawl(start_page, depth, action, visited = set(), visit_foreign_hosts = False):
    if start_page in visited: return
    visited.add(start_page)

    try:
        response = requests.get(start_page).text
    except:
        return

    result = action(response)
    yield (start_page, result)

    if depth <= 0: 
        return

    soup = BeautifulSoup(response, 'html.parser')
    for link in soup.find_all('a'):
        url = link.get('href')
        if url is None: continue
        
        is_new_url = url.startswith('http') or url.startswith('https')
        if not visit_foreign_hosts and is_new_url: continue
        if not(is_new_url):
            url = start_page + url

        yield from crawl(url, depth - 1, action, visited, visit_foreign_hosts)


vis = {}
for url, result in crawl("http://www.ii.uni.wroc.pl", 1, lambda text : 'Python' in text, visit_foreign_hosts = False):
    print(f"{url}: {result}")
    if url not in vis:
        vis[url] = 1
    else: vis[url] += 1
    