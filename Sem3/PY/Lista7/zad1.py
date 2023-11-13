import requests
from bs4 import BeautifulSoup
import threading
import sys


class ReturnValueThread(threading.Thread):  # https://alexandra-zaharia.github.io/posts/how-to-return-a-result-from-a-python-thread/
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.result = None

    def run(self):
        if self._target is None:
            return  # could alternatively raise an exception, depends on the use case
        try:
            self.result = self._target(*self._args, **self._kwargs)
        except Exception as exc:
            print(f'{type(exc).__name__}: {exc}', file=sys.stderr)  # properly handle the exception

    def join(self, *args, **kwargs):
        super().join(*args, **kwargs)
        return self.result


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

    threads = []
    soup = BeautifulSoup(response, 'html.parser')
    for link in soup.find_all('a'):
        url = link.get('href')
        if url is None: continue
        
        is_new_url = url.startswith('http') or url.startswith('https')
        if not visit_foreign_hosts and is_new_url: continue
        if not(is_new_url):
            url = start_page + url

        # crawl(url, depth - 1, action, visited, visit_foreign_hosts)
        threads.append(ReturnValueThread(target = crawl, \
                                       args = (url, depth - 1, action, visited, visit_foreign_hosts)))
        threads[-1].start()
    
    for thread in threads:
        thread.join()
        yield from thread.result


vis = {}
for url, result in crawl("http://www.ii.uni.wroc.pl", 1, lambda text : 'Python' in text, visit_foreign_hosts = False):
    print(f"{url}: {result}")
    if url not in vis:
        vis[url] = 1
    else: vis[url] += 1
    