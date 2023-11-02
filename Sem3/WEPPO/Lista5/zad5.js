const http = require('http');
const https = require('https');

function getContent(url) {
    return new Promise((resolve, reject) => {
        const client = url.startsWith('https') ? https : http;
        client.get(url, (res) => {
            let data = '';
            res.on('data', (chunk) => data += chunk);
            res.on('end', () => resolve(data));
            res.on('error', reject);
        });
    });
}


getContent('https://www.wikipedia.org')
    .then((data) => {
        console.log(data);
    })
    .catch((err) => {
        console.error(err);
    });
