const fs = require('fs');
const util = require('util');

// Klasyczny interfejs programowania asynchronicznego
fs.readFile('text.txt', 'utf8', (err, data) => {
    if (err) throw err;
    console.log(data);
});

// Przy pomocy "ręcznie" napisanej funkcji zwracającej Promise
function readFilePromise(path) {
    return new Promise((resolve, reject) => {
        fs.readFile(path, 'utf8', (err, data) => {
            if (err) reject(err);
            resolve(data);
        });
    });
}

readFilePromise('text.txt')
    .then(data => console.log(data))
    .catch(err => console.error(err));

// Przy pomocy util.promisify
const readFilePromisified = util.promisify(fs.readFile);

readFilePromisified('text.txt', 'utf8')
    .then(data => console.log(data))
    .catch(err => console.error(err));

// Przy pomocy fs.promises
const { promises: fsPromises } = require('fs');

fsPromises.readFile('text.txt', 'utf8')
    .then(data => console.log(data))
    .catch(err => console.error(err));

// Obsługa funkcji zwracającej Promise "po staremu" - wywołanie z kontynuacją (Promise::then)
function doSomethingElse(data) {
    return new Promise((resolve, reject) => {
        setTimeout(() => {
            resolve(data);
        }, 1000);
    });
}

readFilePromise('text.txt')
    .then(data => {
        console.log(data);
        return doSomethingElse(data);
    })
    .then(result => console.log(result))
    .catch(err => console.error(err));

// Obsługa funkcji zwracającej Promise "po nowemu" - wywołanie przez async/await
async function readFileAndDoSomethingElse() {
    try {
        const data = readFilePromise('text.txt');
        console.log(await data);
        const result = doSomethingElse(data);
        console.log(await result);
    } catch (err) {
        console.error(err);
    }
}

readFileAndDoSomethingElse();
