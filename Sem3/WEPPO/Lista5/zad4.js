const fs = require('fs');

// Using callback function
fs.readFile('path/to/file.txt', 'utf8', (err, data) => {
    if (err) {
        console.error(err);
        return;
    }
    console.log(data);
});

// Using promise
fs.promises.readFile('path/to/file.txt', 'utf8')
    .then(data => console.log(data))
    .catch(err => console.error(err));
