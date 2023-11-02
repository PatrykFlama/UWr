const fs = require('fs');
const readline = require('readline');

const ipsCnt = {};

const readInterface = readline.createInterface({
    input: fs.createReadStream('zad6.log'),
});

readInterface.on('line', function(line) {
    data = line.split(' ');
    if(ipsCnt[data[1]] == undefined) ipsCnt[data[1]] = 1;
    else ipsCnt[data[1]]++;
});

readInterface.on('close', function() {
    const sortedIps = Object.keys(ipsCnt).sort((a, b) => ipsCnt[b] - ipsCnt[a]);

    for (let i = 0; i < 3 && i < sortedIps.length; i++) {
        console.log(`${sortedIps[i]} ${ipsCnt[sortedIps[i]]}`);
    }
});