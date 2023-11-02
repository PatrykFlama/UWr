const fs = require('fs');

const methods = ['GET', 'POST', 'PUT', 'DELETE', 'PATCH', 'HEAD'];
const resources = ['Resource1', 'Resource2', 'Resource3', 'Resource4', 'Resource5'];

for (let i = 0; i < 1000; i++) {
    const date = new Date();
    date.setHours(Math.floor(Math.random() * 24));
    date.setMinutes(Math.floor(Math.random() * 60));
    date.setSeconds(Math.floor(Math.random() * 60));
    const time = date.toLocaleTimeString();

    const ip = `192.168.0.${Math.floor(Math.random() * 256)}`;
    const method = methods[Math.floor(Math.random() * methods.length)];
    const path = `/TheApplication/${resources[Math.floor(Math.random() * resources.length)]}.axd`;
    const status = Math.floor(Math.random() * 1000);
    const log = `${time} ${ip} ${method} ${path} ${status}\n`;

    fs.appendFile('zad6.log', log, (err) => {
        if (err) throw err;
    });
}
