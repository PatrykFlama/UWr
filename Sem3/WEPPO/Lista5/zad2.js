const readline = require('readline');

const rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout
});

rl.question('Whats your name? ', (answer) => {
  console.log('Hello ', answer);
  rl.close();
});



// var stdin = process.openStdin();

// stdin.addListener("data", function (d) {
//     console.log("you entered: [" + d.toString().trim() + "]");
// });

// var readline = require('readline');
// var rl = readline.createInterface(process.stdin, process.stdout);
// rl.setPrompt('guess> ');
// rl.prompt();
// rl.on('line', function (line) {
//     if (line === "right") rl.close();
//     rl.prompt();
// }).on('close', function () {
//     process.exit(0);
// });
