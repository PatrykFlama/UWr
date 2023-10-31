const random_number = Math.floor(Math.random() * 100);
const readline = require('readline');

const rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout
});

function action(answer){
    var number = parseInt(answer);
    if(number == random_number){
        console.log("You win!");
        rl.close();
        guessed = true;
    } else if(number > random_number){
        console.log("Too big");
        loop();
    }
    else{
        console.log("Too small");
        loop();
    }
}

function loop(){
    rl.question('Enter number: ', action);
}


loop();
