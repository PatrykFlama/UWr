const moduleA = require('./zad1a.js');

console.log('Module B');
moduleA.runModuleA();

function runModuleB() {
    console.log('Running module B');
}

module.exports = {
    runModuleB
};
