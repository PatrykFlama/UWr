const moduleB = require('./zad1b.js');

console.log('Module A');
moduleB.runModuleB();

function runModuleA() {
    console.log('Running module A');
}

module.exports = {
    runModuleA
};
