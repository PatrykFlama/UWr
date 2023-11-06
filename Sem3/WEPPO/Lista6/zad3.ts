function map<from, to>(tab: from[], func: (x: from) => to): to[] {
    const result: to[] = [];
    for (let i = 0; i < tab.length; i++) {
        result.push(func(tab[i]));
    }
    return result;
}

function forEach<T>(tab: T[], f: (x: T) => void): void {
    for (let i = 0; i < tab.length; i++) {
        f(tab[i]);
    }
}

function filter<T>(tab: T[], f: (x: T) => boolean): T[] {
    const result: T[] = [];
    for (let i = 0; i < tab.length; i++) {
        if (f(tab[i])) {
            result.push(tab[i]);
        }
    }
    return result;
}


// test
const tab = [1, 2, 3, 4, 5];
const tab2 = map(tab, (x) => x * 2);
console.log(tab2);
forEach(tab2, console.log);
const tab3 = filter(tab2, (x) => x % 4 == 0);
console.log(tab3);
