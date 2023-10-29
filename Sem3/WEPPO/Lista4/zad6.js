function TreeDFS(val, left, right) {
    this.left = left;
    this.right = right;
    this.val = val;
}

TreeDFS.prototype[Symbol.iterator] = function* () {
    yield this.val;
    if (this.left) yield* this.left;
    if (this.right) yield* this.right;
}

// var root = new TreeDFS(1, new TreeDFS(2, new TreeDFS(3)), new TreeDFS(4));

// for (var e of root) {
//     console.log(e);
// }
// 1 2 3 4


class TreeBFS {
    constructor(val, left, right) {
        this.left = left;
        this.right = right;
        this.val = val;
    }
    [Symbol.iterator] = function* () {
        var queue = [this];

        while (queue.length > 0) {
            var node = queue.shift();         // queue - BFS
            // var node = queue.pop();        // stack - DFS
            yield node.val;

            if (node.left) queue.push(node.left);
            if (node.right) queue.push(node.right);
            // ewentualnie za pomocą splice: array.splice(start, deleteCount, item1, item2, ...) => 
            // => queue.splice(queue.length, 1, node.left, node.right);
        }
    }
}

var root = new TreeBFS(1,
    new TreeBFS(2,
        new TreeBFS(3,
            new TreeBFS(5),
            new TreeBFS(6)),
        new TreeBFS(4,
            new TreeBFS(7),
            new TreeBFS(8))),
    new TreeBFS(9,
        new TreeBFS(10,
            new TreeBFS(11),
            new TreeBFS(12)),
        new TreeBFS(13,
            new TreeBFS(14),
            new TreeBFS(15)))
);
/*
       1
      / \
     2   9
    / \  / \
   3  4  10 13
  /\ /\  / \  | \
 5 6 7 8 11 12 14 15 
*/

for (var e of root) {
    console.log(e);
}