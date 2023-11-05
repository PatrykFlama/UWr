function Tree(val, left, right) {
    this.left = left;
    this.right = right;
    this.val = val;
}

Tree.prototype[Symbol.iterator] = function*() {
    var queue = [this];     // BFS

    while (queue.length > 0) {
        var node = queue.shift();         // queue - BFS
        yield node.val;

        if (node.left) queue.push(node.left);
        if (node.right) queue.push(node.right);
    }
}

module.exports = Tree;
