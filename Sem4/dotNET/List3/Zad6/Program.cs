namespace Zad6
{
    class Program
    {
        static void Main(string[] args)
        {
        }
    }

    class BinTreeNode<T>
    {
        T val;
        BinTreeNode<T> left, right;

        BinTreeNode(T _val)
        {
            val = _val;
            left = right = null;
        }
        BinTreeNode(T _val, BinTreeNode<T> l, BinTreeNode<T> r)
        {
            val = _val;
            left = l;
            right = r;
        }

        public IEnumerable<T> DFS()
        {
            if (left != null) foreach (var x in left.DFS()) yield return x;
            if (right != null) foreach (var x in right.DFS()) yield return x;
            yield return val;
        }

        public IEnumerable<T> BFS()
        {
            Queue<BinTreeNode<T>> q = new Queue<BinTreeNode<T>>();
            q.Enqueue(this);

            while(q.Count() != 0)
            {
                BinTreeNode<T> node = q.Dequeue();
                yield return node.val;

                if (node.left != null) q.Enqueue(node.left);
                if (node.right != null) q.Enqueue(node.right);
            }
        }
    }
}