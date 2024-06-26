/*
Patryk Flama - lista 4 zadanie 3
kompilacja: mcs zad2.cs
*/
using System;
using System.Collections.Generic;

interface IGraph{
    int verticies {get;}
    int edges {get;}
    void append(string from, string to);
    void remove(string from, string to);
    void add_node(string node);
    List<string> neighbours(string node_name);
    void reset();
};

class GraphList : IGraph{
    int _verticies;
    int _edges;

    Dictionary<string, List<string>> graph;

    
    public GraphList(int n){
        _verticies = n;
        _edges = 0;
        graph = new Dictionary<string, List<string>>();
    }

    public int verticies{
        get {return _verticies;}
    }
    public int edges{
        get {return _edges;}
    }

    public void reset(){
        _edges = 0;
        graph = new Dictionary<string, List<string>>();
    }

    public void append(string from, string to){
        if(!graph.ContainsKey(from))
            graph.Add(from, new List<string>());
        if(!graph.ContainsKey(to)) 
            graph.Add(to, new List<string>());

        if(!graph[from].Contains(to)) graph[from].Add(to);
        if(!graph[to].Contains(from)) graph[to].Add(from);
        _edges++;
    }

    public void remove(string from, string to){
        if(!graph.ContainsKey(from) || !graph.ContainsKey(to)) return;      // this vortex doesn't exist!

        graph[from].Remove(to);
        graph[to].Remove(from);
        _edges--;
    }

    public void add_node(string node){
        if(!graph.ContainsKey(node))
            graph.Add(node, new List<string>());
    }

    public List<string> neighbours(string node_name){
        if(!graph.ContainsKey(node_name)) return new List<string>();        // this vortex doesn't exist!

        return graph[node_name];
    }

    public string print(){
        string s = "";
        foreach(KeyValuePair<string, List<string>> node in graph){
            s += node.Key + " -> ";
            s += string.Join(", ", node.Value);
            s += "\n";
        }
        return s;
    }
};

class GraphMatrix : IGraph{
    int _verticies;     // max v
    int _edges;         // actual e
    bool[,] matrix;

    Dictionary<string, int> name_to_ptr;      // for finding verticies
    Dictionary<int, string> ptr_to_name;     // for finding names


    public GraphMatrix(int n){
        _verticies = n;
        _edges = 0;
        matrix = new bool[n, n];
        name_to_ptr = new Dictionary<string, int>();
        ptr_to_name = new Dictionary<int, string>();
    }

    public int verticies{
        get {return _verticies;}
    }
    public int edges{
        get {return _edges;}
    }

    int get_ptr(string a){
        return name_to_ptr.ContainsKey(a) ? name_to_ptr[a] : name_to_ptr.Count;
    }

    public void reset(){
        _edges = 0;
        matrix = new bool[_verticies, _verticies];
        name_to_ptr = new Dictionary<string, int>();
        ptr_to_name = new Dictionary<int, string>();
    }

    public void append(string from, string to){
        int ptr_from = get_ptr(from);
        if(ptr_from == name_to_ptr.Count){
            name_to_ptr.Add(from, ptr_from);
            ptr_to_name.Add(ptr_from, from);
        }

        int ptr_to = get_ptr(to);
        if(ptr_to == name_to_ptr.Count){
            name_to_ptr.Add(to, ptr_to);
            ptr_to_name.Add(ptr_to, to);
        }

        matrix[ptr_from, ptr_to] = matrix[ptr_to, ptr_from] = true;
        _edges++;
    }

    public void remove(string from, string to){
        int ptr_from = get_ptr(from);
        int ptr_to = get_ptr(to);

        if(ptr_from == name_to_ptr.Count || ptr_to == name_to_ptr.Count) return;        // vortex doesnt exist!

        matrix[ptr_from, ptr_to] = matrix[ptr_to, ptr_from] = false;
        _edges--;
    }

    public void add_node(string node){
        int ptr = get_ptr(node);
        if(ptr == name_to_ptr.Count){
            name_to_ptr.Add(node, ptr);
            ptr_to_name.Add(ptr, node);
        }
    }

    public List<string> neighbours(string node_name){
        int ptr_v = get_ptr(node_name);
        if(ptr_v == name_to_ptr.Count) return new List<string>();                 // vortex doesnt exist!

        List<string> res = new List<string>();
        for(int i = 0; i < _verticies; i++)
            if(matrix[ptr_v, i]) 
                res.Add(ptr_to_name[i]);

        return res;
    }
    
    public string print(){
        string res = "";
        foreach(KeyValuePair<int, string> name in ptr_to_name){
            res += name.Value + " -> ";
            res += string.Join(", ", neighbours(name.Value));
            res += "\n";
        }
        return res;
    }
};

class Randomness : Random{
    public int rand_int(int from, int to){     //? [from, to)
        return Next(from, to);
    }

    public string rand_string(int len){
        string res = "";
        while(len != 0) {
            res += (char)(rand_int('a', 'z'+1)); 
            --len;
        }
        return res;
    }

    public string very_rand_string(){
        return rand_string(rand_int(0, 32));
    }
};

class GraphOperations{
    private void clear_out(IGraph g){
        g.reset();
    }

    public IGraph create_random(IGraph g, int amt_v, int amt_e){
        Randomness rnd = new Randomness();
        List<string> verticies = new List<string>();
        
        if(g is GraphList) g = new GraphList(amt_v);
        else if(g is GraphMatrix) g = new GraphMatrix(amt_v);

        for(int i = 0; i < amt_v; i++){
            verticies.Add(rnd.very_rand_string());
            g.add_node(verticies[i]);
        }
        
        for(int i = 0; i < amt_e; i++){
            g.append(verticies[rnd.rand_int(0, amt_v)], verticies[rnd.rand_int(0, amt_v)]);
        }

        return g;
    }

    public List<string> shortest_path(IGraph g, string from, string to){
        // Queue<string> q = new Queue<string>(); //! problem with including queue, probably becouse its been moved to mscorlib
        List<string> q = new List<string>();
        HashSet<string> vis = new HashSet<string>();
        Dictionary<string, string> father = new Dictionary<string, string>();

        q.Add(from);

        while(q.Count != 0){
            // string now = q.Dequeue();
            string now = q[0];
            q.RemoveAt(0);

            vis.Add(now);
            if(now == to) break;
            
            foreach(string v in g.neighbours(now)){
                if(!vis.Contains(v)){
                    try{father.Add(v, now);}    // we dont want to add same edge twice
                    catch(Exception e) {}
                    // q.Enqueue(v);
                    q.Add(v);
                }
            }
        }

        List<string> res = new List<string>();
        for(string i = to; father.ContainsKey(i); i = father[i]) 
            res.Add(i);

        if(res.Count > 0 || from == to) res.Add(from);

        res.Reverse();
        return res;
    }
}


class Program{
    public static void Main(){
        // we could also keeep general type 'Vertex' instead of string, for type of vortex labeling
        GraphList graph_l = new GraphList(5);
        GraphMatrix graph_m = new GraphMatrix(5);

        Console.WriteLine("--------Manual graph operations-------\n");
        graph_l.append("a","b");
        graph_l.append("b","a");
        graph_l.append("c","a");
        graph_l.append("a","w");
        Console.WriteLine(graph_l.edges);
        Console.WriteLine(graph_l.print());
        
        graph_m.append("a","b");
        graph_m.append("b","a");
        graph_m.append("c","a");
        graph_m.append("a","w");
        Console.WriteLine(graph_m.edges);
        Console.WriteLine(graph_m.print());

        Randomness rnd = new Randomness();
        Console.Write("Random string: ");
        Console.WriteLine(rnd.very_rand_string());

        Console.WriteLine("--------Random graphs-------\n");
        GraphOperations GO = new GraphOperations();
        graph_l = (GraphList)(GO.create_random(graph_l, 5, 7));
        Console.WriteLine(graph_l.edges);
        Console.WriteLine(graph_l.print());

        graph_m = (GraphMatrix)(GO.create_random(graph_m, 10, 5));
        Console.WriteLine(graph_m.edges);
        Console.WriteLine(graph_m.print());

        Console.WriteLine("------Shortest Path------");
        graph_l = new GraphList(10);
        graph_m = new GraphMatrix(10);

        graph_l.append("a","b");
        graph_l.append("b","c");
        graph_l.append("c","d");
        graph_l.append("b","d");
        Console.Write("Shortest path: ");
        Console.WriteLine(string.Join(", ", GO.shortest_path(graph_l, "a", "d")));
        
        graph_m.append("a","b");
        graph_m.append("b","c");
        graph_m.append("c","d");
        graph_m.append("b","d");
        Console.Write("Shortest path: ");
        Console.WriteLine(string.Join(", ", GO.shortest_path(graph_m, "a", "d")));
    }
}
