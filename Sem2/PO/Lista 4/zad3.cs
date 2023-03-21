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
    int _verticies;
    int _edges;
    bool[,] matrix;

    Dictionary<string, int> v_ptr;      // for finding verticies
    Dictionary<int, string> v_name;     // for finding names


    int get_ptr(string a){
        return v_ptr.ContainsKey(a) ? v_ptr[a] : v_ptr.Count;
    }

    public GraphMatrix(int n){
        _verticies = n;
        _edges = 0;
        matrix = new bool[n, n];
        v_ptr = new Dictionary<string, int>();
        v_name = new Dictionary<int, string>();
    }

    public int verticies{
        get {return _verticies;}
    }
    public int edges{
        get {return _edges;}
    }

    public void reset(){
        _edges = 0;
        matrix = new bool[_verticies, _verticies];
        v_ptr = new Dictionary<string, int>();
        v_name = new Dictionary<int, string>();
    }

    public void append(string from, string to){
        int ptr_from = get_ptr(from);
        if(ptr_from == v_ptr.Count){
            v_ptr.Add(from, ptr_from);
            v_name.Add(ptr_from, from);
        }

        int ptr_to = get_ptr(to);
        if(ptr_to == v_ptr.Count){
            v_ptr.Add(to, ptr_to);
            v_name.Add(ptr_to, to);
        }

        matrix[ptr_from, ptr_to] = matrix[ptr_to, ptr_from] = true;
        _edges++;
    }

    public void remove(string from, string to){
        int ptr_from = get_ptr(from);
        int ptr_to = get_ptr(to);

        if(ptr_from == v_ptr.Count || ptr_to == v_ptr.Count) return;        // vortex doesnt exist!

        matrix[ptr_from, ptr_to] = matrix[ptr_to, ptr_from] = false;
        _edges--;
    }

    public List<string> neighbours(string name_v){
        int ptr_v = get_ptr(name_v);
        if(ptr_v == v_ptr.Count) return new List<string>();                 // vortex doesnt exist!

        List<string> res = new List<string>();
        for(int i = 0; i < _verticies; i++)
            if(matrix[ptr_v, i]) 
                res.Add(v_name[i]);

        return res;
    }
    
    public string print(){
        string res = "";
        foreach(KeyValuePair<int, string> name in v_name){
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
    private IGraph clear_out(IGraph g){
        g.reset();
    }

    public static IGraph create_random(IGraph g, int amt_v, int amt_e){
        Randomness rnd = new Randomness();
        List<string> verticies = new List<string>();
        
        if(g is GraphList) g = new GraphList(amt_v);
        else if(g is GraphMatrix) g = new GraphMatrix(amt_v);

        for(int i = 0; i < amt_v; i++)
            verticies.Add(rnd.very_rand_string());
        for(int i = 0; i < amt_e; i++){
            g.append(verticies[rnd.rand_int(0, amt_v)], verticies[rnd.rand_int(0, amt_v)]);     // TODO: probably should check if edge already exists
        }

        return g;
    }

    public static List<string> shortest_path(IGraph g, string from, string to){
        Queue<string> q = new Queue<string>();
        HashSet<string> vis = new HashSet<string>();
        Dictionary<string, string> father = new Dictionary<string, string>();

        q.Enqueue(from);

        while(q.Count != 0){
            string now = q.Dequeue();
            vis.Add(now);
            if(now == to) break;
            
            foreach(string v in g.neighbours(now)){
                if(!vis.Contains(v)){
                    father.Add(v, now);
                    q.Enqueue(v);
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
        // we could also keeep general type Vertex instead of string, for type of vortex labeling
        GraphList graph_l = new GraphList(10);
        GraphMatrix graph_m = new GraphMatrix(10);

        Console.WriteLine("--------Manual graph operations-------\n");
        graph_l.append("a","b");
        graph_l.append("b","a");
        graph_l.append("c","a");
        graph_l.append("a","w");
        Console.WriteLine(graph_l.edges);
        Console.WriteLine(graph_l.print());
        
        Console.WriteLine("---------------\n");
        graph_m.append("a","b");
        graph_m.append("b","a");
        graph_m.append("c","a");
        graph_m.append("a","w");
        Console.WriteLine(graph_m.edges);
        Console.WriteLine(graph_m.print());

        Randomness rnd = new Randomness();
        Console.WriteLine(rnd.very_rand_string(64));

        Console.WriteLine("--------Shortest paths-------\n");
        create_random(graph_l, 5, 7);
        Console.WriteLine(graph_l.edges);
        Console.WriteLine(graph_l.print());


        create_random(graph_m, 10, 5);
        Console.WriteLine(graph_m.edges);
        Console.WriteLine(graph_m.print());

    }
}