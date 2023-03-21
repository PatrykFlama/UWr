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
/*
class MyRandom : Random{
    public int RandomInt(){
        return Next();
    }
    public string RandomString(){
        int l = (Next()%8)+1;
        string res = "";
        while(l --> 0) res += (char)((Next()%0x3d)+0x40);
        return res;
    }
};

class GraphOperations{
    public static IGraph RandomGraph(IGraph g, int n, int e){
        MyRandom r = new MyRandom();      
        List<string> vertex = new List<string>();
        
        if(g is GraphList) g = new GraphList(n);
        else if(g is GraphMatrix) g = new GraphMatrix(n);

        for(int i = 0; i < n; i++) 
            vertex.Add(r.RandomString());
        while(e --> 0) 
            g.append(vertex[r.RandomInt()%n], vertex[r.RandomInt()%n]);

        return g;
    }
    public static List<string> ShortestPath(IGraph g, string a, string b){ //BFS
        List<string> res = new List<string>();
        HashSet<string> visited = new HashSet<string>();
        Dictionary<string,string> prev = new Dictionary<string,string>();
        Queue<string> q = new Queue<string>();

        q.Enqueue(a);
        visited.Add(a);

        while(q.Count > 0){
            string s = q.Dequeue();
            if(s == b) break;
            List<string> neighbours = g.neighbours(s);
            
            foreach(string x in neighbours)
                if(!visited.Contains(x)){
                    prev.Add(x,s);
                    visited.Add(x);
                    q.Enqueue(x);
                }
        }
        for(string i = b; prev.ContainsKey(i); i = prev[i]) res.Add(i);

        if(res.Count > 0 || a == b) res.Add(a);
        res.Reverse();
        return res;
    }
}*/


class Program{
    public static void Main(){
        GraphList graph_l = new GraphList(10);
        GraphMatrix graph_m = new GraphMatrix(10);

        graph_m.append("a","b");
        graph_m.append("b","a");
        graph_m.append("c","a");
        graph_m.append("a","w");
        Console.WriteLine(graph_m.edges);
        Console.WriteLine(graph_m.print());
        
        Console.WriteLine("---------------\n");
        graph_l.append("a","b");
        graph_l.append("b","a");
        graph_l.append("c","a");
        graph_l.append("a","w");
        Console.WriteLine(graph_l.edges);
        Console.WriteLine(graph_l.print());

        // G.print();

        // Console.WriteLine(string.Join(", ", G.neighbours("babcia")));
        // Console.WriteLine(string.Join(", ", G.neighbours("jadzia")));
        // Console.WriteLine(string.Join(", ", GraphOperations.ShortestPath(G,"babcia","stasia")));
        // Console.WriteLine(string.Join(", ", GraphOperations.ShortestPath(G,"basia","babcia")));

        // G.RemoveEdge("jadzia","stasia");
        // Console.WriteLine(string.Join(", ", GraphOperations.ShortestPath(G,"babcia","stasia")));
        // G.RemoveEdge("jadzia","basia");
        // Console.WriteLine(string.Join(", ", GraphOperations.ShortestPath(G,"basia","babcia")));
        // Console.WriteLine(string.Join(", ", GraphOperations.ShortestPath(G,"basia","basia")));
        // Console.WriteLine(G.edges);

        // try{
        //     G.RemoveEdge("kasia","babcia");
        // } catch(Exception e){
        //     Console.WriteLine(e);
        // }
        // try{
        //     G.neighbours("dziadek");
        // } catch(Exception e){
        //     Console.WriteLine(e);
        // }

        // G = (GraphList)GraphOperations.RandomGraph(G,8,12);
        // // G = (GraphMatrix)GraphOperations.RandomGraph(G,8,12);
        // G.print();
    }
}