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

        graph[from].Add(to);
        graph[to].Add(from);
        _edges++;
    }

    public void remove(string from, string to){
        if(!graph.ContainsKey(from) || !graph.ContainsKey(to)) return;      // this vortex doesn't exist!

        graph[from].Remove(to);
        graph[to].Remove(from);
        _edges--;
    }

    public void print(){
        foreach(KeyValuePair<string, List<string>> i in graph){
            Console.Write(i.Key, ": ");
            Console.WriteLine(string.Join(", ", i.Value));
        }
    }
};

class GraphMatrix : IGraph{
    int _verticies;
    int _edges;
    bool[,] matrix;

    Dictionary<string, int> VertexId;
    Dictionary<int, string> VertexLabel;


    int GetId(string a){
        return VertexId.ContainsKey(a) ? VertexId[a] : VertexId.Count;
    }

    public GraphMatrix(int n){
        _verticies = n;
        _edges = 0;
        matrix = new bool[n, n];
        VertexId = new Dictionary<string, int>();
        VertexLabel = new Dictionary<int, string>();
    }

    public int SizeV{
        get {return _verticies;}
    }
    public int edges{
        get {return _edges;}
    }

    public void append(string from, string to){
        int id_a = GetId(from);
        if(id_a == VertexId.Count){
            VertexId.Add(from,id_a);
            VertexLabel.Add(id_a,from);
        }

        int id_b = GetId(b);
        if(id_b == VertexId.Count){
            VertexId.Add(b,id_b);
            VertexLabel.Add(id_b,b);
        }

        matrix[id_a,id_b] = matrix[id_b,id_a] = true;
        _edges++;
    }

    public void RemoveEdge(string from, string b){
        int id_a = GetId(from);
        int id_b = GetId(b);

        if(id_a == VertexId.Count || id_b == VertexId.Count) 
            throw new Exception("Proba usuniecia nieistniejacej krawedzi!");
        matrix[id_a,id_b] = matrix[id_b,id_a] = false;
        _edges--;
    }

    public List<string> GetNeighbors(string s){
        int x = GetId(s);
        if(x == VertexId.Count) 
            throw new Exception("Odwolanie do nieistniejacego wierzcholka!");

        List<string> l = new List<string>();
        for(int i = 0; i < size_v; i++)
            if(matrix[x,i]) l.Add(VertexLabel[i]);
        return l;
    }
    
    public void print(){
        for(int i = 0; i < size_v; i++)
            if(VertexLabel.ContainsKey(i)){
                Console.Write("{0}: ", VertexLabel[i]);
                Console.WriteLine(string.Join(", ", GetNeighbors(VertexLabel[i])));
            }
    }
};

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
            List<string> neighbors = g.GetNeighbors(s);
            
            foreach(string x in neighbors)
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
}


class Program{
    public static void Main(){
        GraphList G = new GraphList(10);
        //GraphMatrix G = new GraphMatrix(10);

        G.append("babcia","jadzia");
        G.append("jadzia","basia");
        G.append("basia","stasia");
        G.append("stasia","jadzia");
        Console.WriteLine(G.edges);

        G.print();

        Console.WriteLine(string.Join(", ", G.GetNeighbors("babcia")));
        Console.WriteLine(string.Join(", ", G.GetNeighbors("jadzia")));
        Console.WriteLine(string.Join(", ", GraphOperations.ShortestPath(G,"babcia","stasia")));
        Console.WriteLine(string.Join(", ", GraphOperations.ShortestPath(G,"basia","babcia")));

        G.RemoveEdge("jadzia","stasia");
        Console.WriteLine(string.Join(", ", GraphOperations.ShortestPath(G,"babcia","stasia")));
        G.RemoveEdge("jadzia","basia");
        Console.WriteLine(string.Join(", ", GraphOperations.ShortestPath(G,"basia","babcia")));
        Console.WriteLine(string.Join(", ", GraphOperations.ShortestPath(G,"basia","basia")));
        Console.WriteLine(G.edges);

        try{
            G.RemoveEdge("kasia","babcia");
        } catch(Exception e){
            Console.WriteLine(e);
        }
        try{
            G.GetNeighbors("dziadek");
        } catch(Exception e){
            Console.WriteLine(e);
        }

        G = (GraphList)GraphOperations.RandomGraph(G,8,12);
        //G = (GraphMatrix)GraphOperations.RandomGraph(G,8,12);
        G.print();
    }
}