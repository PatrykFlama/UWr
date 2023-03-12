/*
Patryk Flama - lista 3 zadanie 1
kompilacja: mcs // TODO
*/

using System;


class List<T>{
    int num_of_elements;
    List_Element<T> begin;
    List_Element<T> end;

    public List(){
        num_of_elements = 0;
        begin = end = new List_Element<T>();
    }

    public void push_front(T val){
        begin = begin.push_left(val);
        if(num_of_elements == 0) end = begin;
        num_of_elements++;
    }

    public void push_back(T val){
        end = end.push_right(val);
        if(num_of_elements == 0) begin = end;
        num_of_elements++;
    }

    public T pop_back(){
        num_of_elements--;
        T deleted = end.val;

        end = end.last;
        end.next = null;
        if(num_of_elements == 0){
            end.last = null;
            begin.next = null;
        }

        return deleted;
    }

    public T pop_front(){
        num_of_elements--;
        T deleted = begin.val;

        begin = begin.next;
        begin.last = null;
        if(num_of_elements == 0){
            begin.next = null;
            end.last = null;
        }
        
        return deleted;
    }
    
    public bool is_empty(){
        if(num_of_elements == 0) return true;
        return false;
    }
};

class List_Element<T>{
    public List_Element<T> next;
    public List_Element<T> last;
    public T val;

    public List_Element<T> push_right(T val){
        this.next = new List_Element<T>();
        this.next.val = val;
        this.next.last = this;
        return this.next;
    }

    public List_Element<T> push_left(T val){
        this.last = new List_Element<T>();
        this.last.val = val;
        this.last.next = this;
        return this.last;
    }
}


class Program{
    public static void Main(string[] args){
        List<int> list = new List<int>();
        list.push_back(1);
        list.push_back(2);
        list.push_back(3);
        Console.WriteLine(list.pop_front());
        Console.WriteLine(list.pop_front());
        Console.WriteLine(list.pop_front());

        // TODO debug
    }
}
