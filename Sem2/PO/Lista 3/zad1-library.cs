/*
Patryk Flama - lista 3 zadanie 1
kompilacja: mcs -target:library -out:lista.dll zad1-library.cs
*/

using System;


public class List<T>{
    List_Element<T> left;
    List_Element<T> right;

    public List(){
        left = right = null;
    }

    public void push_front(T val){      // left side        O -> [O O ... O]
        List_Element<T> temp = new List_Element<T>(val);
        if(left != null){           // list not empty
            left.prev = temp;
            temp.next = left;
        } else right = temp;
        left = temp;
    }

    public void push_back(T val){       // right side       [O O ... O] <- O
        List_Element<T> temp = new List_Element<T>(val);
        if(right != null){      // list not empty
            right.next = temp;
            temp.prev = right;
        } else left = temp;
        right = temp;
    }

    public T pop_back(){            // right side       [O O ... O] <- O
        if(this.is_empty()) throw new Exception("Cant pop from empty list!");
        T temp = right.val;
        if(right != left){      // not last element
            right = right.prev;
            right.next = null;
        } else right = left = null;
        return temp;
    }

    public T pop_front(){           // left side        O -> [O O ... O]
        if(this.is_empty()) throw new Exception("Cant pop from empty list!");
        T temp = left.val;
        if(right != left){      // not last element
            left = left.next;
            left.prev = null;
        } else right = left = null;
        return temp;
    }
    
    public bool is_empty(){
        return left == null;
    }
};

class List_Element<T>{
    public List_Element<T> next;
    public List_Element<T> prev;
    public T val;

    public List_Element(T _val) {
        val = _val;
    }
}

