import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;

/*
Patryk Flama
Lista 6 zadanie 1
kompilacja: javac .\zad1.java; java zad1; rm *.class
 */

class List<T> implements Serializable{
    private static final long serialVersionUID = 1L;

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

    public T pop_back() throws Exception {            // right side       [O O ... O] <- O
        if(this.is_empty()) throw new Exception("Cant pop from empty list!");
        T temp = right.val;
        if(right != left){      // not last element
            right = right.prev;
            right.next = null;
        } else right = left = null;
        return temp;
    }

    public T pop_front() throws Exception {           // left side        O -> [O O ... O]
        if(this.is_empty()) throw new Exception("Cant pop from empty list!");
        T temp = left.val;
        if(right != left){      // not last element
            left = left.next;
            left.prev = null;
        } else right = left = null;
        return temp;
    }
    
    public boolean is_empty(){
        return left == null;
    }
};

class List_Element<T> implements Serializable{
    private static final long serialVersionUID = 1L;

    public List_Element<T> next;
    public List_Element<T> prev;
    public T val;

    public List_Element(T _val) {
        val = _val;
    }
}

public class zad1 {
    public static void main(String[] args) throws IOException, ClassNotFoundException, Exception {
        List<Integer> list = new List<Integer>();
        list.push_back(1);
        list.push_front(2);
        list.push_back(3);
        list.push_back(4);

        try{
            FileOutputStream fileOutput = new FileOutputStream("zad1-class");
            ObjectOutputStream objOutput = new ObjectOutputStream(fileOutput);
            objOutput.writeObject(list);
            objOutput.close();
            fileOutput.close();
            System.out.println("Succesfully saved data!");
        } catch (IOException e){
            System.out.println("Error while saving data!");
            // throw new IOException(e);
        }

        try{
            FileInputStream fileInput = new FileInputStream("zad1-class");
            ObjectInputStream objInput = new ObjectInputStream(fileInput);
            
            List<?> inputList = (List<?>) objInput.readObject();
            // List<Integer> inputList = (List<Integer>) objInput.readObject();

            objInput.close();
            fileInput.close();
            System.out.println("Succesfully read data!");

            while(!inputList.is_empty()){
                System.out.println(inputList.pop_back());
            }
        } catch (IOException e){
            System.out.println("Error while reading data!");
            // throw new IOException(e);
        } catch (ClassNotFoundException e){
            System.out.println("List of integers class not found!");
            // throw new ClassNotFoundException();
        }
    }
}
