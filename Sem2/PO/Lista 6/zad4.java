import java.util.ArrayList;
import java.util.Arrays;

/*
Patryk Flama
Lista 6 zadanie 4
kompilacja: javac .\zad4.java; java zad4; rm *.class
 */
class MergeSort<T extends Comparable<T>> extends Thread {
    T[] tab;

    public MergeSort(T[] _tab){
        tab = _tab;
    }

    private T[] main(MergeSort<T> l, MergeSort<T> r) throws InterruptedException {
        // start spearate threads to compute result
        l.start();
        r.start();
        // wait for the result
        l.join();
        r.join();

        ArrayList<T> res = new ArrayList<T>();
        int ptr_l = 0, ptr_r = 0;
        
        for(int i = 0; i < l.tab.length + r.tab.length; i++){
            if(ptr_r >= r.tab.length && ptr_l >= l.tab.length) return res.toArray(l.tab);
            else if(ptr_l >= l.tab.length) res.add(r.tab[ptr_r++]);
            else if(ptr_r >= r.tab.length) res.add(l.tab[ptr_l++]);
            else if(l.tab[ptr_l].compareTo(r.tab[ptr_r]) < 0) res.add(l.tab[ptr_l++]);
            else res.add(r.tab[ptr_r++]);
        }

        return res.toArray(l.tab);
    }

    public void run(){
        if(tab.length <= 1) return;

        int mid = tab.length / 2;
        try{
            tab = main(new MergeSort<T>(Arrays.copyOfRange(tab, 0, mid)),
                       new MergeSort<T>(Arrays.copyOfRange(tab, mid, tab.length)));
        } catch(InterruptedException e){
            System.out.println("ERROR! A thread has interrupted the current thread!");
        }
    }

    public static <T extends Comparable<T>> T[] execute(T[] _tab) throws InterruptedException {
        MergeSort<T> aux = new MergeSort<T>(_tab);
        // execute sort
        aux.start();
        aux.join();

        return aux.tab;
    }
}

public class zad4 {
    public static void main(String[] args) throws InterruptedException {
        Integer[] tab1 = {9,3,5,4,7,6,2,8,0,1};
        System.out.println(Arrays.toString(tab1));
        tab1 = MergeSort.execute(tab1);
        System.out.println(Arrays.toString(tab1));
        
        Double[] tab2 = {237496.846,39.576,2645.458,364.597,3312.684,765.4567,657.456,7467.488,15.648,789.};
        System.out.println(Arrays.toString(tab2));
        tab2 = MergeSort.execute(tab2);
        System.out.println(Arrays.toString(tab2));
        
        String[] tab3 = {"thats", "an", "interesting", "string", "table"};
        System.out.println(Arrays.toString(tab3));
        tab3 = MergeSort.execute(tab3);
        System.out.println(Arrays.toString(tab3));
    }
}
