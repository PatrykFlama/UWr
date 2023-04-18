/*
Patryk Flama
Lista 5 zadanie 2 oraz 4
kompilacja: java zad lub javac zad.java; java zad
 */
import java.util.HashMap;

class Expression {
    /*
    Expression is tree of oprations (classes of operations; Add and Mult) 
    and contains variables, constatns at leafs - saved in pertinent classes
    evaluation of variables to values is saved on the expression hash map
     */
    HashMap<String, Float> variable_names;

    public Expression(){
        variable_names = new HashMap<String, Float>();
    }

    public float evaluate() {
        return 0;
    }

    public String toString(){
        return "";
    }

    public void updateVariable(String name, float val){
        variable_names.put(name, val);
    }

    public String niceEvaluate(){
        return this.toString() + " = " + String.valueOf(this.evaluate());
    }

    public Expression derivative(){
        return this;
    }
}

class Const extends Expression{
    float val;

    public Const(float _val){ 
        val = _val; 
    }

    public float evaluate(){
        return val;
    }

    public String toString(){
        return String.valueOf(val);
    }

    public Expression derivative(){
        return new Const(0);
    }
}

class Variable extends Expression{
    String name;

    public Variable(String _name){
        name = _name;
    }
    public Variable(String _name, float val){
        name = _name;
        updateVariable(name, val);
    }

    public float evaluate() {
        Float temp = variable_names.get(name);
        if(temp == null) {
            System.out.println("Variable " + name + " without value, assigning 0!");
            temp = 0f;
        }
        return temp;
    }

    public String toString(){
        return name;
    }

    public Expression derivative(){
        return new Const(1);
    }
}

class Add extends Expression{
    Expression L, R;

    public Add(Expression _L, Expression _R){
        L = _L;
        R = _R;
    }

    public float evaluate(){
        return L.evaluate() + R.evaluate();
    }

    public String toString(){
        return L.toString() + " + " + R.toString();
    }

    public void updateVariable(String name, float val){
        variable_names.put(name, val);
        L.updateVariable(name, val);
        R.updateVariable(name, val);
    }

    public Expression derivative(){
        return new Add(L.derivative(), R.derivative());
    }
}

class Mult extends Expression{
    Expression L, R;

    public Mult(Expression _L, Expression _R){
        L = _L;
        R = _R;
    }

    public float evaluate(){
        return L.evaluate() * R.evaluate();
    }

    public String toString(){
        String tempL = L.toString();
        String tempR = R.toString();

        if(L instanceof Add) tempL = "(" + tempL + ")";
        if(R instanceof Add) tempR = "(" + tempR + ")";

        return tempL + " * " + tempR;
    }

    public void updateVariable(String name, float val){
        variable_names.put(name, val);
        L.updateVariable(name, val);
        R.updateVariable(name, val);
    }

    public Expression derivative(){
        Mult m1 = new Mult(L.derivative(), R);
        Mult m2 = new Mult(L, R.derivative());
        return new Add(m1, m2);
    }
}

public class zad5 {
    public static void main(String[] args) {
        Expression e = new Add(new Const(4f), new Variable("x", 2f));
        System.out.println(e.niceEvaluate());
        System.out.println("f(x)' = " + e.derivative() + " = " + e.derivative().evaluate());
        
        e = new Add(new Const(4f), new Mult(new Const(3f), new Variable("x", 2f)));
        System.out.println(e.niceEvaluate());
        System.out.println("f(x)' = " + e.derivative() + " = " + e.derivative().evaluate());

        e = new Mult(e, new Variable("y"));
        System.out.println(e.niceEvaluate());
        
        e.updateVariable("y", 10f);
        System.out.println(e.niceEvaluate());
        
        e.updateVariable("y", 100f);
        System.out.println(e.niceEvaluate());
        
        e = new Add(new Variable("x"), new Mult(new Const(5f), new Add(new Const(11f), e)));
        e.updateVariable("x", 2f);
        System.out.println(e.niceEvaluate());

        e = (new Mult(new Variable("x"), new Variable("x"))).derivative();
        e.updateVariable("x", 3f);
        System.out.println("f(x)' = " + e.niceEvaluate());
    }
}
